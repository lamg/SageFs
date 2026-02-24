# Live Unit Testing Architecture

> Make VS Enterprise look foolish. Sub-500ms from keystroke to inline test feedback.
> All major .NET frameworks. Every editor. Free.

## Problem Statement

Visual Studio Enterprise charges ~$250/month for "Live Unit Testing" that:
- Requires a **full project build** before running tests (seconds to minutes)
- Only triggers on **file save**, not as-you-type
- Requires **IL instrumentation** for code coverage (heavy, slow)
- Only works with MSTest/xUnit/NUnit — no Expecto, no TUnit
- Locked to Windows + Visual Studio

SageFs already has 90% of the infrastructure to obliterate this:
- FSI sessions are already warm — no compilation step
- Harmony hot-patches methods in-place — patched functions are immediately tested
- File watcher already detects changes with 500ms debounce
- Hot reload already re-evaluates files via `#load`
- Elm architecture already pushes UI updates in real-time
- MCP server already broadcasts events to connected agents
- CellGrid rendering already supports rich colors per-cell

**Target: 200-500ms from keystroke to inline test feedback.** VS Enterprise takes 5-30 seconds.

---

## Core Architecture Values

1. **Pure domain model** — all test discovery, category detection, affected-test selection,
   result merging, and policy filtering are pure functions. 100% unit testable.
   IO only at provider execution boundaries and the Elm effect edge.

2. **Neovim-style extensibility** — composable APIs with solid extension points. Community
   authors add test framework support without understanding SageFs internals.

3. **Human-first API design** — a developer reads the provider types and immediately knows
   what to implement. No magic, no reflection-based registration, no abstract base classes.

4. **Two-tier provider model** — Tier 1 (attribute-based) covers 80% of frameworks in ~10 lines.
   Tier 2 (custom discovery) handles exotic patterns like Expecto's value-based tests.

5. **Providers return pure data, core manages lifecycle** — providers don't track staleness,
   invalidation, or rendering. They answer questions; the core manages everything else.

6. **No Option types — model the domain** — every `option` is a missed opportunity for a
   meaningful DU. `SourceFile: string option` becomes `TestOrigin = SourceMapped | ReflectionOnly`.
   `stackTrace: string option` becomes `TestFailure = AssertionFailed | ExceptionThrown | TimedOut`.

---

## Phase 1: Core Domain Types

All types validated in SageFs FSI — every example below compiles and runs.

### TestId — Stable Identity

```fsharp
open System
open System.Text

type TestId = TestId of string

module TestId =
  let create (fullName: string) (framework: string) =
    sprintf "%s::%s" framework fullName
    |> Encoding.UTF8.GetBytes
    |> Security.Cryptography.SHA256.HashData
    |> Convert.ToHexString
    |> fun h -> TestId (h.Substring(0, 16))

  let value (TestId id) = id
```

### Test Categories & Run Policies

```fsharp
[<RequireQualifiedAccess>]
type TestCategory =
  | Unit
  | Integration
  | Browser
  | Benchmark
  | Architecture
  | Property

[<RequireQualifiedAccess>]
type RunPolicy =
  | OnEveryChange
  | OnSaveOnly
  | OnDemand
  | Disabled

module RunPolicyDefaults =
  let defaults = Map.ofList [
    TestCategory.Unit,         RunPolicy.OnEveryChange
    TestCategory.Integration,  RunPolicy.OnDemand
    TestCategory.Browser,      RunPolicy.OnDemand
    TestCategory.Benchmark,    RunPolicy.OnDemand
    TestCategory.Architecture, RunPolicy.OnSaveOnly
    TestCategory.Property,     RunPolicy.OnEveryChange
  ]

[<RequireQualifiedAccess>]
type RunTrigger =
  | Keystroke
  | FileSave
  | ExplicitRun
```

### Assembly Info & Source Location

```fsharp
open System.Reflection

type AssemblyInfo = {
  Name: string
  Location: string
  ReferencedAssemblies: AssemblyName array
}

[<Struct>]
type SourceTestLocation = {
  AttributeName: string
  FilePath: string
  Line: int
  Column: int
}

[<RequireQualifiedAccess>]
type TestOrigin =
  | SourceMapped of file: string * line: int
  | ReflectionOnly
```

### Test Case, Failure, Result

```fsharp
type TestCase = {
  Id: TestId
  FullName: string
  DisplayName: string
  Origin: TestOrigin
  Labels: string list
  Framework: string
  Category: TestCategory
}

[<RequireQualifiedAccess>]
type TestFailure =
  | AssertionFailed of message: string
  | ExceptionThrown of message: string * stackTrace: string
  | TimedOut of after: TimeSpan

[<RequireQualifiedAccess>]
type TestResult =
  | Passed of duration: TimeSpan
  | Failed of failure: TestFailure * duration: TimeSpan
  | Skipped of reason: string
  | NotRun

type TestRunResult = {
  TestId: TestId
  TestName: string
  Result: TestResult
  Timestamp: DateTimeOffset
}

[<RequireQualifiedAccess>]
type RunHistory =
  | NeverRun
  | PreviousRun of duration: TimeSpan
```

### UI Contract Types — TestRunStatus & TestStatusEntry

Every UI target (TUI, VS Code, Visual Studio, Neovim, Dashboard) reads these.
The core pre-computes status — no target derives it independently.

```fsharp
[<RequireQualifiedAccess>]
type TestRunStatus =
  | Detected
  | Queued
  | Running
  | Passed of duration: TimeSpan
  | Failed of failure: TestFailure * duration: TimeSpan
  | Skipped of reason: string
  | Stale
  | PolicyDisabled

type TestStatusEntry = {
  TestId: TestId
  DisplayName: string
  FullName: string
  Origin: TestOrigin
  Framework: string
  Category: TestCategory
  CurrentPolicy: RunPolicy
  Status: TestRunStatus
  PreviousStatus: TestRunStatus
}
```

`PreviousStatus` enables clients to detect transitions ("went from Passed to Failed")
without maintaining their own history. Notification urgency is a UI concern, not domain.

### Provider Descriptions (Pure Data — Stored in Elm Model)

```fsharp
type AttributeProviderDescription = {
  name: string
  testAttributes: string list
  assemblyMarker: string
}

type CustomProviderDescription = {
  name: string
  assemblyMarker: string
}

[<RequireQualifiedAccess>]
type ProviderDescription =
  | AttributeBased of AttributeProviderDescription
  | Custom of CustomProviderDescription
```

### Provider Executors (Impure — Functions, NOT in Elm Model)

```fsharp
type AttributeTestExecutor = {
  description: AttributeProviderDescription
  execute: MethodInfo -> Async<TestResult>
}

type CustomTestExecutor = {
  description: CustomProviderDescription
  detect: AssemblyInfo -> bool
  discover: Assembly -> TestCase list
  execute: TestCase -> Async<TestResult>
}

[<RequireQualifiedAccess>]
type TestExecutor =
  | AttributeBased of AttributeTestExecutor
  | Custom of CustomTestExecutor
```

### Built-In Provider Descriptions

```fsharp
module TestProviderDescriptions =
  let xunit: ProviderDescription =
    ProviderDescription.AttributeBased {
      name = "xunit"
      testAttributes = ["FactAttribute"; "TheoryAttribute"]
      assemblyMarker = "xunit.core"
    }

  let nunit: ProviderDescription =
    ProviderDescription.AttributeBased {
      name = "nunit"
      testAttributes = ["TestAttribute"; "TestCaseAttribute"; "TestCaseSourceAttribute"]
      assemblyMarker = "nunit.framework"
    }

  let mstest: ProviderDescription =
    ProviderDescription.AttributeBased {
      name = "mstest"
      testAttributes = ["TestMethodAttribute"; "DataTestMethodAttribute"]
      assemblyMarker = "Microsoft.VisualStudio.TestPlatform.TestFramework"
    }

  let tunit: ProviderDescription =
    ProviderDescription.AttributeBased {
      name = "tunit"
      testAttributes = ["TestAttribute"]
      assemblyMarker = "TUnit.Core"
    }

  let expecto: ProviderDescription =
    ProviderDescription.Custom {
      name = "expecto"
      assemblyMarker = "Expecto"
    }

  let builtIn = [ xunit; nunit; mstest; tunit; expecto ]

  let detectProviders (assemblies: AssemblyInfo list) : ProviderDescription list =
    builtIn |> List.filter (fun desc ->
      let marker =
        match desc with
        | ProviderDescription.AttributeBased ap -> ap.assemblyMarker
        | ProviderDescription.Custom cp -> cp.assemblyMarker
      assemblies
      |> List.exists (fun a ->
        a.ReferencedAssemblies
        |> Array.exists (fun r -> r.Name = marker)))
```

### Dependency Graph

Inverted index: given a changed symbol, look up which tests use it. O(k) lookup.

```fsharp
type TestDependencyGraph = {
  SymbolToTests: Map<string, TestId array>
  TransitiveCoverage: Map<string, TestId array>
  SourceVersion: int
}

module TestDependencyGraph =
  let empty = { SymbolToTests = Map.empty; TransitiveCoverage = Map.empty; SourceVersion = 0 }

  let findAffected (changedSymbols: string list) (graph: TestDependencyGraph) : TestId array =
    changedSymbols
    |> List.choose (fun sym -> Map.tryFind sym graph.SymbolToTests)
    |> Array.concat
    |> Array.distinct
```

### Aggregate State (Elm Model)

```fsharp
type LiveTestState = {
  SourceLocations: SourceTestLocation array
  DiscoveredTests: TestCase array
  LastResults: Map<TestId, TestRunResult>
  StatusEntries: TestStatusEntry array
  CoverageAnnotations: CoverageAnnotation array
  IsRunning: bool
  History: RunHistory
  AffectedTests: Set<TestId>
  Enabled: bool
  ShowCoverage: bool
  RunPolicies: Map<TestCategory, RunPolicy>
  DetectedProviders: ProviderDescription list
}

module LiveTestState =
  let empty = {
    SourceLocations = Array.empty
    DiscoveredTests = Array.empty
    LastResults = Map.empty
    StatusEntries = Array.empty
    CoverageAnnotations = Array.empty
    IsRunning = false
    History = RunHistory.NeverRun
    AffectedTests = Set.empty
    Enabled = true
    ShowCoverage = true
    RunPolicies = RunPolicyDefaults.defaults
    DetectedProviders = []
  }
```

### Concurrency Model

```fsharp
module ExecutionPolicy =
  let unitParallelism = System.Environment.ProcessorCount
  let integrationParallelism = 1
  let defaultTimeout = TimeSpan.FromSeconds 5.0
  let browserTimeout = TimeSpan.FromSeconds 30.0
```

### Events

```fsharp
type LiveTestEvent =
  | TestLocationsDetected of locations: SourceTestLocation array
  | ProvidersDetected of providers: ProviderDescription list
  | TestsDiscovered of tests: TestCase array
  | TestDependencyGraphUpdated of graph: TestDependencyGraph
  | CoverageUpdated of annotations: CoverageAnnotation array
  | TestRunStarted of testIds: TestId array
  | TestRunCompleted of results: TestRunResult array * duration: TimeSpan
  | TestRunFailed of error: string
  | LiveTestingToggled of enabled: bool
  | CoverageToggled of show: bool
  | TestRunPolicyChanged of category: TestCategory * policy: RunPolicy
```

---

## Phase 1: Pure Functions

### filterByPolicy

Only return tests whose category policy matches the trigger.

```fsharp
module LiveTesting =
  let filterByPolicy
    (policies: Map<TestCategory, RunPolicy>)
    (trigger: RunTrigger)
    (tests: TestCase array)
    : TestCase array =
    tests
    |> Array.filter (fun test ->
      let policy =
        policies
        |> Map.tryFind test.Category
        |> Option.defaultValue RunPolicy.OnEveryChange
      match trigger, policy with
      | _, RunPolicy.Disabled -> false
      | RunTrigger.Keystroke, RunPolicy.OnEveryChange -> true
      | RunTrigger.Keystroke, _ -> false
      | RunTrigger.FileSave, RunPolicy.OnEveryChange -> true
      | RunTrigger.FileSave, RunPolicy.OnSaveOnly -> true
      | RunTrigger.FileSave, _ -> false
      | RunTrigger.ExplicitRun, _ -> true)
```

### mergeResults

Pure state update after a test run completes.

```fsharp
  let mergeResults (state: LiveTestState) (results: TestRunResult array) : LiveTestState =
    let updatedResults =
      results
      |> Array.fold (fun acc r -> Map.add r.TestId r acc) state.LastResults
    let totalDuration =
      results
      |> Array.choose (fun r ->
        match r.Result with
        | TestResult.Passed d -> Some d
        | TestResult.Failed (_, d) -> Some d
        | _ -> None)
      |> Array.fold (fun (acc: TimeSpan) d -> acc + d) TimeSpan.Zero
    { state with
        LastResults = updatedResults
        History = RunHistory.PreviousRun totalDuration
        IsRunning = false }
```

### computeStatusEntries

Derive UI-consumable status from current state. Called after every state change.
All UI targets read this — no target computes status independently.

```fsharp
  let computeStatusEntries (state: LiveTestState) : TestStatusEntry array =
    state.DiscoveredTests
    |> Array.map (fun test ->
      let policy =
        state.RunPolicies
        |> Map.tryFind test.Category
        |> Option.defaultValue RunPolicy.OnEveryChange
      let status =
        if policy = RunPolicy.Disabled then TestRunStatus.PolicyDisabled
        elif state.AffectedTests.Contains test.Id then TestRunStatus.Queued
        else
          match Map.tryFind test.Id state.LastResults with
          | Some r ->
            match r.Result with
            | TestResult.Passed d -> TestRunStatus.Passed d
            | TestResult.Failed (f, d) -> TestRunStatus.Failed (f, d)
            | TestResult.Skipped reason -> TestRunStatus.Skipped reason
            | TestResult.NotRun -> TestRunStatus.Detected
          | None -> TestRunStatus.Detected
      let previousStatus =
        match Map.tryFind test.Id state.LastResults with
        | Some r ->
          match r.Result with
          | TestResult.Passed d -> TestRunStatus.Passed d
          | TestResult.Failed (f, d) -> TestRunStatus.Failed (f, d)
          | TestResult.Skipped reason -> TestRunStatus.Skipped reason
          | TestResult.NotRun -> TestRunStatus.Detected
        | None -> TestRunStatus.Detected
      {
        TestId = test.Id
        DisplayName = test.DisplayName
        FullName = test.FullName
        Origin = test.Origin
        Framework = test.Framework
        Category = test.Category
        CurrentPolicy = policy
        Status = status
        PreviousStatus = previousStatus
      })
```

---

## Three-Layer Detection Architecture

```
LAYER 1: Tree-sitter (~1ms, works on broken code)
  Keystroke → tree-sitter parse
    → .scm query matches [<Fact>], [<Test>], [<TestMethod>], etc.
    → Immediate gutter markers: "tests exist here"
    → Attribute name + source location — no namespace resolution

LAYER 2: FCS Typed AST (~50-200ms, after type-check)
  Type-check succeeds → FSharpCheckFileResults
    → Full namespace resolution: NUnit.Framework.TestAttribute vs TUnit.Core.TestAttribute
    → Symbol usage graph → affected-test selection
    → Inverted dependency index

LAYER 3: Assembly reflection (after hot reload)
  HotReload completes → Harmony patches applied
    → Reflect on loaded assemblies → runnable test methods
    → Execute via framework-specific dispatch
    → Store results, push into Elm loop
```

### Tree-sitter Queries

Extend existing `highlights.scm`:

```scheme
;; Test attribute detection — framework-agnostic source-level markers
(attribute (_type) @test.attribute
  (#any-of? @test.attribute
    "Fact" "Theory"                          ;; xUnit
    "Test" "TestCase" "TestCaseSource"       ;; NUnit / TUnit
    "TestMethod" "DataTestMethod"            ;; MSTest
    "Tests"                                  ;; Expecto
    "Benchmark"                              ;; Benchmark.NET
    "Property"))                             ;; FsCheck
```

### Test Framework Support

| Framework | Discovery | Execution | Tier |
|-----------|-----------|-----------|------|
| **xUnit** | `[<Fact>]`, `[<Theory>]` | Reflection invoke | Tier 1 |
| **NUnit** | `[<Test>]`, `[<TestCase>]` | NUnit internal runner | Tier 1 |
| **MSTest** | `[<TestMethod>]`, `[<DataTestMethod>]` | Reflection invoke | Tier 1 |
| **TUnit** | `[<Test>]` (TUnit-specific) | TUnit engine | Tier 1 |
| **Expecto** | `[<Tests>]` on static properties | `Expecto.Tests.runTestsWithCLIArgs` | Tier 2 |

### Test Categorization

Tests are categorized by convention + configuration:
- `[<Category("Integration")>]`, `[<Trait("Category","Integration")>]`, or namespace containing
  "Integration" → `TestCategory.Integration`
- Assembly referencing `Microsoft.Playwright` → `TestCategory.Browser`
- `[<Benchmark>]` → `TestCategory.Benchmark`
- `[<Property>]` (FsCheck standalone) → `TestCategory.Property`
- Everything else → `TestCategory.Unit`
- User overrides via `.sagefs/test-config.json` or MCP tool

---

## Phase 2: Inline Gutter Rendering

### Gutter Design

2-character gutter column left of text content. Serves TWO purposes:

**In test code** — test execution status:
```
✓ let ``should add`` () =           ← test passed
✗ let ``should validate`` () =      ← test failed
● let ``should handle edge`` () =   ← detected, not run yet
◆ let ``should sort`` () =          ← tree-sitter detected, stale/pending
```

**In production code** — test reachability coverage (function definition lines only):
```
▸ let add x y = x + y              ← covered, all tests passing (green)
    x + y                            (no glyph on body lines)
▸ let validate x =                  ← covered, tests failing (red)
    if x > 0 then Ok x
○ let unused () = ()                ← not covered by any test (dim gray)
· let newFunc () = ()               ← pending FCS analysis (faint)
  let helper = 42                    (no glyph — not a function definition)
```

Test code legend:
- `✓` (green) — all tests covering this line pass
- `✗` (red) — at least one test fails
- `●` (blue/dim) — tests exist but haven't run yet
- `◆` (yellow/dim) — tree-sitter detected test attribute, results stale/pending

Production code legend (function definition lines only — avoids visual noise):
- `▸` (green) — reachable from tests, all passing
- `▸` (red) — reachable from tests, at least one failing
- `▹` (yellow) — reachable from tests, results stale
- `○` (dim gray) — not reachable from any test
- `·` (faint) — FCS dependency graph not yet built

Coverage appears at FCS speed (~50-200ms after type-check), before tests execute.
Coverage display toggleable via `Ctrl+Alt+C` or MCP tool (default: on).

**Important:** This is function-level test reachability from the FCS typed AST symbol graph,
NOT IL-level branch/statement coverage. Dynamic dispatch and reflection won't trace through.
Labeled "reachable from tests" in UI, not "code coverage."

### Coverage Types

```fsharp
[<RequireQualifiedAccess>]
type CoverageStatus =
  | Covered of testCount: int * allPassing: bool
  | NotCovered
  | Pending

type CoverageAnnotation = {
  Symbol: string
  FilePath: string
  DefinitionLine: int
  Status: CoverageStatus
}

module CoverageProjection =
  let symbolCoverage
    (graph: TestDependencyGraph)
    (results: Map<TestId, TestRunResult>)
    (symbol: string)
    : CoverageStatus =
    match Map.tryFind symbol graph.TransitiveCoverage with
    | None -> CoverageStatus.NotCovered
    | Some testIds when Array.isEmpty testIds -> CoverageStatus.NotCovered
    | Some testIds ->
      let allPassing =
        testIds |> Array.forall (fun tid ->
          match Map.tryFind tid results with
          | Some r ->
            match r.Result with
            | TestResult.Passed _ -> true
            | _ -> false
          | None -> false)
      CoverageStatus.Covered(testIds.Length, allPassing)
```

### LineAnnotation Type

```fsharp
type LineAnnotation = {
  Line: int
  Glyph: char
  Color: uint32
}
```

---

## Phase 3: MCP + SSE Integration (UI Contract)

### Two-Tier Push Architecture

**Tier 1: Summary** — always pushed, every state change:
```fsharp
| TestSummaryChanged of total: int * passed: int * failed: int * stale: int * running: int
```

**Tier 2: Detail** — pushed after run completes:
```fsharp
| TestResultsBatch of results: TestStatusEntry array
```

### MCP Tool: `get_live_test_status`

Supports optional `file` filter for per-file queries:

```json
{
  "enabled": true,
  "summary": { "total": 47, "passed": 45, "failed": 2, "stale": 0, "running": 0 },
  "pipelineTiming": { "treeSitterMs": 0.8, "fcsMs": 142, "executionMs": 87, "totalMs": 280 },
  "tests": [
    {
      "testId": "abc123",
      "displayName": "should validate input",
      "fullName": "MyModule.Tests.should validate input",
      "file": "MyModule.fs",
      "line": 42,
      "framework": "xunit",
      "category": "Unit",
      "policy": "OnEveryChange",
      "status": "Failed",
      "previousStatus": "Passed",
      "failure": { "kind": "AssertionFailed", "message": "Expected Ok but got Error" },
      "durationMs": 12
    }
  ]
}
```

### Four Endpoints Serve All UI Targets

1. **SSE: `TestSummaryChanged`** — push, every change → status bars
2. **SSE: `TestResultsBatch`** — push, after run → gutters, decorations, signs
3. **MCP: `get_live_test_status`** — pull with optional `file` filter → explorers
4. **MCP: `get_pipeline_trace`** — pull → timing displays

---

## Phase 4: As-You-Type Evaluation

### Three-Speed Feedback

```
NEAR-IMMEDIATE (50ms debounce): Tree-sitter re-parse
  → ~1ms parse → gutter markers (◆)
  → Works on broken code

FAST (300ms debounce): FCS type-check
  → 50-200ms → namespace disambiguation + dependency graph
  → Upgrade ◆ to precise framework markers

EXECUTION (after type-check + hot reload): Test run
  → Harmony patches → affected tests (O(k) lookup) → run
  → Respect RunPolicy per TestCategory
  → Upgrade ◆ to ✓/✗
```

### Debounced Effects

```fsharp
| EditorEffect.RequestDebouncedTreeSitter of code: string * delayMs: int
| EditorEffect.RequestDebouncedTypeCheck of code: string * delayMs: int
```

Each effect handler: cancel previous `CancellationTokenSource`, create new one,
`Task.Delay(debounce, token)`, then execute if not cancelled.

### Latency Budget

- Tree-sitter gutter: **~51ms** (50ms debounce + ~1ms parse)
- FCS enrichment: **350-500ms** (300ms debounce + 50-200ms type-check)
- Test execution: **370-700ms** (full pipeline)
- VS Enterprise: **5-30 seconds**

---

## Observability: OTEL + Status Bar

### Pipeline Depth & Timing

```fsharp
[<RequireQualifiedAccess>]
type PipelineDepth =
  | TreeSitterOnly of treeSitter: TimeSpan
  | ThroughFcs of treeSitter: TimeSpan * fcs: TimeSpan
  | ThroughExecution of treeSitter: TimeSpan * fcs: TimeSpan * execution: TimeSpan

type PipelineTiming = {
  Depth: PipelineDepth
  TotalTests: int
  AffectedTests: int
  Trigger: RunTrigger
  Timestamp: DateTimeOffset
}

module PipelineTiming =
  let treeSitterMs (t: PipelineTiming) =
    match t.Depth with
    | PipelineDepth.TreeSitterOnly ts
    | PipelineDepth.ThroughFcs (ts, _)
    | PipelineDepth.ThroughExecution (ts, _, _) -> ts.TotalMilliseconds
```

### OTEL Declarations

Zero new NuGet dependencies — `ActivitySource` is in `System.Diagnostics`,
`Meter` is in `System.Diagnostics.Metrics` (both BCL).

```fsharp
open System.Diagnostics
open System.Diagnostics.Metrics

let activitySource = new ActivitySource("SageFs.LiveTesting")
let meter = new Meter("SageFs.LiveTesting")
let treeSitterHistogram = meter.CreateHistogram<float>("sagefs.live_testing.treesitter_ms")
let fcsHistogram = meter.CreateHistogram<float>("sagefs.live_testing.fcs_ms")
let executionHistogram = meter.CreateHistogram<float>("sagefs.live_testing.execution_ms")
let testCountGauge = meter.CreateUpDownCounter<int>("sagefs.live_testing.test_count")
```

### Traced Wrapper

**Note:** Cannot use `inline` — F# restricts `inline` on let-bound functions using
structural comparison. Use a regular function instead.

```fsharp
let traced (name: string) (tags: (string * obj) list) (f: unit -> 'a) : 'a =
  use activity = activitySource.StartActivity(name)
  let sw = Stopwatch.StartNew()
  let result = f ()
  sw.Stop()
  if activity <> null then
    for (k, v) in tags do activity.SetTag(k, v) |> ignore
    activity.SetTag("duration_ms", sw.Elapsed.TotalMilliseconds) |> ignore
  result
```

When Aspire is running: full span waterfall with per-stage timing. When no collector:
`StartActivity()` returns null, cost is ~50ns.

### Status Bar Display (Always On)

```
TS:0.8ms | FCS:142ms | Run:87ms (12 tests) | Total:280ms
```

### OTEL Configuration (Add to McpServer.fs)

```fsharp
builder.Services.AddOpenTelemetry()
    .ConfigureResource(fun resource -> ... )  // existing
    .WithLogging(fun logging -> ... )          // existing
    .WithTracing(fun tracing ->                // NEW
        tracing
            .AddSource("SageFs.LiveTesting")
            .AddOtlpExporter() |> ignore)
    .WithMetrics(fun metrics ->                // NEW
        metrics
            .AddMeter("SageFs.LiveTesting")
            .AddOtlpExporter() |> ignore)
|> ignore
```

### Span Hierarchy

```
SageFs.LiveTesting.Pipeline (root span)
  ├── SageFs.LiveTesting.TreeSitterParse   { file, test_count, trigger }
  ├── SageFs.LiveTesting.FcsTypeCheck      { file, error_count, dep_graph_size }
  ├── SageFs.LiveTesting.TestDiscovery     { framework, discovered_count }
  └── SageFs.LiveTesting.TestExecution     { framework, total, passed, failed, skipped }
```

---

## UI Contract: One Data Model, Five Native Experiences

### Principle

Every UI target connects to the SageFs daemon on port 37749 via HTTP/SSE. The core
exposes pure data. Each target maps it to native affordances. The core NEVER references
signs, decorations, CodeLens, or Error List — it emits `TestStatusEntry`.

### Per-Target Affordance Mapping

| Feature | TUI/Raylib GUI | VS Code | Visual Studio | Neovim (sagefs.nvim) | Dashboard |
|---------|----------------|---------|---------------|---------------------|-----------|
| **Gutter** | CellGrid column (◆✓✗●) | `DecorationProvider` | Margin glyphs | `sign_place()` | CSS classes |
| **Status** | Status bar region | `StatusBarItem` | Status bar | Statusline component | Datastar fragment |
| **Failures** | Diagnostics pane | `DiagnosticCollection` + CodeLens | Error List | `vim.diagnostic.set()` | HTML table |
| **Explorer** | Pane with list | `TestController` API | Test Explorer window | Telescope picker | HTML filter/sort |
| **Controls** | `Ctrl+Alt+T` toggle | Command palette + CodeLens | Context menu | `:SageFsToggleLiveTesting` | Buttons |
| **Policy** | Status bar tooltip | TreeView sidebar | Tool Window tab | `:SageFsPolicy` picker | Settings panel |
| **Staleness** | Yellow ● gutter | Faded decoration | Stale margin icon | `DiagnosticSeverity.HINT` | Amber background |
| **Timing** | `TS:0.8ms \| FCS:142ms` | Hover tooltip | Tool Window stats | Virtual text/statusline | Real-time panel |

### Target-Specific Notes

**TUI/Raylib GUI** — reference implementation. Full control. Proves the data model works.

**VS Code** — use `TestController` API for native Test Explorer sidebar. Extend existing
CodeLens and `DiagnosticCollection`. SSE listener already in `DiagnosticsListener.fs`.

**Visual Studio** — route to Test Explorer if feasible. Error List for failures. Tool Window
"Live Tests" tab. Gentle UX: info bars, not aggressive popups.

**Neovim** — `vim.diagnostic.set()` with custom namespace. Signs: `SageFsTestPass` (✓),
`SageFsTestFail` (✗), `SageFsTestStale` (●), `SageFsTestDetected` (◆). Telescope picker:
`:Telescope sagefs tests`. Virtual text: `-- ✓ 3ms`. NEVER interrupt flow with popups.

**Dashboard** — passive monitoring. Full test table with sort/filter. Datastar morphs on
SSE push. Pipeline timing as real-time bar chart.

---

## File Plan

### New Files
1. `SageFs.Core/Features/LiveTestingTypes.fs` — All domain types
2. `SageFs.Core/Features/LiveTesting.fs` — Pure orchestration functions
3. `SageFs.Core/Features/LiveTestingInstrumentation.fs` — OTEL: ActivitySource, Meter, traced
4. `SageFs.Core/Features/TestProviders/ExpectoProvider.fs` — Tier 2
5. `SageFs.Core/Features/TestProviders/XunitProvider.fs` — Tier 1
6. `SageFs.Core/Features/TestProviders/NunitProvider.fs` — Tier 1
7. `SageFs.Core/Features/TestProviders/MsTestProvider.fs` — Tier 1
8. `SageFs.Core/Features/TestProviders/TunitProvider.fs` — Tier 1
9. `SageFs.Core/Features/TestTreeSitter.fs` — Tree-sitter queries
10. `SageFs.Core/Features/TestFcsEnrichment.fs` — FCS enrichment + dep graph

### Modified Files
1. `SageFs.Core/SageFsEvent.fs` — Add test events
2. `SageFs.Core/SageFsApp.fs` — Add LiveTestState to model
3. `SageFs.Core/RenderPipeline.fs` — Add LineAnnotation, ToggleLiveTesting action
4. `SageFs.Core/Middleware/HotReloading.fs` — Hook test discovery after hot reload
5. `SageFs.Core/Screen.fs` — Render gutter column
6. `SageFs.Core/highlights.scm` — Tree-sitter test attribute queries
7. `SageFs.Core/SyntaxHighlight.fs` — Emit test location data
8. `SageFs/McpTools.fs` — New MCP tools
9. `SageFs/McpServer.fs` — Push events + OTEL tracing/metrics

### Test Files
1. `SageFs.Tests/LiveTestingTests.fs` — Discovery, selection, result mapping, status entries
2. `SageFs.Tests/TestTreeSitterTests.fs` — Detection for all frameworks
3. `SageFs.Tests/TestCategoryTests.fs` — Category detection, policy enforcement

---

## Runtime & Language Feature Adoption

### Target Framework: net10.0 (upgrade to net11.0 when stable if measurements justify)

Targeting .NET 11 preview destroys optionality. Nothing in the
architecture *requires* .NET 11. FCS improvements arrive via SDK updates, not runtime changes.

### F# 10 Features to Adopt

| Feature | Where | Impact |
|---------|-------|--------|
| Parallel compilation | `<ParallelCompilation>true</ParallelCompilation>` | Could drop FCS 50-200ms → 20-50ms. **Benchmark first.** |
| ValueOption params | Provider config, optional filters | Zero-alloc optional parameters |
| `and!` in tasks | Concurrent test execution | `task { let! a = ... and! b = ... }` |
| Scoped nowarn | Reflection-heavy discovery code | Targeted warning suppression |
| Tailcalls in CEs | Recursive test discovery | Stack-safe nested suite traversal |

### F# Compiler Service 10.0.300 (Latest)

| Feature | Impact |
|---------|--------|
| Overload resolution caching | Reduces FCS type-check latency. Compounds with parallel compilation. |
| Find All References fixes (8+ bugs) | More accurate dependency graph = better "affected test" detection |
| Graph-based type checking fixes | Correct dependency resolution for same-module-name across files |
| `FSharpType.ImportILType` | Could bridge IL→FCS types for coverage-to-source mapping |

### F# 11 (Nov 2026 — one feature so far)

- **Simplified DIM interface hierarchies** (RFC FS-1336): Minor impact.
- **`MethodOverloadsCache`** (preview): May further reduce FCS latency. Track in benchmarks.

### .NET 11 Preview 1 (Nov 2026)

| Feature | Impact | Decision |
|---------|--------|----------|
| x86-64-v2 baseline (SSE4.2 guaranteed) | Removes SIMD feature detection for coverage bitmaps | Ship with fallback on net10.0, simplify on net11.0 |
| `BitArray.PopCount()` | Hardware bit counting | Use raw `ulong[]` instead — better SIMD access |
| `StringSyntaxAttribute.FSharp` | F# syntax highlighting in string literals | Nice-to-have |
| C# Unions (in progress) | C# sum types for provider authors | Long-term only, no short-term impact |

### IL Branch Coverage Data Model

```fsharp
/// Per-line coverage from IL instrumentation — authoritative result
[<RequireQualifiedAccess>]
type LineCoverage =
  | FullyCovered
  | PartiallyCovered of covered: int * total: int
  | NotCovered

/// IL sequence/branch point mapped to source
type SequencePoint = {
  File: string
  Line: int
  Column: int
  BranchId: int  // 0 for sequence points, >0 for branches
}

/// Coverage state per assembly — collected after test execution
type CoverageState = {
  Slots: SequencePoint array
  Hits: bool array  // indexed by slot ID
}
```

**Two-layer feedback:**
- FAST PREVIEW: FCS reachability (~1-5ms BFS) → "might be covered"
- AUTHORITATIVE: IL coverage after test execution → actual branch hits

### Microsoft.Testing.Platform

Hostable in any .NET app. Could serve as a provider backend. Recommendation: thin
abstraction (~50 lines) to preserve optionality. Don't commit until verifying FSI hosting
is viable — MTP's compile-time registration may conflict with hot-reload.

---

## Comparison: SageFs vs VS Enterprise

| Feature | VS Enterprise ($250/mo) | SageFs (free) |
|---------|------------------------|---------------|
| Feedback latency | 5-30 seconds | **~51ms** (gutter), **370-700ms** (results) |
| Triggers on | File save only | Every keystroke (debounced) |
| Compilation | Full project rebuild | FSI hot-reload (~100ms) |
| Stale work | Completes even if you keep typing | **Cancels** previous pending work |
| Test frameworks | MSTest/xUnit/NUnit only | ALL: xUnit, NUnit, MSTest, TUnit, Expecto |
| Test categories | None — runs everything | Unit/Integration/Browser/Benchmark per-category |
| Affected test detection | IL instrumentation (heavy) | FCS inverted dependency index O(k) |
| Code coverage | IL instrumentation (heavy, slow) | **Full IL branch** (30-80ms) + FCS reachability (~1ms) |
| Source-level detection | After build only | Tree-sitter (~1ms, works on broken code) |
| Platform | Windows + VS only | Terminal, GUI, any OS |
| IDE lock-in | Visual Studio only | 6 targets from same API |
| Pipeline visibility | Black box | Always-on: `TS:0.8ms \| FCS:142ms \| Run:87ms` |
| Status lifecycle | Binary pass/fail | 8-state DU with transition tracking |

---

## Upstream Contribution: ionide/tree-sitter-fsharp

### Opportunity

The `tags.scm` file in [ionide/tree-sitter-fsharp](https://github.com/ionide/tree-sitter-fsharp) is
**empty**. No test attribute detection exists upstream. Every tree-sitter consumer (Neovim, Helix,
Zed, VS Code with tree-sitter) would benefit from test attribute queries.

### Grammar Structure (proven)

```
attribute
  target: (identifier)?         ;; e.g., "assembly:" — rare for test attrs
  _object_construction
    _type
      long_identifier
        (identifier)+           ;; e.g., ["NUnit"; "Framework"; "Test"]
    expression?                 ;; e.g., constructor args
```

The existing `[<Literal>]` detection in `highlights.scm` proves this pattern works:
```scheme
((value_declaration
   (attributes
     (attribute
       (_type
         (long_identifier
           (identifier) @attribute))))
   (function_or_value_defn
     (value_declaration_left
       .
       (_) @constant)))
 (#eq? @attribute "Literal"))
```

### Proposed `tags.scm`

Two captures per match: `@test.framework` on the attribute name, `@definition.test` on the
decorated function or value.

**Critical design decision:** The `#any-of?` list includes BOTH short names and `Attribute`-suffixed
variants. In .NET, `[<Fact>]` and `[<FactAttribute>]` are equivalent — the compiler strips the
suffix. But tree-sitter sees raw text, so `[<FactAttribute>]` captures the identifier `"FactAttribute"`,
which won't match `"Fact"` in the predicate. Both forms must be listed explicitly.

`#any-of?` with doubled list chosen over `#match?` regex — more maintainable for community
contributors (add one line per new attribute, no regex editing).

```scheme
;; Test function/method detection — all major .NET test frameworks
;; Captures the attribute name AND the decorated function for test runner tooling.
;;
;; Both short names and Attribute-suffixed forms are listed because tree-sitter
;; sees raw text — [<FactAttribute>] captures "FactAttribute", not "Fact".
;; The .NET compiler strips the suffix, but tree-sitter doesn't.

;; Attribute-based test functions (let bindings with function signatures)
((value_declaration
   (attributes
     (attribute
       (_type
         (long_identifier
           (identifier) @test.framework))))
   (function_or_value_defn
     (function_declaration_left
       .
       (_) @definition.test)))
 (#any-of? @test.framework
   ;; xUnit
   "Fact" "FactAttribute"
   "Theory" "TheoryAttribute"
   ;; NUnit / TUnit (both use [<Test>] — disambiguation is Layer 2's job)
   "Test" "TestAttribute"
   "TestCase" "TestCaseAttribute"
   "TestCaseSource" "TestCaseSourceAttribute"
   ;; MSTest
   "TestMethod" "TestMethodAttribute"
   "DataTestMethod" "DataTestMethodAttribute"
   ;; Expecto
   "Tests" "TestsAttribute"
   ;; Benchmark.NET
   "Benchmark" "BenchmarkAttribute"
   ;; FsCheck
   "Property" "PropertyAttribute"))

;; Value-based test bindings (Expecto [<Tests>] on let bindings — not functions)
((value_declaration
   (attributes
     (attribute
       (_type
         (long_identifier
           (identifier) @test.framework))))
   (function_or_value_defn
     (value_declaration_left
       .
       (_) @definition.test)))
 (#any-of? @test.framework "Tests" "TestsAttribute"))
```

### Test Corpus: 75+ Cases Across 9 Files

Parse trees in corpus entries are **NEVER hand-written**. The workflow is:
1. Write the F# source for each test case
2. Run `npx tree-sitter parse test_file.fs` to get the ACTUAL parse tree
3. Copy that exact tree into the corpus entry
4. Run `npx tree-sitter test` to verify round-trip

All parse trees below are shown as `(... generated by tree-sitter parse ...)` — they MUST be
generated from the parser at contribution time, not invented.

#### File: `test/corpus/test_attributes_xunit.txt` (7 tests)

```
================================================================================
xUnit Fact attribute on let binding
================================================================================

[<Fact>]
let ``should add two numbers`` () =
  Assert.Equal(4, 2 + 2)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit Theory attribute on let binding
================================================================================

[<Theory>]
[<InlineData(1, 2, 3)>]
let ``should add any two numbers`` a b expected =
  Assert.Equal(expected, a + b)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit Fact attribute on member method
================================================================================

type MyTests() =
  [<Fact>]
  member this.``should add`` () =
    Assert.Equal(4, 2 + 2)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit FactAttribute suffix
================================================================================

[<FactAttribute>]
let ``should work with suffix`` () =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit fully qualified with Attribute suffix
================================================================================

[<Xunit.FactAttribute>]
let ``fully qualified test`` () =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit fully qualified without suffix
================================================================================

[<Xunit.Fact>]
let ``fully qualified no suffix`` () =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit Theory with InlineData
================================================================================

[<Theory>]
[<InlineData(42)>]
[<InlineData(0)>]
let ``should handle multiple data`` x =
  Assert.True(x >= 0)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_nunit.txt` (8 tests)

```
================================================================================
NUnit Test attribute on let binding
================================================================================

[<Test>]
let ``should validate input`` () =
  Assert.That(true, Is.True)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit TestCase attribute with arguments
================================================================================

[<TestCase(1, 2, 3)>]
let ``should add with test case`` a b expected =
  Assert.AreEqual(expected, a + b)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit TestCaseSource attribute
================================================================================

[<TestCaseSource("cases")>]
let ``should add from source`` a b expected =
  Assert.AreEqual(expected, a + b)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit Test attribute on member method
================================================================================

type NUnitTests() =
  [<Test>]
  member this.``should validate`` () =
    Assert.That(true, Is.True)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit fully qualified Test attribute
================================================================================

[<NUnit.Framework.Test>]
let ``fully qualified nunit`` () =
  Assert.Pass()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit fully qualified with Attribute suffix
================================================================================

[<NUnit.Framework.TestAttribute>]
let ``fully qualified nunit with suffix`` () =
  Assert.Pass()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit TestAttribute suffix only
================================================================================

[<TestAttribute>]
let ``nunit attribute suffix`` () =
  Assert.Pass()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit Test with Category metadata
================================================================================

[<Test>]
[<Category("Integration")>]
let ``test with category`` () =
  Assert.Pass()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_mstest.txt` (6 tests)

```
================================================================================
MSTest TestMethod on member method
================================================================================

type MSTestTests() =
  [<TestMethod>]
  member this.``should process request`` () =
    Assert.IsTrue(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest DataTestMethod attribute
================================================================================

type MSTestDataTests() =
  [<DataTestMethod>]
  [<DataRow(1, "hello")>]
  member this.``should handle data`` i s =
    Assert.IsNotNull(s)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest TestMethod on let binding
================================================================================

[<TestMethod>]
let ``mstest let binding`` () =
  Assert.IsTrue(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest TestMethodAttribute suffix
================================================================================

[<TestMethodAttribute>]
let ``mstest with suffix`` () =
  Assert.IsTrue(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest fully qualified monster
================================================================================

[<Microsoft.VisualStudio.TestTools.UnitTesting.TestMethodAttribute>]
let ``fully qualified mstest`` () =
  Assert.IsTrue(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest DataTestMethod with DataRow
================================================================================

[<DataTestMethod>]
[<DataRow(1, 2, 3)>]
[<DataRow(4, 5, 9)>]
let ``data test method with rows`` a b expected =
  Assert.AreEqual(expected, a + b)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_tunit.txt` (4 tests)

```
================================================================================
TUnit Test attribute
================================================================================

[<Test>]
let ``tunit basic test`` () =
  Assert.That(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
TUnit fully qualified Test
================================================================================

[<TUnit.Core.Test>]
let ``tunit fully qualified`` () =
  Assert.That(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
TUnit Test with Arguments
================================================================================

[<Test>]
[<Arguments(1, 2)>]
let ``tunit with arguments`` a b =
  Assert.That(a + b > 0)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
TUnit TestAttribute suffix
================================================================================

[<TestAttribute>]
let ``tunit attribute suffix`` () =
  Assert.That(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_expecto.txt` (5 tests)

```
================================================================================
Expecto Tests attribute on let value binding
================================================================================

[<Tests>]
let myTests =
  testList "suite" [
    testCase "should work" <| fun () ->
      Expect.equal 1 1 "one equals one"
  ]

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Expecto Tests in nested module
================================================================================

module Inner =
  [<Tests>]
  let innerTests =
    testCase "nested" <| fun () ->
      Expect.isTrue true "nested test"

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Expecto Tests on module-level binding
================================================================================

[<Tests>]
let allTests =
  testList "all" [
    testCase "first" <| fun () -> ()
    testCase "second" <| fun () -> ()
  ]

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Expecto Tests followed by testList composition
================================================================================

[<Tests>]
let composedTests =
  testList "composed" [
    unitTests
    integrationTests
  ]

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Expecto TestsAttribute suffix
================================================================================

[<TestsAttribute>]
let suffixedTests =
  testCase "suffixed" <| fun () -> ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_fscheck.txt` (2 tests)

```
================================================================================
FsCheck Property attribute
================================================================================

[<Property>]
let ``should reverse twice is identity`` (xs: int list) =
  List.rev (List.rev xs) = xs

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
FsCheck PropertyAttribute suffix
================================================================================

[<PropertyAttribute>]
let ``property with suffix`` (x: int) =
  x + 0 = x

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_benchmark.txt` (3 tests)

```
================================================================================
Benchmark.NET Benchmark attribute on member
================================================================================

type MyBenchmarks() =
  [<Benchmark>]
  member this.Sort() =
    Array.sort [| 3; 1; 2 |]

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark.NET Benchmark with BenchmarkCategory
================================================================================

type CategorizedBenchmarks() =
  [<Benchmark>]
  [<BenchmarkCategory("perf")>]
  member this.FastSort() =
    Array.sort [| 1; 2; 3 |]

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark.NET BenchmarkAttribute suffix
================================================================================

type SuffixBenchmarks() =
  [<BenchmarkAttribute>]
  member this.Bench() =
    ignore (1 + 1)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_negative.txt` (25 tests)

These MUST NOT be captured by the test attribute query. The query should produce
zero matches for every entry in this file. Verifies false positive prevention.

```
================================================================================
NUnit SetUp lifecycle — NOT a test
================================================================================

[<SetUp>]
let setup () =
  initializeTestState ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit TearDown lifecycle — NOT a test
================================================================================

[<TearDown>]
let teardown () =
  cleanupTestState ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit OneTimeSetUp lifecycle — NOT a test
================================================================================

[<OneTimeSetUp>]
let fixtureSetup () =
  initializeFixture ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit OneTimeTearDown lifecycle — NOT a test
================================================================================

[<OneTimeTearDown>]
let fixtureTeardown () =
  cleanupFixture ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit legacy TestFixtureSetUp — NOT a test
================================================================================

[<TestFixtureSetUp>]
let legacySetup () =
  initializeLegacy ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit legacy TestFixtureTearDown — NOT a test
================================================================================

[<TestFixtureTearDown>]
let legacyTeardown () =
  cleanupLegacy ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest ClassInitialize lifecycle — NOT a test
================================================================================

type TestLifecycle() =
  [<ClassInitialize>]
  static member Init(ctx) = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest ClassCleanup lifecycle — NOT a test
================================================================================

type TestLifecycle2() =
  [<ClassCleanup>]
  static member Cleanup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest TestInitialize lifecycle — NOT a test
================================================================================

type TestInit() =
  [<TestInitialize>]
  member this.Init() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
MSTest TestCleanup lifecycle — NOT a test
================================================================================

type TestClean() =
  [<TestCleanup>]
  member this.Cleanup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark GlobalSetup lifecycle — NOT a test
================================================================================

type BenchSetup() =
  [<GlobalSetup>]
  member this.Setup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark GlobalCleanup lifecycle — NOT a test
================================================================================

type BenchCleanup() =
  [<GlobalCleanup>]
  member this.Cleanup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark IterationSetup lifecycle — NOT a test
================================================================================

type BenchIterSetup() =
  [<IterationSetup>]
  member this.Setup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Benchmark IterationCleanup lifecycle — NOT a test
================================================================================

type BenchIterCleanup() =
  [<IterationCleanup>]
  member this.Cleanup() = ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
TUnit Before lifecycle — NOT a test
================================================================================

[<Before(Test)>]
let beforeEach () =
  resetState ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
TUnit After lifecycle — NOT a test
================================================================================

[<After(Test)>]
let afterEach () =
  verifyState ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Category metadata — NOT a test
================================================================================

[<Category("Integration")>]
let notATest () =
  ignore ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit Trait metadata — NOT a test
================================================================================

[<Trait("Category", "Unit")>]
let alsoNotATest () =
  ignore ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit InlineData without Theory — NOT a test
================================================================================

[<InlineData(42)>]
let orphanedData x =
  ignore x

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit Collection — NOT a test
================================================================================

[<Collection("SharedFixture")>]
type MyCollection() = class end

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
xUnit CollectionDefinition — NOT a test
================================================================================

[<CollectionDefinition("SharedFixture")>]
type MyCollectionDef() = class end

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
AutoOpen module attribute — NOT a test
================================================================================

[<AutoOpen>]
module Helpers =
  let add x y = x + y

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Literal value attribute — NOT a test
================================================================================

[<Literal>]
let MaxRetries = 3

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Struct type attribute — NOT a test
================================================================================

[<Struct>]
type Point = { X: float; Y: float }

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Assembly-level attribute — NOT a test
================================================================================

[<assembly: InternalsVisibleTo("MyTests")>]
do ()

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

#### File: `test/corpus/test_attributes_edge_cases.txt` (8 tests)

```
================================================================================
Multiple attributes including non-test on same function
================================================================================

[<Fact>]
[<Trait("category", "unit")>]
let ``test with metadata`` () =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Test in deeply nested module
================================================================================

module A =
  module B =
    [<Fact>]
    let ``deeply nested test`` () =
      Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Test with return type annotation
================================================================================

[<Fact>]
let ``test with annotation`` () : unit =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Test with task computation expression
================================================================================

[<Fact>]
let ``async test`` () =
  task {
    let! result = Task.FromResult(42)
    Assert.Equal(42, result)
  }

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Test function with backtick name
================================================================================

[<Fact>]
let ``should handle special characters: <>&"'`` () =
  Assert.True(true)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
Class with multiple test members
================================================================================

type MultipleTests() =
  [<Fact>]
  member this.``first test`` () =
    Assert.True(true)

  [<Fact>]
  member this.``second test`` () =
    Assert.False(false)

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
NUnit TestFixture on type — NOT a test definition
================================================================================

[<TestFixture>]
type MyFixture() =
  member this.Helper() = 42

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)

================================================================================
RequireQualifiedAccess on module — NOT a test
================================================================================

[<RequireQualifiedAccess>]
module MyStrictModule =
  let value = 42

--------------------------------------------------------------------------------

(... generated by tree-sitter parse ...)
```

### Test Corpus Summary

| Corpus File | Test Count | Purpose |
|-------------|-----------|---------|
| `test_attributes_xunit.txt` | 7 | xUnit positive cases |
| `test_attributes_nunit.txt` | 8 | NUnit positive cases |
| `test_attributes_mstest.txt` | 6 | MSTest positive cases |
| `test_attributes_tunit.txt` | 4 | TUnit positive cases |
| `test_attributes_expecto.txt` | 5 | Expecto positive cases |
| `test_attributes_fscheck.txt` | 2 | FsCheck positive cases |
| `test_attributes_benchmark.txt` | 3 | Benchmark.NET positive cases |
| `test_attributes_negative.txt` | 25 | False positive prevention |
| `test_attributes_edge_cases.txt` | 8 | Edge cases and boundaries |
| **Total** | **68** | |

**Negative test density: 37%** (25/68) — false positive prevention is as important as detection.

### Query Validation (Separate from Corpus)

In addition to corpus entries (which test grammar parsing), query validation tests run
`tree-sitter query tags.scm` against `.fs` files and verify the captured text:

1. `[<Fact>]` → captures `@test.framework = "Fact"`, `@definition.test = "should add"`
2. `[<FactAttribute>]` → captures `@test.framework = "FactAttribute"`, `@definition.test = ...`
3. `[<Xunit.FactAttribute>]` → captures `@test.framework = "FactAttribute"` (last identifier)
4. `[<SetUp>]` → zero captures (not in `#any-of?` list)
5. `[<AutoOpen>]` → zero captures

These live in `test/tags/` as `.fs` files with adjacent `.expected` files documenting captures.

### Contribution Plan

1. Fork `ionide/tree-sitter-fsharp`
2. Build locally: `npm install && npx tree-sitter generate`
3. Write all 68 F# source snippets for corpus entries
4. Run `npx tree-sitter parse` on EACH snippet to generate exact parse trees (never hand-write)
5. Assemble into 9 corpus files with generated parse trees
6. Write `tags.scm` with the queries above
7. Run `npx tree-sitter test` — all 68 corpus entries must pass
8. Write query validation `.fs` files in `test/tags/`
9. Run `npx tree-sitter query queries/tags.scm` against each validation file
10. Verify all positive cases produce correct captures
11. Verify all negative cases produce zero captures
12. Run FULL existing test suite to ensure no regressions
13. Open PR: "Add test framework attribute detection to tags.scm"

### Note on Disambiguation

Tree-sitter matches `[<Test>]` whether it's NUnit or TUnit — both use the same short name.
**This is by design.** Tree-sitter provides source-level detection (Layer 1). Framework
disambiguation happens at Layer 2 (FCS typed AST) via namespace resolution, or Layer 3
(assembly reflection) via `assemblyMarker` matching.

---

## Design Decisions

| Decision | Chosen | Why |
|----------|--------|-----|
| Interface style | Record-of-functions | Simplest for community, F#-idiomatic |
| Capability model | Two explicit tiers | Avoids combinatorial optionality |
| Purity boundary | Provider does IO, returns pure data | Pragmatic |
| Extensibility timing | Built-in first, extract patterns after 3 implementations | Ship first |
| Discovery ownership | Core owns discovery, provider supplies markers | Keeps providers ignorant of SageFs |
| `inline` on `traced` | Not used | F# restricts `inline` on structural comparison |
| `Meter` namespace | `System.Diagnostics.Metrics` | Not in `System.Diagnostics` root |
| Notification urgency | Client-side concern, not domain | `PreviousStatus` enables transitions |
| `filterByPolicy` dead arm | `ExplicitRun, Disabled` caught by `_, Disabled` | Keep the earlier wildcard match |

### Accepted Tradeoffs

- **No category-theoretic foundations** — record-of-functions + DU return types are adequate.
  Revisit if composition patterns emerge.
- **No actor isolation for providers** — try/catch at boundaries is sufficient for V1.
- **No dynamic provider loading in V1** — `.sagefs/providers/` is a future extension point.
- **Option used in filterByPolicy for Map.tryFind** — this is an F# stdlib concern, not domain
  modeling. The `Option.defaultValue` immediately eliminates it.
