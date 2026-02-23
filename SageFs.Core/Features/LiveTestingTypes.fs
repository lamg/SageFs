namespace SageFs.Features.LiveTesting

open System
open System.Reflection
open System.Security.Cryptography
open System.Text

// --- Stable Test Identity ---

type TestId = private TestId of string

module TestId =
  let create (fullName: string) (framework: string) =
    let input = sprintf "%s|%s" fullName framework
    let bytes = Encoding.UTF8.GetBytes(input)
    let hash = SHA256.HashData(bytes)
    TestId(Convert.ToHexString(hash).Substring(0, 16))

  let value (TestId id) = id

// --- Test Categories & Run Policies ---

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
  let defaults =
    Map.ofList [
      TestCategory.Unit, RunPolicy.OnEveryChange
      TestCategory.Integration, RunPolicy.OnDemand
      TestCategory.Browser, RunPolicy.OnDemand
      TestCategory.Benchmark, RunPolicy.OnDemand
      TestCategory.Architecture, RunPolicy.OnSaveOnly
      TestCategory.Property, RunPolicy.OnEveryChange
    ]

[<RequireQualifiedAccess>]
type RunTrigger =
  | Keystroke
  | FileSave
  | ExplicitRun

// --- Assembly Info ---

type AssemblyInfo = {
  Name: string
  Location: string
  ReferencedAssemblies: AssemblyName array
}

// --- Source-Level Detection ---

[<Struct>]
type SourceTestLocation = {
  AttributeName: string
  FilePath: string
  Line: int
  Column: int
}

// --- Test Origin ---

[<RequireQualifiedAccess>]
type TestOrigin =
  | SourceMapped of file: string * line: int
  | ReflectionOnly

// --- Test Case ---

type TestCase = {
  Id: TestId
  FullName: string
  DisplayName: string
  Origin: TestOrigin
  Labels: string list
  Framework: string
  Category: TestCategory
}

// --- Test Failure & Results ---

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

// --- Run History ---

[<RequireQualifiedAccess>]
type RunHistory =
  | NeverRun
  | PreviousRun of duration: TimeSpan

// --- Test Run Status (UI lifecycle) ---

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

// --- Provider Descriptions (pure data — stored in Elm model) ---

type AttributeProviderDescription = {
  Name: string
  TestAttributes: string list
  AssemblyMarker: string
}

type CustomProviderDescription = {
  Name: string
  AssemblyMarker: string
}

[<RequireQualifiedAccess>]
type ProviderDescription =
  | AttributeBased of AttributeProviderDescription
  | Custom of CustomProviderDescription

// --- Dependency Graph ---

type TestDependencyGraph = {
  SymbolToTests: Map<string, TestId array>
  TransitiveCoverage: Map<string, TestId array>
  SourceVersion: int
}

// --- Coverage ---

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

// --- IL Branch Coverage ---

[<RequireQualifiedAccess>]
type LineCoverage =
  | FullyCovered
  | PartiallyCovered of covered: int * total: int
  | NotCovered

type SequencePoint = {
  File: string
  Line: int
  Column: int
  BranchId: int
}

type CoverageState = {
  Slots: SequencePoint array
  Hits: bool array
}

// --- Pipeline Timing ---

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

// --- Live Test State (Elm model aggregate) ---

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

// --- Pure functions — all stubs for RED phase ---

module TestDependencyGraph =
  let empty = {
    SymbolToTests = Map.empty
    TransitiveCoverage = Map.empty
    SourceVersion = 0
  }

  let findAffected (changedSymbols: string list) (graph: TestDependencyGraph) : TestId array =
    changedSymbols
    |> List.choose (fun sym -> Map.tryFind sym graph.SymbolToTests)
    |> Array.concat
    |> Array.distinct

  /// BFS from a symbol through the call graph, returning all reachable symbols.
  let private reachableFrom (callGraph: Map<string, string array>) (start: string) : string list =
    let visited = System.Collections.Generic.HashSet<string>()
    let queue = System.Collections.Generic.Queue<string>()
    queue.Enqueue(start)
    visited.Add(start) |> ignore
    while queue.Count > 0 do
      let current = queue.Dequeue()
      match Map.tryFind current callGraph with
      | Some callees ->
        for callee in callees do
          if visited.Add(callee) then
            queue.Enqueue(callee)
      | None -> ()
    visited |> Seq.toList

  /// Compute transitive coverage: for every symbol reachable from a directly-tested
  /// symbol via the call graph, attribute those tests to the callee.
  let computeTransitiveCoverage
    (callGraph: Map<string, string array>)
    (directSymbolToTests: Map<string, TestId array>)
    : Map<string, TestId array> =
    let mutable result = System.Collections.Generic.Dictionary<string, System.Collections.Generic.HashSet<TestId>>()
    for kvp in directSymbolToTests do
      let symbol = kvp.Key
      let testIds = kvp.Value
      let reachable = reachableFrom callGraph symbol
      for reached in reachable do
        match result.TryGetValue(reached) with
        | true, existing ->
          for tid in testIds do existing.Add(tid) |> ignore
        | false, _ ->
          let hs = System.Collections.Generic.HashSet<TestId>()
          for tid in testIds do hs.Add(tid) |> ignore
          result.[reached] <- hs
    result
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Seq.toArray)
    |> Map.ofSeq

module LiveTesting =
  let filterByPolicy
    (policies: Map<TestCategory, RunPolicy>)
    (trigger: RunTrigger)
    (tests: TestCase array)
    : TestCase array =
    tests
    |> Array.filter (fun test ->
      match Map.tryFind test.Category policies with
      | Some RunPolicy.Disabled -> false
      | Some RunPolicy.OnDemand ->
        match trigger with
        | RunTrigger.ExplicitRun -> true
        | _ -> false
      | Some RunPolicy.OnSaveOnly ->
        match trigger with
        | RunTrigger.ExplicitRun | RunTrigger.FileSave -> true
        | _ -> false
      | Some RunPolicy.OnEveryChange -> true
      | None -> true)

  let mergeResults (state: LiveTestState) (results: TestRunResult array) : LiveTestState =
    if Array.isEmpty results then state
    else
      let newResults =
        results
        |> Array.fold (fun acc r -> Map.add r.TestId r acc) state.LastResults
      let maxDuration =
        results
        |> Array.map (fun r ->
          match r.Result with
          | TestResult.Passed d -> d
          | TestResult.Failed (_, d) -> d
          | _ -> System.TimeSpan.Zero)
        |> Array.max
      { state with
          LastResults = newResults
          IsRunning = false
          History = RunHistory.PreviousRun maxDuration }

  let computeStatusEntries (state: LiveTestState) : TestStatusEntry array =
    state.DiscoveredTests
    |> Array.map (fun test ->
      let status =
        match Map.tryFind test.Category state.RunPolicies with
        | Some RunPolicy.Disabled -> TestRunStatus.PolicyDisabled
        | _ ->
          if Set.contains test.Id state.AffectedTests then
            TestRunStatus.Queued
          else
            match Map.tryFind test.Id state.LastResults with
            | Some r ->
              match r.Result with
              | TestResult.Passed d -> TestRunStatus.Passed d
              | TestResult.Failed (f, d) -> TestRunStatus.Failed (f, d)
              | TestResult.Skipped reason -> TestRunStatus.Skipped reason
              | TestResult.NotRun -> TestRunStatus.Detected
            | None -> TestRunStatus.Detected
      { TestId = test.Id
        DisplayName = test.DisplayName
        FullName = test.FullName
        Origin = test.Origin
        Framework = test.Framework
        Category = test.Category
        CurrentPolicy =
          match Map.tryFind test.Category state.RunPolicies with
          | Some p -> p
          | None -> RunPolicy.OnEveryChange
        Status = status
        PreviousStatus = TestRunStatus.Detected })

module CoverageProjection =
  let symbolCoverage
    (graph: TestDependencyGraph)
    (results: Map<TestId, TestRunResult>)
    (symbol: string)
    : CoverageStatus =
    match Map.tryFind symbol graph.TransitiveCoverage with
    | None -> CoverageStatus.NotCovered
    | Some tests when Array.isEmpty tests -> CoverageStatus.NotCovered
    | Some tests ->
      let allPassing =
        tests |> Array.forall (fun tid ->
          match Map.tryFind tid results with
          | Some r ->
            match r.Result with
            | TestResult.Passed _ -> true
            | _ -> false
          | None -> false)
      CoverageStatus.Covered (tests.Length, allPassing)

  let computeAll
    (graph: TestDependencyGraph)
    (results: Map<TestId, TestRunResult>)
    : Map<string, CoverageStatus> =
    graph.TransitiveCoverage
    |> Map.map (fun symbol tests ->
      if Array.isEmpty tests then CoverageStatus.NotCovered
      else
        let allPassing =
          tests |> Array.forall (fun tid ->
            match Map.tryFind tid results with
            | Some r ->
              match r.Result with
              | TestResult.Passed _ -> true
              | _ -> false
            | None -> false)
        CoverageStatus.Covered (tests.Length, allPassing))

module CoverageComputation =
  let computeLineCoverage (state: CoverageState) (file: string) (line: int) : LineCoverage =
    let matching =
      state.Slots
      |> Array.mapi (fun i slot -> (slot, state.Hits.[i]))
      |> Array.filter (fun (slot, _) -> slot.File = file && slot.Line = line)
    if Array.isEmpty matching then LineCoverage.NotCovered
    else
      let total = matching.Length
      let covered = matching |> Array.filter snd |> Array.length
      if covered = total then LineCoverage.FullyCovered
      elif covered = 0 then LineCoverage.NotCovered
      else LineCoverage.PartiallyCovered (covered, total)

module PipelineTiming =
  let treeSitterMs (t: PipelineTiming) : float =
    match t.Depth with
    | PipelineDepth.TreeSitterOnly ts -> ts.TotalMilliseconds
    | PipelineDepth.ThroughFcs (ts, _) -> ts.TotalMilliseconds
    | PipelineDepth.ThroughExecution (ts, _, _) -> ts.TotalMilliseconds

module TestProviderDescriptions =
  let builtInDescriptions : ProviderDescription list = [
    ProviderDescription.AttributeBased {
      Name = "xunit"; TestAttributes = ["Fact"; "Theory"]
      AssemblyMarker = "xunit.core"
    }
    ProviderDescription.AttributeBased {
      Name = "nunit"; TestAttributes = ["Test"; "TestCase"; "TestCaseSource"]
      AssemblyMarker = "nunit.framework"
    }
    ProviderDescription.AttributeBased {
      Name = "mstest"; TestAttributes = ["TestMethod"; "DataTestMethod"]
      AssemblyMarker = "Microsoft.VisualStudio.TestPlatform.TestFramework"
    }
    ProviderDescription.AttributeBased {
      Name = "tunit"; TestAttributes = ["Test"]
      AssemblyMarker = "TUnit.Core"
    }
    ProviderDescription.Custom {
      Name = "expecto"; AssemblyMarker = "Expecto"
    }
  ]

  let detectProviders
    (assemblies: AssemblyInfo list)
    : ProviderDescription list =
    let refNames =
      assemblies
      |> List.collect (fun asm ->
        asm.ReferencedAssemblies
        |> Array.map (fun r -> r.Name)
        |> Array.toList)
      |> Set.ofList
    builtInDescriptions
    |> List.filter (fun desc ->
      let marker =
        match desc with
        | ProviderDescription.AttributeBased d -> d.AssemblyMarker
        | ProviderDescription.Custom d -> d.AssemblyMarker
      Set.contains marker refNames)
