module SageFs.Tests.PureFunctionCoverageTests

open Expecto
open Expecto.Flip
open FsCheck
open FSharp.Compiler.EditorServices
open SageFs.Middleware.HotReloading
open SageFs.Features.AutoCompletion
open SageFs.AppState
open SageFs.FileWatcher
open SageFs.McpAdapter
open SageFs.FsiRewrite
open SageFs.FrameDiff
open SageFs.Features.Replay
open SageFs.Features.Events
open SageFs.Features.Diagnostics
open System
open System.Text.Json

// ═══════════════════════════════════════════════════════════
// HotReloading — isTopLevelFunctionBinding
// ═══════════════════════════════════════════════════════════

let isTopLevelFunctionBindingTests = testList "isTopLevelFunctionBinding" [
  test "simple function with parameter" {
    isTopLevelFunctionBinding "let f x = x + 1"
    |> Expect.isTrue "let with param is function"
  }
  test "function with multiple params" {
    isTopLevelFunctionBinding "let add x y = x + y"
    |> Expect.isTrue "multiple params is function"
  }
  test "function with unit param" {
    isTopLevelFunctionBinding "let f () = 42"
    |> Expect.isTrue "unit param is function"
  }
  test "function with typed param" {
    isTopLevelFunctionBinding "let f (x: int) = x"
    |> Expect.isTrue "typed param is function"
  }
  test "value binding is not function" {
    isTopLevelFunctionBinding "let x = 42"
    |> Expect.isFalse "simple value is not function"
  }
  test "typed value binding is not function" {
    isTopLevelFunctionBinding "let x : int = 42"
    |> Expect.isFalse "typed value is not function"
  }
  test "indented binding is not top-level" {
    isTopLevelFunctionBinding "  let f x = x"
    |> Expect.isFalse "indented is not top-level"
  }
  test "let! is not a binding" {
    isTopLevelFunctionBinding "let! x = async { return 1 }"
    |> Expect.isFalse "let! is computation expression"
  }
  test "private function" {
    isTopLevelFunctionBinding "let private f x = x"
    |> Expect.isTrue "private function is still function"
  }
  test "inline function" {
    isTopLevelFunctionBinding "let inline f x = x"
    |> Expect.isTrue "inline function is still function"
  }
  test "rec function" {
    isTopLevelFunctionBinding "let rec f x = f x"
    |> Expect.isTrue "recursive function is still function"
  }
  test "no equals sign" {
    isTopLevelFunctionBinding "let f x"
    |> Expect.isFalse "no equals means not a complete binding"
  }
]

// ═══════════════════════════════════════════════════════════
// HotReloading — isStaticMemberFunction
// ═══════════════════════════════════════════════════════════

let isStaticMemberFunctionTests = testList "isStaticMemberFunction" [
  test "static member with parens" {
    isStaticMemberFunction "  static member Create(x) = x"
    |> Expect.isTrue "static member with parens is function"
  }
  test "static member with named param" {
    isStaticMemberFunction "  static member Add x y = x + y"
    |> Expect.isTrue "static member with params is function"
  }
  test "static member property" {
    isStaticMemberFunction "  static member Value = 42"
    |> Expect.isFalse "static member without params is property"
  }
  test "static member with type annotation" {
    isStaticMemberFunction "  static member Default: int = 0"
    |> Expect.isFalse "type-annotated value is not function"
  }
  test "non-static member is not matched" {
    isStaticMemberFunction "  member this.Foo(x) = x"
    |> Expect.isFalse "instance member is not static"
  }
  test "regular let binding" {
    isStaticMemberFunction "let f x = x"
    |> Expect.isFalse "let binding is not static member"
  }
  test "no equals sign" {
    isStaticMemberFunction "  static member Foo"
    |> Expect.isFalse "no equals, not a complete definition"
  }
]

// ═══════════════════════════════════════════════════════════
// AutoCompletion — CompletionKind.ofGlyph
// ═══════════════════════════════════════════════════════════

let completionKindOfGlyphTests = testList "CompletionKind.ofGlyph" [
  test "Class maps to Class" {
    CompletionKind.ofGlyph FSharpGlyph.Class
    |> Expect.equal "Class" CompletionKind.Class
  }
  test "Method maps to Method" {
    CompletionKind.ofGlyph FSharpGlyph.Method
    |> Expect.equal "Method" CompletionKind.Method
  }
  test "OverridenMethod maps to OverriddenMethod" {
    CompletionKind.ofGlyph FSharpGlyph.OverridenMethod
    |> Expect.equal "OverridenMethod" CompletionKind.OverriddenMethod
  }
  test "NameSpace maps to Namespace" {
    CompletionKind.ofGlyph FSharpGlyph.NameSpace
    |> Expect.equal "Namespace" CompletionKind.Namespace
  }
  test "Error maps to Type" {
    CompletionKind.ofGlyph FSharpGlyph.Error
    |> Expect.equal "Error fallback → Type" CompletionKind.Type
  }
  test "all glyphs handled without exception" {
    let glyphs = [
      FSharpGlyph.Class; FSharpGlyph.Constant; FSharpGlyph.Delegate
      FSharpGlyph.Enum; FSharpGlyph.EnumMember; FSharpGlyph.Event
      FSharpGlyph.Exception; FSharpGlyph.Field; FSharpGlyph.Interface
      FSharpGlyph.Method; FSharpGlyph.OverridenMethod; FSharpGlyph.Module
      FSharpGlyph.NameSpace; FSharpGlyph.Property; FSharpGlyph.Struct
      FSharpGlyph.Typedef; FSharpGlyph.Type; FSharpGlyph.Union
      FSharpGlyph.Variable; FSharpGlyph.ExtensionMethod; FSharpGlyph.Error
      FSharpGlyph.TypeParameter
    ]
    for g in glyphs do
      CompletionKind.ofGlyph g |> ignore
  }
]

// ═══════════════════════════════════════════════════════════
// AppState — stripAnsi
// ═══════════════════════════════════════════════════════════

let stripAnsiTests = testList "stripAnsi" [
  test "removes color escape sequences" {
    stripAnsi "\u001b[31mred text\u001b[0m"
    |> Expect.equal "color stripped" "red text"
  }
  test "removes bold/underline" {
    stripAnsi "\u001b[1mbold\u001b[22m"
    |> Expect.equal "bold stripped" "bold"
  }
  test "cursor reset becomes newline" {
    stripAnsi "line1\u001b[0Gline2"
    |> Expect.stringContains "cursor reset becomes newline" "line1"
  }
  test "plain text unchanged" {
    stripAnsi "hello world"
    |> Expect.equal "no change" "hello world"
  }
  test "empty string unchanged" {
    stripAnsi "" |> Expect.equal "empty" ""
  }
  test "multiple sequences stripped" {
    stripAnsi "\u001b[32mgreen\u001b[0m and \u001b[34mblue\u001b[0m"
    |> Expect.equal "both stripped" "green and blue"
  }
]

// ═══════════════════════════════════════════════════════════
// AppState — reformatExpectoSummary
// ═══════════════════════════════════════════════════════════

let reformatExpectoSummaryTests = testList "reformatExpectoSummary" [
  test "reformats standard expecto summary" {
    let input = "EXPECTO! 10 tests run in 00:00:01.234 for MyTests \u2013 8 passed, 1 ignored, 1 failed, 0 errored. OK!"
    let result = reformatExpectoSummary input
    result |> Expect.stringContains "has test suite name" "MyTests"
    result |> Expect.stringContains "has count" "10"
    result |> Expect.stringContains "has passed" "8"
    result |> Expect.stringContains "has failed" "1"
  }
  test "non-expecto line passes through" {
    let input = "just a regular line"
    reformatExpectoSummary input
    |> Expect.equal "unchanged" "just a regular line"
  }
]

// ═══════════════════════════════════════════════════════════
// Behavioral contract properties (FsCheck)
//
// These test WHY each function exists — the invariants that
// callers depend on. If any of these break, something upstream
// (hot-reload, dashboard, autocomplete, file watcher) will
// silently misbehave.
// ═══════════════════════════════════════════════════════════

let cfg = { FsCheckConfig.defaultConfig with maxTest = 200 }

// --- stripAnsi: the output pipeline depends on these ---

/// Callers may process text multiple times (cleanStdout then display).
/// If stripping mutates already-clean text, re-processing corrupts output.
let stripAnsiIdempotent =
  testPropertyWithConfig cfg
    "stripAnsi: stripping twice equals stripping once (idempotent)"
    <| fun (s: NonEmptyString) ->
      let once = stripAnsi s.Get
      stripAnsi once = once

/// Dashboard and MCP tool output must be free of ANSI CSI sequences.
/// A surviving CSI renders as garbage in HTML and JSON contexts.
let stripAnsiNoCompleteCsiSurvives =
  testPropertyWithConfig cfg
    "stripAnsi: no complete ANSI CSI sequence survives"
    <| fun (s: NonEmptyString) ->
      let cleaned = stripAnsi s.Get
      not (Text.RegularExpressions.Regex.IsMatch(cleaned, @"\x1b\[[0-9;]*[a-zA-Z]"))

/// OSC sequences (title changes, hyperlinks) must also be stripped.
let stripAnsiNoOscSurvives =
  testPropertyWithConfig cfg
    "stripAnsi: no OSC sequence (ESC]...BEL) survives"
    <| fun (s: NonEmptyString) ->
      let cleaned = stripAnsi s.Get
      not (Text.RegularExpressions.Regex.IsMatch(cleaned, @"\x1b\].*?\x07"))

/// Most text passing through cleanStdout is already plain ASCII.
/// If the regex accidentally mutates non-ANSI text, real output is corrupted.
let stripAnsiIdentityOnPlainText =
  testPropertyWithConfig cfg
    "stripAnsi: plain ASCII text passes through unchanged"
    <| fun (s: NonEmptyString) ->
      let plain = Text.RegularExpressions.Regex.Replace(s.Get, @"[^\x20-\x7E]", "")
      plain.Length = 0 || stripAnsi plain = plain

// --- cleanStdout: dashboard SSE handler depends on these ---

/// The dashboard applies cleanStdout on every state change. During
/// state replay or snapshot rebuild, output may be re-processed.
/// Idempotency guarantees stability across replays.
let cleanStdoutIdempotent =
  testPropertyWithConfig cfg
    "cleanStdout: cleaning already-clean text is stable (idempotent)"
    <| fun (s: NonEmptyString) ->
      let once = cleanStdout s.Get
      cleanStdout once = once

/// Same rationale as stripAnsi — dashboard HTML must not contain ANSI.
let cleanStdoutNoAnsi =
  testPropertyWithConfig cfg
    "cleanStdout: output never contains complete ANSI sequences"
    <| fun (s: NonEmptyString) ->
      let cleaned = cleanStdout s.Get
      not (Text.RegularExpressions.Regex.IsMatch(cleaned, @"\x1b\[[0-9;]*[a-zA-Z]"))

// --- isTopLevelFunctionBinding: hot-reload patch safety ---

/// Hot reload only needs NoInlining on TOP-LEVEL functions.
/// Nested bindings (inside match/if/function bodies) are never
/// Harmony patch targets. Injecting attributes on them would
/// change indentation semantics and potentially break compilation.
let topLevelRejectsIndented =
  testPropertyWithConfig cfg
    "isTopLevelFunctionBinding: any leading whitespace means not top-level"
    <| fun (NonNegativeInt indent) (name: NonEmptyString) ->
      let spaces = String(' ', indent + 1)
      let safeName = Text.RegularExpressions.Regex.Replace(name.Get, @"[^a-zA-Z_]", "a")
      let line = sprintf "%slet %s x = x" spaces safeName
      isTopLevelFunctionBinding line = false

/// Users mark functions `private`/`inline`/`rec` for their own reasons.
/// The hot-reload detector must classify function-vs-value regardless
/// of modifiers. If modifiers confused it, private functions would
/// silently escape hot-reload — a nasty latent bug.
let modifiersDontAffectClassification =
  let modifiers = ["private "; "internal "; "public "; "inline "; "rec "]
  testList "isTopLevelFunctionBinding: modifiers preserve function-vs-value" [
    for m in modifiers do
      test (sprintf "'let %sf x = x' is still a function" m) {
        isTopLevelFunctionBinding (sprintf "let %sf x = x" m)
        |> Expect.isTrue (sprintf "%s preserves function" m)
      }
      test (sprintf "'let %sx = 42' is still a value" m) {
        isTopLevelFunctionBinding (sprintf "let %sx = 42" m)
        |> Expect.isFalse (sprintf "%s preserves value" m)
      }
  ]

// --- injectNoInlining: transform correctness ---

/// The transform must ONLY add attributes — never modify comments,
/// blank lines, type definitions, or other non-function code. If it
/// accidentally mutates a non-function line, it introduces compile
/// errors or changes the semantics of the user's code.
let injectPreservesNonFunctionLines = testList "injectNoInlining: non-function lines preserved" [
  test "type definition passes through" {
    injectNoInlining "type X = { A: int }" |> Expect.equal "type def unchanged" "type X = { A: int }"
  }
  test "comments pass through" {
    injectNoInlining "// this is a comment\n(* block comment *)"
    |> Expect.equal "comments unchanged" "// this is a comment\n(* block comment *)"
  }
  test "open declarations pass through" {
    injectNoInlining "open System\nopen System.IO"
    |> Expect.equal "opens unchanged" "open System\nopen System.IO"
  }
  test "empty string passes through" {
    injectNoInlining "" |> Expect.equal "empty unchanged" ""
  }
]

/// Each function needs exactly one [<MethodImpl(NoInlining)>] to
/// prevent JIT inlining. Zero = Harmony patch invisible. This
/// structural property ensures the transform is precise.
let injectAttributeCountMatchesFunctionCount =
  test "injectNoInlining: attribute count equals function count" {
    let code = "let f x = x\nlet x = 42\nlet g (y: int) = y * 2\n  static member Create(z) = z"
    let result = injectNoInlining code
    let lines = result.Split('\n')
    let attrCount =
      lines |> Array.filter (fun l -> l.TrimStart().StartsWith("[<MethodImpl")) |> Array.length
    let funcCount =
      code.Split('\n')
      |> Array.filter (fun l -> isTopLevelFunctionBinding l || isStaticMemberFunction l)
      |> Array.length
    attrCount |> Expect.equal "one attribute per function" funcCount
  }

// --- shouldExcludeFile: glob matching contracts ---

/// The default WatchConfig has no exclusions. If empty patterns
/// accidentally matched everything, hot-reload would silently
/// stop watching all files.
let emptyPatternsExcludeNothing =
  testPropertyWithConfig cfg
    "shouldExcludeFile: empty pattern list never excludes"
    <| fun (s: NonEmptyString) ->
      shouldExcludeFile [] s.Get = false

/// Windows file systems are case-insensitive. Patterns like "Bin/*"
/// must match "bin/Debug/test.dll". Without case-insensitive matching,
/// exclusion patterns silently fail on Windows.
let excludeIsCaseInsensitive = testList "shouldExcludeFile: case insensitive matching" [
  test "uppercase pattern matches lowercase path" {
    shouldExcludeFile ["BIN/*"] "bin/Debug/test.dll" |> Expect.isTrue "BIN/* matches bin/"
  }
  test "lowercase pattern matches uppercase path" {
    shouldExcludeFile ["bin/*"] "BIN/Debug/test.dll" |> Expect.isTrue "bin/* matches BIN/"
  }
]

/// FileSystemWatcher on Windows reports backslashes, but users write
/// glob patterns with forward slashes. Without normalization, the same
/// path would match or not depending on OS.
let excludeNormalizesSlashes = testList "shouldExcludeFile: slash normalization" [
  test "forward-slash pattern matches backslash path" {
    shouldExcludeFile ["bin/*"] @"bin\Debug\test.dll"
    |> Expect.isTrue "forward matches backslash"
  }
  test "backslash pattern matches forward-slash path" {
    shouldExcludeFile [@"bin\*"] "bin/Debug/test.dll"
    |> Expect.isTrue "backslash matches forward"
  }
]

// --- fileChangeAction: hot-reload routing safety ---

/// When a .fs file is deleted, its old definitions remain valid in FSI.
/// Trying to #load a deleted file would crash. The hot-reload system
/// MUST treat deletes as no-ops regardless of extension.
let deletedFilesAlwaysIgnored =
  testPropertyWithConfig cfg
    "fileChangeAction: deleted files are always Ignore regardless of extension"
    <| fun (s: NonEmptyString) ->
      let exts = [".fs"; ".fsx"; ".fsproj"; ".txt"; ".dll"; ""]
      exts |> List.forall (fun ext ->
        let change = { FilePath = s.Get + ext; Kind = FileChangeKind.Deleted; Timestamp = DateTimeOffset.UtcNow }
        fileChangeAction change = FileChangeAction.Ignore
      )

/// The escalation hierarchy is a critical design decision:
///   Reload = cheapest (re-eval one file via #load)
///   SoftReset = heavier (rebuild all assembly references)
/// Wrong routing means either wasted rebuilds or stale code.
let fileChangeActionEscalation = testList "fileChangeAction: escalation hierarchy" [
  for ext, expectReload, label in [
    ".fs", true, "source → Reload"
    ".fsx", true, "script → Reload"
    ".fsproj", false, "project → SoftReset"
    ".txt", false, "unrelated → Ignore"
  ] do
    test label {
      let change = { FilePath = sprintf "test%s" ext; Kind = FileChangeKind.Changed; Timestamp = DateTimeOffset.UtcNow }
      let result = fileChangeAction change
      if expectReload then
        match result with
        | FileChangeAction.Reload _ -> ()
        | other -> failwith (sprintf "expected Reload, got %A" other)
      elif ext = ".fsproj" then
        result |> Expect.equal label FileChangeAction.SoftReset
      else
        result |> Expect.equal label FileChangeAction.Ignore
    }
]

// --- shouldTriggerRebuild: build noise suppression ---

/// IDEs create temp files (~foo.fs, foo.tmp), and build output lands
/// in bin/obj. Without this filter, every IDE save causes a double-
/// rebuild and every build triggers an infinite rebuild loop.
let tempFilesNeverTriggerRebuild = testList "shouldTriggerRebuild: temp/build artifacts filtered" [
  let config = { Directories = []; Extensions = [".fs"; ".fsx"; ".fsproj"]; ExcludePatterns = []; DebounceMs = 500 }
  for path, label in [
    "~MyFile.fs", "tilde-prefix"; "scratch.fs.tmp", "tmp-suffix"
    "src/obj/Debug/net10.0/test.fs", "obj dir"; "src/bin/Debug/net10.0/SageFs.dll", "bin dir"
    @"C:\proj\bin\Release\net10.0\app.fs", "Win bin"; @"C:\proj\obj\Debug\net10.0\temp.fsx", "Win obj"
  ] do
    test label { shouldTriggerRebuild config path |> Expect.isFalse label }
]

/// Users configure exclusion patterns to suppress rebuilds for generated
/// code, vendored deps, etc. If patterns were ignored after a refactor,
/// generated files would trigger spurious rebuilds.
let exclusionPatternsHonored = testList "shouldTriggerRebuild: exclusion patterns respected" [
  let config ext pats = { Directories = []; Extensions = ext; ExcludePatterns = pats; DebounceMs = 500 }
  test "excluded .fs file doesn't trigger" {
    shouldTriggerRebuild (config [".fs"] ["**/Generated/*"]) "src/Generated/Code.fs"
    |> Expect.isFalse "excluded"
  }
  test "non-excluded .fs file triggers" {
    shouldTriggerRebuild (config [".fs"] ["**/Generated/*"]) "src/MyCode.fs"
    |> Expect.isTrue "not excluded"
  }
]

/// Only configured extensions trigger rebuilds. Firing on unconfigured
/// extensions means spurious rebuilds (slow). NOT firing on configured
/// ones means stale code (dangerous).
let onlyConfiguredExtensionsTrigger = testList "shouldTriggerRebuild: only configured extensions" [
  let config exts = { Directories = []; Extensions = exts; ExcludePatterns = []; DebounceMs = 500 }
  test ".fs fires when configured" {
    shouldTriggerRebuild (config [".fs"]) "src/App.fs" |> Expect.isTrue ".fs"
  }
  test ".cs silent when only .fs configured" {
    shouldTriggerRebuild (config [".fs"]) "src/App.cs" |> Expect.isFalse ".cs"
  }
  test "empty extensions means nothing fires" {
    shouldTriggerRebuild (config []) "src/App.fs" |> Expect.isFalse "empty"
  }
]

// --- scoreCandidate: autocomplete ranking contracts ---

/// When a user types "get" in autocomplete, they expect "getUser" to
/// rank above "budgetItem" even though "budget" fuzzy-matches "get".
/// The 200-point prefix bonus makes autocomplete feel responsive.
let prefixMatchBeatsFuzzy =
  testPropertyWithConfig cfg
    "scoreCandidate: prefix match always outscores non-prefix of same length"
    <| fun (NonEmptyString prefix) ->
      let safePfx = Text.RegularExpressions.Regex.Replace(prefix, @"[^a-zA-Z]", "a")
      if safePfx.Length = 0 then true
      else
        let prefixCandidate = safePfx + "Suffix"
        let nonPrefixCandidate = "zz" + safePfx + "Suffix"
        scoreCandidate safePfx prefixCandidate > scoreCandidate safePfx nonPrefixCandidate

/// When two candidates both prefix-match, the shorter one is more
/// likely what the user wants. The brevity bonus (100/(len+1))
/// ensures "getX" ranks above "getVeryLongMethodName".
let shorterCandidateScoresHigher =
  testPropertyWithConfig cfg
    "scoreCandidate: shorter candidate scores >= longer for same prefix"
    <| fun (NonEmptyString prefix) (PositiveInt extra) ->
      let safePfx = Text.RegularExpressions.Regex.Replace(prefix, @"[^a-zA-Z]", "a")
      if safePfx.Length = 0 then true
      else
        let short = safePfx + "x"
        let long = safePfx + String('x', extra + 2)
        scoreCandidate safePfx short >= scoreCandidate safePfx long

// --- reformatExpectoSummary: output passthrough safety ---

/// cleanStdout applies reformatExpectoSummary to every line containing
/// "EXPECTO!". Non-matching lines MUST pass through unchanged — if the
/// regex accidentally matches non-Expecto output, it mangles it.
let reformatPassthroughOnNonExpecto =
  testPropertyWithConfig cfg
    "reformatExpectoSummary: non-matching lines pass through verbatim"
    <| fun (s: NonEmptyString) ->
      let plain = Text.RegularExpressions.Regex.Replace(s.Get, @"EXPECTO!", "test")
      reformatExpectoSummary plain = plain

// ═══════════════════════════════════════════════════════════
// McpAdapter — escapeJson (RFC 8259 §7 compliance)
//
// WHY: escapeJson is used at 14+ call sites to serialize
// arbitrary text into JSON strings for MCP clients and
// browser dashboards. If control characters slip through,
// JSON parsing fails silently in downstream consumers.
// ═══════════════════════════════════════════════════════════

let escapeJsonPropertyCfg = { FsCheckConfig.defaultConfig with maxTest = 500 }

let escapeJsonProducesValidJson =
  testPropertyWithConfig escapeJsonPropertyCfg
    "escapeJson: output always produces parseable JSON string"
    <| fun (s: NonEmptyString) ->
      let escaped = escapeJson s.Get
      let json = sprintf "\"%s\"" escaped
      try
        use doc = JsonDocument.Parse(json)
        doc.RootElement.GetString() |> ignore
        true
      with _ -> false

let escapeJsonRoundtripPreservesContent =
  testPropertyWithConfig escapeJsonPropertyCfg
    "escapeJson: JSON parse roundtrip preserves original content"
    <| fun (s: NonEmptyString) ->
      let escaped = escapeJson s.Get
      let json = sprintf "\"%s\"" escaped
      use doc = JsonDocument.Parse(json)
      doc.RootElement.GetString() = s.Get

let escapeJsonControlCharExamples = testList "escapeJson: control character examples" [
  test "backspace is escaped" {
    let result = escapeJson "hello\bworld"
    use doc = JsonDocument.Parse(sprintf "\"%s\"" result)
    doc.RootElement.GetString() |> Expect.equal "roundtrip" "hello\bworld"
  }
  test "form feed is escaped" {
    let result = escapeJson "page\x0Cbreak"
    use doc = JsonDocument.Parse(sprintf "\"%s\"" result)
    doc.RootElement.GetString() |> Expect.equal "roundtrip" "page\x0Cbreak"
  }
  test "null byte is escaped" {
    let result = escapeJson "null\x00here"
    use doc = JsonDocument.Parse(sprintf "\"%s\"" result)
    doc.RootElement.GetString() |> Expect.equal "roundtrip" "null\x00here"
  }
  test "all standard escapes present" {
    let input = "\"\\\n\r\t\b\x0C"
    let escaped = escapeJson input
    escaped |> Expect.stringContains "has quote" "\\\""
    escaped |> Expect.stringContains "has backslash" "\\\\"
    escaped |> Expect.stringContains "has newline" "\\n"
    escaped |> Expect.stringContains "has return" "\\r"
    escaped |> Expect.stringContains "has tab" "\\t"
  }
]

// ═══════════════════════════════════════════════════════════
// FsiRewrite — rewriteInlineUseStatements
//
// WHY: FSI doesn't support `use` at top level. The rewrite
// replaces leading `use ` with `let ` for FSI eval. A bug
// that replaces ALL occurrences of "use " in the line
// corrupts string literals containing "use ".
// ═══════════════════════════════════════════════════════════

let rewriteInlineUseStatementTests = testList "rewriteInlineUseStatements" [
  test "leading use becomes let" {
    let input = "    use svc = createService()"
    let result = rewriteInlineUseStatements input
    result |> Expect.equal "rewritten" "    let svc = createService()"
  }
  test "use inside string literal is NOT replaced" {
    let input = """    use svc = create "use this service" """
    let result = rewriteInlineUseStatements input
    result |> Expect.stringContains "string preserved" "\"use this service\""
  }
  test "line without use is unchanged" {
    let input = "    let x = 42"
    let result = rewriteInlineUseStatements input
    result |> Expect.equal "unchanged" "    let x = 42"
  }
  test "use! is not rewritten" {
    let input = "    use! conn = getConnectionAsync()"
    let result = rewriteInlineUseStatements input
    result |> Expect.equal "use! unchanged" "    use! conn = getConnectionAsync()"
  }
  test "multiple lines only rewrites the use lines" {
    let input = "    use x = a()\n    let y = 42\n    use z = b()"
    let result = rewriteInlineUseStatements input
    result |> Expect.stringContains "first rewritten" "    let x = a()"
    result |> Expect.stringContains "middle unchanged" "    let y = 42"
    result |> Expect.stringContains "third rewritten" "    let z = b()"
  }
  test "idempotent — rewriting twice gives same result" {
    let input = "    use svc = createService()"
    let once = rewriteInlineUseStatements input
    let twice = rewriteInlineUseStatements once
    twice |> Expect.equal "idempotent" once
  }
]

// ═══════════════════════════════════════════════════════════
// McpAdapter — splitStatements (gap-closing examples)
//
// WHY: splitStatements parses F# code on `;;` boundaries
// for FSI evaluation. Incorrect splitting inside strings,
// comments, or nested constructs would send broken code
// to the compiler.
// ═══════════════════════════════════════════════════════════

let splitStatementsGapTests = testList "splitStatements: gap-closing examples" [
  test "double-semicolon inside string literal is not a boundary" {
    let code = """let x = "hello;; world";; """
    let parts = splitStatements code
    parts.Length |> Expect.equal "one statement" 1
  }
  test "double-semicolon inside line comment is not a boundary" {
    let code = "// this is a comment with ;;\nlet x = 1;;"
    let parts = splitStatements code
    parts.Length |> Expect.equal "one statement" 1
  }
  test "double-semicolon inside block comment is not a boundary" {
    let code = "(* comment with ;; inside *)\nlet x = 1;;"
    let parts = splitStatements code
    parts.Length |> Expect.equal "one statement" 1
  }
  test "multiple real boundaries produce multiple statements" {
    let code = "let x = 1;;\nlet y = 2;;"
    let parts = splitStatements code
    parts.Length |> Expect.equal "two statements" 2
  }
  test "nested parentheses don't confuse parser" {
    let code = "let f (x: (int * int)) = fst x;;"
    let parts = splitStatements code
    parts.Length |> Expect.equal "one statement" 1
  }
  test "triple-quoted string with semicolons" {
    let code = "let x = \"\"\"a;;b;;c\"\"\";;"
    let parts = splitStatements code
    parts.Length |> Expect.equal "one statement" 1
  }
  test "empty input produces no statements" {
    let parts = splitStatements ""
    parts.Length |> Expect.equal "no statements" 0
  }
  test "whitespace-only input produces no statements" {
    let parts = splitStatements "   \n  \n  "
    parts.Length |> Expect.equal "no statements" 0
  }
]

// ═══════════════════════════════════════════════════════════
// KeyMap — parseConfigLines
//
// WHY: parseConfigLines reads user keybinding configs. Zero
// coverage means we don't know if the hand-rolled parser
// handles edge cases (empty input, malformed lines, unknown
// keys). A crash here blocks SageFs startup.
// ═══════════════════════════════════════════════════════════

let parseConfigLinesTests = testList "SageFs.KeyMap.parseConfigLines contracts" [
  test "valid two-binding config produces two entries" {
    let lines = [|
      "let keybindings = ["
      "  \"Ctrl+Q\", \"Quit\""
      "  \"Ctrl+S\", \"CycleFocus\""
      "]"
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "two bindings" 2
  }
  test "lines before 'let keybindings' are ignored" {
    let lines = [|
      "// this is a comment"
      "let x = 42"
      "let keybindings = ["
      "  \"Ctrl+Q\", \"Quit\""
      "]"
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "one binding" 1
  }
  test "empty keybindings block produces empty map" {
    let lines = [| "let keybindings = ["; "]" |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "no bindings" 0
  }
  test "unrecognized key combo is silently skipped" {
    let lines = [|
      "let keybindings = ["
      "  \"NotAKey+Combo\", \"Quit\""
      "  \"Ctrl+Q\", \"Quit\""
      "]"
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "only valid binding" 1
  }
  test "unrecognized action is silently skipped" {
    let lines = [|
      "let keybindings = ["
      "  \"Ctrl+Q\", \"NotAnAction\""
      "  \"Ctrl+Q\", \"Quit\""
      "]"
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "only valid binding" 1
  }
  test "never crashes on empty input" {
    let result = SageFs.KeyMap.parseConfigLines [||]
    result.Count |> Expect.equal "empty" 0
  }
  test "never crashes on arbitrary non-config text" {
    let lines = [| "random text"; "more stuff"; "123"; "" |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "no bindings" 0
  }
  test "closing bracket stops parsing" {
    let lines = [|
      "let keybindings = ["
      "  \"Ctrl+Q\", \"Quit\""
      "]"
      "  \"Ctrl+S\", \"CycleFocus\""
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "stops at bracket" 1
  }
  test "case-insensitive 'Keybindings' detection" {
    let lines = [|
      "let Keybindings = ["
      "  \"Ctrl+Q\", \"Quit\""
      "]"
    |]
    let result = SageFs.KeyMap.parseConfigLines lines
    result.Count |> Expect.equal "case insensitive" 1
  }
]

let parseConfigLinesNeverCrashes =
  testPropertyWithConfig { FsCheckConfig.defaultConfig with maxTest = 300 }
    "parseConfigLines: never crashes on arbitrary string arrays"
    <| fun (lines: string[]) ->
      let safeLines = lines |> Array.filter (not << isNull)
      try
        SageFs.KeyMap.parseConfigLines safeLines |> ignore
        true
      with _ -> false

// ═══════════════════════════════════════════════════════════
// McpAdapter — formatEventsJson / formatCompletionsJson
// (MCP protocol contract tests)
//
// WHY: These functions serialize data for MCP clients. If the
// JSON shape changes, all MCP consumers (VS Code, Neovim,
// Visual Studio) break silently.
// ═══════════════════════════════════════════════════════════

let formatEventsJsonTests = testList "formatEventsJson: MCP protocol contract" [
  test "empty events produces valid JSON with count 0" {
    let result = formatEventsJson []
    use doc = JsonDocument.Parse(result)
    let root = doc.RootElement
    root.GetProperty("count").GetInt32() |> Expect.equal "count" 0
    root.GetProperty("events").GetArrayLength() |> Expect.equal "array" 0
  }
  test "single event produces correct JSON shape" {
    let ts = DateTime(2025, 1, 15, 10, 30, 0, DateTimeKind.Utc)
    let result = formatEventsJson [(ts, "mcp:test", "hello world")]
    use doc = JsonDocument.Parse(result)
    let root = doc.RootElement
    root.GetProperty("count").GetInt32() |> Expect.equal "count" 1
    let ev = root.GetProperty("events").[0]
    ev.GetProperty("source").GetString() |> Expect.equal "source" "mcp:test"
    ev.GetProperty("text").GetString() |> Expect.equal "text" "hello world"
    ev.GetProperty("timestamp").GetString().Length > 0 |> Expect.isTrue "has timestamp"
  }
  test "special characters in text are properly escaped" {
    let ts = DateTime(2025, 1, 15, 10, 30, 0, DateTimeKind.Utc)
    let result = formatEventsJson [(ts, "src", "line1\nline2\ttab\"quote")]
    use doc = JsonDocument.Parse(result)
    let text = doc.RootElement.GetProperty("events").[0].GetProperty("text").GetString()
    text |> Expect.equal "roundtrip" "line1\nline2\ttab\"quote"
  }
]

let formatCompletionsJsonTests = testList "formatCompletionsJson: MCP protocol contract" [
  test "empty completions produces valid JSON with count 0" {
    let result = formatCompletionsJson []
    use doc = JsonDocument.Parse(result)
    let root = doc.RootElement
    root.GetProperty("count").GetInt32() |> Expect.equal "count" 0
    root.GetProperty("completions").GetArrayLength() |> Expect.equal "array" 0
  }
  test "completion item has label, kind, insertText fields" {
    let item = {
      DisplayText = "myFunction"
      ReplacementText = "myFunction"
      Kind = CompletionKind.Method
      GetDescription = None
    }
    let result = formatCompletionsJson [item]
    use doc = JsonDocument.Parse(result)
    let comp = doc.RootElement.GetProperty("completions").[0]
    comp.GetProperty("label").GetString() |> Expect.equal "label" "myFunction"
    comp.GetProperty("kind").GetString() |> Expect.equal "kind" "Method"
    comp.GetProperty("insertText").GetString() |> Expect.equal "insert" "myFunction"
  }
  test "special characters in display text are properly escaped" {
    let item = {
      DisplayText = "func<'a>"
      ReplacementText = "func"
      Kind = CompletionKind.Union
      GetDescription = None
    }
    let result = formatCompletionsJson [item]
    use doc = JsonDocument.Parse(result)
    let label = doc.RootElement.GetProperty("completions").[0].GetProperty("label").GetString()
    label |> Expect.equal "roundtrip" "func<'a>"
  }
]

// ═══════════════════════════════════════════════════════════
// FrameDiff — parsePositionedLines / diff
// ═══════════════════════════════════════════════════════════

let moveTo row col = sprintf "\x1b[%d;%dH" row col

let parsePositionedLinesTests = testList "parsePositionedLines" [
  test "empty string produces empty map" {
    parsePositionedLines "" |> Expect.isEmpty "no rows"
  }
  test "single row with moveTo" {
    let frame = moveTo 1 1 + "Hello"
    let result = parsePositionedLines frame
    result |> Map.find 1 |> Expect.stringContains "should have content" "Hello"
  }
  test "multiple distinct rows" {
    let frame = moveTo 1 1 + "Row1" + moveTo 3 1 + "Row3"
    let result = parsePositionedLines frame
    result |> Map.count |> Expect.equal "two rows" 2
    result |> Map.find 1 |> Expect.stringContains "row 1" "Row1"
    result |> Map.find 3 |> Expect.stringContains "row 3" "Row3"
  }
  test "same row appearing twice appends content" {
    let frame = moveTo 2 1 + "Part1" + moveTo 2 10 + "Part2"
    let result = parsePositionedLines frame
    result |> Map.count |> Expect.equal "one row key" 1
    let content = result |> Map.find 2
    content |> Expect.stringContains "has Part1" "Part1"
    content |> Expect.stringContains "has Part2" "Part2"
  }
  test "color codes preserved in row content" {
    let frame = moveTo 1 1 + "\x1b[32mGreen\x1b[0m"
    let result = parsePositionedLines frame
    let content = result |> Map.find 1
    content |> Expect.stringContains "has green" "\x1b[32m"
    content |> Expect.stringContains "has reset" "\x1b[0m"
  }
  test "erase K code preserved in row" {
    let frame = moveTo 1 1 + "\x1b[K"
    let result = parsePositionedLines frame
    let content = result |> Map.find 1
    content |> Expect.stringContains "has K seq" "\x1b[K"
  }
  test "text before any moveTo is discarded" {
    let frame = "OrphanText" + moveTo 1 1 + "Hello"
    let result = parsePositionedLines frame
    result |> Map.find 1 |> Expect.stringContains "has Hello" "Hello"
    let content = result |> Map.find 1
    content.Contains("Orphan") |> Expect.isFalse "orphan discarded"
  }
  test "row-only moveTo uses row with default col" {
    let frame = "\x1b[5H" + "Content"
    let result = parsePositionedLines frame
    result |> Map.containsKey 5 |> Expect.isTrue "row 5 exists"
  }
]

let frameDiffExampleTests = testList "diff examples" [
  test "identical frames produce empty diff" {
    let frame = moveTo 1 1 + "Hello" + moveTo 2 1 + "World"
    diff frame frame |> Expect.equal "no changes" ""
  }
  test "changed rows only in diff" {
    let prev = moveTo 1 1 + "Same" + moveTo 2 1 + "Old"
    let next = moveTo 1 1 + "Same" + moveTo 2 1 + "New"
    let result = diff prev next
    result.Contains("New") |> Expect.isTrue "has new content"
    result.Contains("Same") |> Expect.isFalse "no unchanged rows"
  }
  test "new rows appear in diff" {
    let prev = moveTo 1 1 + "Row1"
    let next = moveTo 1 1 + "Row1" + moveTo 3 1 + "Row3"
    let result = diff prev next
    result.Contains("Row3") |> Expect.isTrue "has new row"
    result.Contains("Row1") |> Expect.isFalse "no unchanged"
  }
  test "empty previous means all rows appear" {
    let next = moveTo 1 1 + "Hello"
    let result = diff "" next
    result.Contains("Hello") |> Expect.isTrue "has all content"
  }
  test "both empty gives empty diff" {
    diff "" "" |> Expect.equal "empty" ""
  }
]

let frameDiffProperties = testList "diff properties" [
  testProperty "diffing identical frames always produces empty string" <| fun (s: string) ->
    let s = if isNull s then "" else s
    let frame = moveTo 1 1 + s
    diff frame frame = ""
]

// ═══════════════════════════════════════════════════════════
// Replay — SessionReplayState / DaemonReplayState
// ═══════════════════════════════════════════════════════════

let replayTs = DateTimeOffset(2024, 1, 1, 0, 0, 0, TimeSpan.Zero)
let mkDiag sev msg : DiagnosticEvent =
  { Severity = sev; Message = msg; StartLine = 1; StartColumn = 1; EndLine = 1; EndColumn = 5 }

let sessionReplayTests = testList "SessionReplayState.applyEvent" [
  test "SessionStarted sets WarmingUp and StartedAt" {
    let evt = SessionStarted {| Config = Map.empty; StartedAt = replayTs |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.Status |> Expect.equal "status" WarmingUp
    s.StartedAt |> Expect.equal "started" (Some replayTs)
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
  }
  test "SessionWarmUpCompleted stores errors" {
    let evt = SessionWarmUpCompleted {| Duration = TimeSpan.FromSeconds 1.0; Errors = ["err1"] |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.WarmupErrors |> Expect.equal "errors" ["err1"]
  }
  test "SessionWarmUpProgress updates LastActivity" {
    let evt = SessionWarmUpProgress {| Step = 1; Total = 5; Message = "step" |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
  }
  test "SessionReady sets Ready" {
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty SessionReady
    s.Status |> Expect.equal "status" Ready
  }
  test "SessionFaulted sets Faulted with error" {
    let evt = SessionFaulted {| Error = "boom"; StackTrace = None |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.Status |> Expect.equal "status" (Faulted "boom")
  }
  test "SessionReset increments ResetCount and sets WarmingUp" {
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty SessionReset
    s.Status |> Expect.equal "status" WarmingUp
    s.ResetCount |> Expect.equal "count" 1
  }
  test "SessionHardReset increments HardResetCount" {
    let evt = SessionHardReset {| Rebuild = true |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.HardResetCount |> Expect.equal "count" 1
    s.Status |> Expect.equal "status" WarmingUp
  }
  test "EvalRequested sets Evaluating" {
    let evt = EvalRequested {| Code = "1+1"; Source = EventSource.Console |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.Status |> Expect.equal "status" Evaluating
  }
  test "EvalCompleted increments count and records history" {
    let evt = EvalCompleted {| Code = "1+1"; Result = "2"; TypeSignature = Some "int"; Duration = TimeSpan.FromMilliseconds 50.0 |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.EvalCount |> Expect.equal "count" 1
    s.LastEvalResult |> Expect.equal "result" (Some "2")
    s.EvalHistory |> Expect.hasLength "history" 1
    s.Status |> Expect.equal "status" Ready
  }
  test "EvalFailed increments failed count and stores diagnostics" {
    let evt = EvalFailed {| Code = "bad"; Error = "oops"; Diagnostics = [mkDiag DiagnosticSeverity.Error "oops"] |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.FailedEvalCount |> Expect.equal "count" 1
    s.LastDiagnostics |> Expect.hasLength "diags" 1
    s.Status |> Expect.equal "status" Ready
  }
  test "DiagnosticsChecked stores diagnostics" {
    let evt = DiagnosticsChecked {| Code = "x"; Diagnostics = [mkDiag DiagnosticSeverity.Warning "warn"]; Source = EventSource.Console |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastDiagnostics |> Expect.hasLength "diags" 1
  }
  test "DiagnosticsCleared empties diagnostics" {
    let init = { SessionReplayState.empty with LastDiagnostics = [mkDiag DiagnosticSeverity.Error "e"] }
    let s = SessionReplayState.applyEvent replayTs init DiagnosticsCleared
    s.LastDiagnostics |> Expect.isEmpty "cleared"
  }
  test "ScriptLoaded updates activity only" {
    let evt = ScriptLoaded {| FilePath = "test.fsx"; StatementCount = 3; Source = EventSource.Console |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
    s.Status |> Expect.equal "status unchanged" NotStarted
  }
  test "ScriptLoadFailed updates activity only" {
    let evt = ScriptLoadFailed {| FilePath = "bad.fsx"; Error = "parse error" |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
  }
  test "McpInputReceived updates activity only" {
    let evt = McpInputReceived {| Source = EventSource.Console; Content = "hello" |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
  }
  test "McpOutputSent updates activity only" {
    let evt = McpOutputSent {| Source = EventSource.Console; Content = "result" |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s.LastActivity |> Expect.equal "activity" (Some replayTs)
  }
  test "DaemonSessionCreated is ignored by session replay" {
    let evt = DaemonSessionCreated {| SessionId = "s1"; Projects = ["p.fsproj"]; WorkingDir = "/tmp"; CreatedAt = replayTs |}
    let s = SessionReplayState.applyEvent replayTs SessionReplayState.empty evt
    s |> Expect.equal "unchanged" SessionReplayState.empty
  }
]

let daemonReplayTests = testList "DaemonReplayState.applyEvent" [
  test "DaemonSessionCreated adds session and sets active" {
    let evt = DaemonSessionCreated {| SessionId = "s1"; Projects = ["p.fsproj"]; WorkingDir = "/tmp"; CreatedAt = replayTs |}
    let s = DaemonReplayState.applyEvent DaemonReplayState.empty evt
    s.Sessions |> Map.containsKey "s1" |> Expect.isTrue "session added"
    s.ActiveSessionId |> Expect.equal "active" (Some "s1")
  }
  test "DaemonSessionStopped picks next alive session as active" {
    let created1 = DaemonSessionCreated {| SessionId = "s1"; Projects = []; WorkingDir = "/"; CreatedAt = replayTs |}
    let created2 = DaemonSessionCreated {| SessionId = "s2"; Projects = []; WorkingDir = "/"; CreatedAt = replayTs |}
    let stopped = DaemonSessionStopped {| SessionId = "s1"; StoppedAt = replayTs.AddHours(1.0) |}
    let s =
      DaemonReplayState.empty
      |> fun s -> DaemonReplayState.applyEvent s created1
      |> fun s -> DaemonReplayState.applyEvent s created2
      |> fun s -> DaemonReplayState.applyEvent s stopped
    s.Sessions.["s1"].StoppedAt.IsSome |> Expect.isTrue "s1 stopped"
    s.ActiveSessionId |> Expect.equal "s2 now active" (Some "s2")
  }
  test "DaemonSessionSwitched sets active" {
    let evt = DaemonSessionSwitched {| FromId = Some "s1"; ToId = "s2"; SwitchedAt = replayTs |}
    let s = DaemonReplayState.applyEvent DaemonReplayState.empty evt
    s.ActiveSessionId |> Expect.equal "active" (Some "s2")
  }
  test "non-daemon events are ignored" {
    let s = DaemonReplayState.applyEvent DaemonReplayState.empty SessionReady
    s |> Expect.equal "unchanged" DaemonReplayState.empty
  }
  test "aliveSessions filters out stopped" {
    let created1 = DaemonSessionCreated {| SessionId = "s1"; Projects = []; WorkingDir = "/"; CreatedAt = replayTs |}
    let created2 = DaemonSessionCreated {| SessionId = "s2"; Projects = []; WorkingDir = "/"; CreatedAt = replayTs |}
    let stopped = DaemonSessionStopped {| SessionId = "s1"; StoppedAt = replayTs.AddHours(1.0) |}
    let s =
      DaemonReplayState.empty
      |> fun s -> DaemonReplayState.applyEvent s created1
      |> fun s -> DaemonReplayState.applyEvent s created2
      |> fun s -> DaemonReplayState.applyEvent s stopped
    let alive = s.Sessions |> Map.filter (fun _ r -> r.StoppedAt.IsNone)
    alive |> Map.count |> Expect.equal "one alive" 1
    alive |> Map.containsKey "s2" |> Expect.isTrue "s2 alive"
  }
]

let replayStreamIntegrationTests = testList "replayStream integration" [
  test "full lifecycle produces correct final state" {
    let events = [
      replayTs, SessionStarted {| Config = Map.empty; StartedAt = replayTs |}
      replayTs.AddSeconds(1.0), SessionWarmUpCompleted {| Duration = TimeSpan.FromSeconds 1.0; Errors = [] |}
      replayTs.AddSeconds(2.0), SessionReady
      replayTs.AddSeconds(3.0), EvalRequested {| Code = "1+1;;"; Source = EventSource.Console |}
      replayTs.AddSeconds(4.0), EvalCompleted {| Code = "1+1;;"; Result = "2"; TypeSignature = Some "int"; Duration = TimeSpan.FromMilliseconds 50.0 |}
      replayTs.AddSeconds(5.0), EvalRequested {| Code = "bad;;"; Source = EventSource.Console |}
      replayTs.AddSeconds(6.0), EvalFailed {| Code = "bad;;"; Error = "not defined"; Diagnostics = [] |}
      replayTs.AddSeconds(7.0), SessionReset
      replayTs.AddSeconds(8.0), SessionReady
    ]
    let final = SessionReplayState.replayStream events
    final.Status |> Expect.equal "status" Ready
    final.EvalCount |> Expect.equal "evals" 1
    final.FailedEvalCount |> Expect.equal "failed" 1
    final.ResetCount |> Expect.equal "resets" 1
    final.HardResetCount |> Expect.equal "hard resets" 0
    final.EvalHistory |> Expect.hasLength "history" 1
    final.StartedAt |> Expect.equal "started" (Some replayTs)
    final.LastActivity |> Expect.isSome "has last activity"
  }
]

[<Tests>]
let allPureFunctionCoverageTests = testList "Pure function coverage" [
  testList "HotReloading" [
    isTopLevelFunctionBindingTests
    isStaticMemberFunctionTests
  ]
  testList "AutoCompletion" [
    completionKindOfGlyphTests
  ]
  testList "AppState" [
    stripAnsiTests
    reformatExpectoSummaryTests
  ]
  testList "Behavioral contracts" [
    testList "stripAnsi" [
      stripAnsiIdempotent
      stripAnsiNoCompleteCsiSurvives
      stripAnsiNoOscSurvives
      stripAnsiIdentityOnPlainText
    ]
    testList "cleanStdout" [
      cleanStdoutIdempotent
      cleanStdoutNoAnsi
    ]
    testList "isTopLevelFunctionBinding" [
      topLevelRejectsIndented
      modifiersDontAffectClassification
    ]
    testList "injectNoInlining" [
      injectPreservesNonFunctionLines
      injectAttributeCountMatchesFunctionCount
    ]
    testList "shouldExcludeFile" [
      emptyPatternsExcludeNothing
      excludeIsCaseInsensitive
      excludeNormalizesSlashes
    ]
    testList "fileChangeAction" [
      deletedFilesAlwaysIgnored
      fileChangeActionEscalation
    ]
    testList "shouldTriggerRebuild" [
      tempFilesNeverTriggerRebuild
      exclusionPatternsHonored
      onlyConfiguredExtensionsTrigger
    ]
    testList "scoreCandidate" [
      prefixMatchBeatsFuzzy
      shorterCandidateScoresHigher
    ]
    testList "reformatExpectoSummary" [
      reformatPassthroughOnNonExpecto
    ]
  ]
  testList "Expert-guided priority tests" [
    testList "escapeJson" [
      escapeJsonProducesValidJson
      escapeJsonRoundtripPreservesContent
      escapeJsonControlCharExamples
    ]
    testList "rewriteInlineUseStatements" [
      rewriteInlineUseStatementTests
    ]
    testList "splitStatements" [
      splitStatementsGapTests
    ]
    testList "parseConfigLines" [
      parseConfigLinesTests
      parseConfigLinesNeverCrashes
    ]
    testList "formatEventsJson" [
      formatEventsJsonTests
    ]
    testList "formatCompletionsJson" [
      formatCompletionsJsonTests
    ]
  ]
  testList "FrameDiff and Replay tests" [
    testList "FrameDiff" [
      parsePositionedLinesTests
      frameDiffExampleTests
      frameDiffProperties
    ]
    testList "Replay" [
      sessionReplayTests
      daemonReplayTests
      replayStreamIntegrationTests
    ]
  ]
]
