// TestTreeViewModel tests — self-contained script
open System
open Expecto

// Mirror types
[<RequireQualifiedAccess>]
type TestOutcome =
  | Passed of durationMs: float
  | Failed of message: string * durationMs: float option
  | Skipped of reason: string
  | Errored of message: string
  | Running | Detected | Stale | PolicyDisabled

[<RequireQualifiedAccess>]
type TestId = | TestId of string
module TestId =
  let create s = TestId.TestId s
  let value (TestId.TestId s) = s

type TestInfo = {
  Id: TestId; DisplayName: string; FullName: string
  FilePath: string option; Line: int option
}
type TestResult = {
  Id: TestId; Outcome: TestOutcome
  DurationMs: float option; Output: string option
}
type TestSummary = {
  Total: int; Passed: int; Failed: int
  Running: int; Stale: int; Disabled: int
}
[<RequireQualifiedAccess>]
type LiveTestingEnabled = | On | Off
[<RequireQualifiedAccess>]
type ResultFreshness = | Fresh | StaleCodeEdited | StaleWrongGeneration
[<RequireQualifiedAccess>]
type RunPolicy = | EveryKeystroke | OnSave | OnDemand | Disabled
[<RequireQualifiedAccess>]
type TestCategory = | Unit | Integration | Browser | Benchmark | Architecture | Property

type LiveTestState = {
  Tests: Map<TestId, TestInfo>
  Results: Map<TestId, TestResult>
  RunningTests: Set<TestId>
  Enabled: LiveTestingEnabled
  LastSummary: TestSummary option
  Freshness: ResultFreshness
  Policies: Map<TestCategory, RunPolicy>
}
module LiveTestState =
  let empty: LiveTestState = {
    Tests = Map.empty; Results = Map.empty; RunningTests = Set.empty
    Enabled = LiveTestingEnabled.Off; LastSummary = None
    Freshness = ResultFreshness.Fresh; Policies = Map.empty
  }

// ViewModel types
[<RequireQualifiedAccess>]
type TestStatusFilter = | All | FailedOnly | RunningOnly | StaleOnly | PassedOnly

type TestFileGroup = {
  FilePath: string
  Tests: (TestInfo * TestResult option) array
}

// ViewModel module (exact copy of what's in LiveTestingTypes.fs)
[<RequireQualifiedAccess>]
module TestTreeViewModel =
  let private outcomeIcon (outcome: TestOutcome) =
    match outcome with
    | TestOutcome.Passed _ -> "✓"
    | TestOutcome.Failed _ | TestOutcome.Errored _ -> "✗"
    | TestOutcome.Running -> "●"
    | TestOutcome.Stale -> "◌"
    | TestOutcome.Detected -> "○"
    | TestOutcome.Skipped _ | TestOutcome.PolicyDisabled -> "⊘"

  let private outcomeOrder (outcome: TestOutcome) =
    match outcome with
    | TestOutcome.Failed _ | TestOutcome.Errored _ -> 0
    | TestOutcome.Running -> 1
    | TestOutcome.Stale -> 2
    | TestOutcome.Detected -> 3
    | TestOutcome.Passed _ -> 4
    | TestOutcome.Skipped _ -> 5
    | TestOutcome.PolicyDisabled -> 6

  let private formatDuration (ms: float option) =
    match ms with
    | None -> ""
    | Some d when d < 1.0 -> "(<1ms)"
    | Some d when d < 1000.0 -> sprintf "(%.0fms)" d
    | Some d -> sprintf "(%.1fs)" (d / 1000.0)

  let groupByFile (state: LiveTestState) : TestFileGroup array =
    state.Tests
    |> Map.toArray
    |> Array.map (fun (_, info) ->
      let result = Map.tryFind info.Id state.Results
      let path = info.FilePath |> Option.defaultValue "(no file)"
      path, (info, result))
    |> Array.groupBy fst
    |> Array.map (fun (path, items) ->
      { FilePath = path; Tests = items |> Array.map snd })
    |> Array.sortBy (fun g -> IO.Path.GetFileName g.FilePath)

  let filterGroups (filter: TestStatusFilter) (groups: TestFileGroup array) : TestFileGroup array =
    match filter with
    | TestStatusFilter.All -> groups
    | _ ->
      let predicate (_, result: TestResult option) =
        match filter, result with
        | TestStatusFilter.FailedOnly, Some r ->
          match r.Outcome with TestOutcome.Failed _ | TestOutcome.Errored _ -> true | _ -> false
        | TestStatusFilter.RunningOnly, Some r ->
          match r.Outcome with TestOutcome.Running -> true | _ -> false
        | TestStatusFilter.StaleOnly, Some r ->
          match r.Outcome with TestOutcome.Stale -> true | _ -> false
        | TestStatusFilter.PassedOnly, Some r ->
          match r.Outcome with TestOutcome.Passed _ -> true | _ -> false
        | _ -> false
      groups
      |> Array.choose (fun g ->
        let filtered = g.Tests |> Array.filter predicate
        if filtered.Length > 0 then Some { g with Tests = filtered }
        else None)

  let searchGroups (query: string) (groups: TestFileGroup array) : TestFileGroup array =
    if String.IsNullOrWhiteSpace query then groups
    else
      let q = query.ToLowerInvariant()
      groups
      |> Array.choose (fun g ->
        let filtered =
          g.Tests |> Array.filter (fun (info, _) ->
            info.DisplayName.ToLowerInvariant().Contains(q) ||
            info.FullName.ToLowerInvariant().Contains(q))
        if filtered.Length > 0 then Some { g with Tests = filtered }
        else None)

  let sortTests (tests: (TestInfo * TestResult option) array) =
    tests |> Array.sortBy (fun (info, result) ->
      let order = match result with Some r -> outcomeOrder r.Outcome | None -> 3
      order, info.DisplayName)

  let formatGroupHeader (group: TestFileGroup) =
    let fileName = IO.Path.GetFileName group.FilePath
    let total = group.Tests.Length
    let passed =
      group.Tests
      |> Array.filter (fun (_, r) ->
        match r with Some { Outcome = TestOutcome.Passed _ } -> true | _ -> false)
      |> Array.length
    let failed =
      group.Tests
      |> Array.filter (fun (_, r) ->
        match r with
        | Some { Outcome = TestOutcome.Failed _ }
        | Some { Outcome = TestOutcome.Errored _ } -> true
        | _ -> false)
      |> Array.length
    if failed > 0 then
      sprintf "✗ %s (%d/%d passed, %d failed)" fileName passed total failed
    else
      sprintf "✓ %s (%d/%d passed)" fileName passed total

  let formatTestLine (info: TestInfo) (result: TestResult option) =
    let icon = match result with Some r -> outcomeIcon r.Outcome | None -> "○"
    let duration = match result with Some r -> formatDuration r.DurationMs | None -> ""
    if duration = "" then sprintf "%s %s" icon info.DisplayName
    else sprintf "%s %s %s" icon info.DisplayName duration

  let filterLabel (filter: TestStatusFilter) =
    match filter with
    | TestStatusFilter.All -> "All"
    | TestStatusFilter.FailedOnly -> "Failed"
    | TestStatusFilter.RunningOnly -> "Running"
    | TestStatusFilter.StaleOnly -> "Stale"
    | TestStatusFilter.PassedOnly -> "Passed"

  let nextFilter (filter: TestStatusFilter) =
    match filter with
    | TestStatusFilter.All -> TestStatusFilter.FailedOnly
    | TestStatusFilter.FailedOnly -> TestStatusFilter.RunningOnly
    | TestStatusFilter.RunningOnly -> TestStatusFilter.StaleOnly
    | TestStatusFilter.StaleOnly -> TestStatusFilter.PassedOnly
    | TestStatusFilter.PassedOnly -> TestStatusFilter.All

  let formatGroupedOutput (filter: TestStatusFilter) (search: string) (state: LiveTestState) =
    if state.Tests.Count = 0 then "No tests discovered yet."
    else
      let groups =
        groupByFile state
        |> filterGroups filter
        |> searchGroups search
      if groups.Length = 0 then
        sprintf "No tests match filter '%s'%s"
          (filterLabel filter)
          (if String.IsNullOrWhiteSpace search then ""
           else sprintf " and search '%s'" search)
      else
        let sb = Text.StringBuilder()
        for group in groups do
          sb.AppendLine(formatGroupHeader group) |> ignore
          let sorted = sortTests group.Tests
          for (info, result) in sorted do
            sb.Append("  ").AppendLine(formatTestLine info result) |> ignore
          sb.AppendLine() |> ignore
        sb.ToString().TrimEnd()

// Test helpers
let mkState (tests: (string * string * TestOutcome option) list) : LiveTestState =
  let mutable testMap = Map.empty
  let mutable resultMap = Map.empty
  for (name, file, outcome) in tests do
    let id = TestId.create name
    let info = {
      Id = id; DisplayName = name; FullName = sprintf "Namespace.%s" name
      FilePath = Some file; Line = Some 1
    }
    testMap <- Map.add id info testMap
    match outcome with
    | Some o ->
      let result = { Id = id; Outcome = o; DurationMs = (match o with TestOutcome.Passed d -> Some d | _ -> None); Output = None }
      resultMap <- Map.add id result resultMap
    | None -> ()
  { LiveTestState.empty with Tests = testMap; Results = resultMap }

// Tests
let testTreeViewModelTests = testList "TestTreeViewModel" [
  testList "groupByFile" [
    test "empty state returns empty" {
      let groups = TestTreeViewModel.groupByFile LiveTestState.empty
      groups |> Expect.hasLength "should be empty" 0
    }
    test "groups tests by file path" {
      let state = mkState [
        ("Test1", "src/A.fs", Some (TestOutcome.Passed 10.0))
        ("Test2", "src/A.fs", Some (TestOutcome.Passed 20.0))
        ("Test3", "src/B.fs", Some (TestOutcome.Failed ("boom", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      groups |> Expect.hasLength "should have 2 groups" 2
      groups.[0].FilePath |> Expect.equal "first group is A.fs" "src/A.fs"
      groups.[0].Tests |> Expect.hasLength "A.fs has 2 tests" 2
      groups.[1].FilePath |> Expect.equal "second group is B.fs" "src/B.fs"
      groups.[1].Tests |> Expect.hasLength "B.fs has 1 test" 1
    }
    test "tests without file go to (no file)" {
      let id = TestId.create "OrphanTest"
      let state = { LiveTestState.empty with
                      Tests = Map.ofList [ id, { Id = id; DisplayName = "OrphanTest"; FullName = "OrphanTest"; FilePath = None; Line = None } ] }
      let groups = TestTreeViewModel.groupByFile state
      groups |> Expect.hasLength "should have 1 group" 1
      groups.[0].FilePath |> Expect.equal "should be (no file)" "(no file)"
    }
  ]

  testList "filterGroups" [
    test "All returns everything" {
      let state = mkState [
        ("Pass1", "A.fs", Some (TestOutcome.Passed 10.0))
        ("Fail1", "A.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let filtered = TestTreeViewModel.filterGroups TestStatusFilter.All groups
      filtered |> Expect.hasLength "should keep all groups" 1
      filtered.[0].Tests |> Expect.hasLength "should keep all tests" 2
    }
    test "FailedOnly keeps only failures" {
      let state = mkState [
        ("Pass1", "A.fs", Some (TestOutcome.Passed 10.0))
        ("Fail1", "A.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let filtered = TestTreeViewModel.filterGroups TestStatusFilter.FailedOnly groups
      filtered |> Expect.hasLength "should have 1 group" 1
      filtered.[0].Tests |> Expect.hasLength "should have 1 test" 1
    }
    test "filter removes empty groups" {
      let state = mkState [
        ("Pass1", "A.fs", Some (TestOutcome.Passed 10.0))
        ("Fail1", "B.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let filtered = TestTreeViewModel.filterGroups TestStatusFilter.FailedOnly groups
      filtered |> Expect.hasLength "should have only B.fs" 1
      filtered.[0].FilePath |> Expect.equal "should be B.fs" "B.fs"
    }
  ]

  testList "searchGroups" [
    test "empty query returns all" {
      let state = mkState [ ("Test1", "A.fs", None) ]
      let groups = TestTreeViewModel.groupByFile state
      let searched = TestTreeViewModel.searchGroups "" groups
      searched |> Expect.hasLength "should keep all" 1
    }
    test "search is case-insensitive" {
      let state = mkState [
        ("MyTest", "A.fs", None)
        ("Other", "A.fs", None)
      ]
      let groups = TestTreeViewModel.groupByFile state
      let searched = TestTreeViewModel.searchGroups "mytest" groups
      searched.[0].Tests |> Expect.hasLength "should find 1 match" 1
    }
    test "search removes empty groups" {
      let state = mkState [
        ("Alpha", "A.fs", None)
        ("Beta", "B.fs", None)
      ]
      let groups = TestTreeViewModel.groupByFile state
      let searched = TestTreeViewModel.searchGroups "Alpha" groups
      searched |> Expect.hasLength "should have only A.fs" 1
    }
  ]

  testList "sortTests" [
    test "failures sort before passes" {
      let state = mkState [
        ("Pass1", "A.fs", Some (TestOutcome.Passed 10.0))
        ("Fail1", "A.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let sorted = TestTreeViewModel.sortTests groups.[0].Tests
      let (firstInfo, _) = sorted.[0]
      firstInfo.DisplayName |> Expect.equal "failure should be first" "Fail1"
    }
  ]

  testList "formatGroupHeader" [
    test "all passed" {
      let state = mkState [
        ("T1", "src/Tests.fs", Some (TestOutcome.Passed 10.0))
        ("T2", "src/Tests.fs", Some (TestOutcome.Passed 20.0))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let header = TestTreeViewModel.formatGroupHeader groups.[0]
      header |> Expect.stringContains "should have check" "✓"
      header |> Expect.stringContains "should have 2/2" "2/2 passed"
    }
    test "some failed" {
      let state = mkState [
        ("T1", "src/Tests.fs", Some (TestOutcome.Passed 10.0))
        ("T2", "src/Tests.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let groups = TestTreeViewModel.groupByFile state
      let header = TestTreeViewModel.formatGroupHeader groups.[0]
      header |> Expect.stringContains "should have X" "✗"
      header |> Expect.stringContains "should show failed count" "1 failed"
    }
  ]

  testList "formatTestLine" [
    test "passed test with duration" {
      let info = { Id = TestId.create "T1"; DisplayName = "my test"; FullName = "my test"; FilePath = None; Line = None }
      let result = Some { Id = info.Id; Outcome = TestOutcome.Passed 42.0; DurationMs = Some 42.0; Output = None }
      let line = TestTreeViewModel.formatTestLine info result
      line |> Expect.stringContains "should have check" "✓"
      line |> Expect.stringContains "should have name" "my test"
      line |> Expect.stringContains "should have duration" "(42ms)"
    }
    test "no result shows empty circle" {
      let info = { Id = TestId.create "T1"; DisplayName = "my test"; FullName = "my test"; FilePath = None; Line = None }
      let line = TestTreeViewModel.formatTestLine info None
      line |> Expect.stringContains "should have circle" "○"
    }
  ]

  testList "formatGroupedOutput" [
    test "empty state" {
      let output = TestTreeViewModel.formatGroupedOutput TestStatusFilter.All "" LiveTestState.empty
      output |> Expect.equal "should say no tests" "No tests discovered yet."
    }
    test "no matches shows filter message" {
      let state = mkState [ ("T1", "A.fs", Some (TestOutcome.Passed 10.0)) ]
      let output = TestTreeViewModel.formatGroupedOutput TestStatusFilter.FailedOnly "" state
      output |> Expect.stringContains "should mention filter" "No tests match filter"
    }
    test "produces grouped output" {
      let state = mkState [
        ("T1", "src/A.fs", Some (TestOutcome.Passed 10.0))
        ("T2", "src/B.fs", Some (TestOutcome.Failed ("x", None)))
      ]
      let output = TestTreeViewModel.formatGroupedOutput TestStatusFilter.All "" state
      output |> Expect.stringContains "should have A.fs header" "A.fs"
      output |> Expect.stringContains "should have B.fs header" "B.fs"
      output |> Expect.stringContains "should have T1" "T1"
      output |> Expect.stringContains "should have T2" "T2"
    }
  ]

  testList "filterLabel and nextFilter" [
    test "filterLabel All" {
      TestTreeViewModel.filterLabel TestStatusFilter.All
      |> Expect.equal "should be All" "All"
    }
    test "nextFilter cycles" {
      TestTreeViewModel.nextFilter TestStatusFilter.All
      |> Expect.equal "should go to FailedOnly" TestStatusFilter.FailedOnly
      TestTreeViewModel.nextFilter TestStatusFilter.PassedOnly
      |> Expect.equal "should wrap to All" TestStatusFilter.All
    }
  ]
]

Expecto.Tests.runTestsWithCLIArgs [] [||] testTreeViewModelTests
