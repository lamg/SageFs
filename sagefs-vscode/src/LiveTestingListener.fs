module SageFs.Vscode.LiveTestingListener

open Fable.Core.JsInterop
open Vscode

open SageFs.Vscode.LiveTestingTypes
open SageFs.Vscode.JsHelpers

// ── Server JSON → VscLiveTestEvent mappers ───────────────────

/// Extract DU Case string from a Fable-serialized DU object
let parseDuCase (du: obj) : string option =
  tryOfObj du
  |> Option.map (fun du ->
    du?Case
    |> tryOfObj
    |> Option.map unbox<string>
    |> Option.defaultValue (string du))

/// Extract the first field from a Fable-serialized DU's Fields array
let duFirstField<'T> (du: obj) : 'T option =
  tryOfObj du
  |> Option.bind (fun du ->
    tryOfObj du?Fields
    |> Option.bind (fun fields ->
      let arr = fields :> obj array
      match arr.Length with 0 -> None | _ -> Some (unbox<'T> arr.[0])))

/// Extract DU Fields array from a Fable-serialized DU
let duFields (du: obj) : obj array option =
  tryOfObj du
  |> Option.bind (fun du ->
    tryOfObj du?Fields
    |> Option.map unbox<obj array>)

/// Parse HH:MM:SS duration string to milliseconds
let parseDuration (dur: string) : float option =
  tryOfObj dur
  |> Option.bind (fun dur ->
    let parts = dur.Split(':')
    match parts.Length with
    | 3 ->
      let h = float parts.[0]
      let m = float parts.[1]
      let s = float parts.[2]
      Some ((h * 3600.0 + m * 60.0 + s) * 1000.0)
    | _ -> None)

/// Extract TestId string from a server TestId DU object
let parseTestId (testIdObj: obj) : string =
  tryOfObj testIdObj
  |> Option.bind (duFirstField<string>)
  |> Option.defaultValue (
    tryOfObj testIdObj
    |> Option.map string
    |> Option.defaultValue "")

/// Map server TestSummary JSON to VscTestSummary
let parseSummary (data: obj) : VscTestSummary =
  { Total = data?Total |> unbox<int>
    Passed = data?Passed |> unbox<int>
    Failed = data?Failed |> unbox<int>
    Running = data?Running |> unbox<int>
    Stale = data?Stale |> unbox<int>
    Disabled = data?Disabled |> unbox<int> }

/// Map a server TestStatusEntry to VscTestResult
let parseTestResult (entry: obj) : VscTestResult =
  let id = entry?TestId |> parseTestId |> VscTestId.create
  let status = entry?Status
  let statusCase = parseDuCase status |> Option.defaultValue "Detected"
  let fields = duFields status
  let outcome =
    match statusCase with
    | "Passed" -> VscTestOutcome.Passed
    | "Failed" ->
      let msg =
        fields
        |> Option.bind (fun f ->
          match f.Length with
          | 0 -> None
          | _ -> Some f.[0])
        |> Option.bind (fun failObj -> duFirstField<string> failObj)
        |> Option.defaultValue "test failed"
      VscTestOutcome.Failed msg
    | "Skipped" ->
      let reason = fields |> Option.bind (fun f -> Some (f.[0] |> unbox<string>)) |> Option.defaultValue "skipped"
      VscTestOutcome.Skipped reason
    | "Running" -> VscTestOutcome.Running
    | "Stale" -> VscTestOutcome.Stale
    | "PolicyDisabled" -> VscTestOutcome.PolicyDisabled
    | _ -> VscTestOutcome.Skipped "unknown status"
  let durationMs =
    match statusCase, fields with
    | "Passed", Some f when f.Length > 0 ->
      f.[0] |> unbox<string> |> parseDuration
    | "Failed", Some f when f.Length >= 2 ->
      f.[1] |> unbox<string> |> parseDuration
    | _ -> None
  { Id = id; Outcome = outcome; DurationMs = durationMs; Output = None }

/// Map a server TestStatusEntry to VscTestInfo
let parseTestInfo (entry: obj) : VscTestInfo =
  let testIdStr = entry?TestId |> parseTestId
  let origin = entry?Origin
  let filePath, line =
    match parseDuCase origin with
    | Some "SourceMapped" ->
      let fields = duFields origin |> Option.defaultValue [||]
      match fields.Length >= 2 with
      | true -> Some(fields.[0] |> unbox<string>), Some(fields.[1] |> unbox<int>)
      | false -> None, None
    | _ -> None, None
  { Id = VscTestId.create testIdStr
    DisplayName = entry?DisplayName |> unbox<string>
    FullName = entry?FullName |> unbox<string>
    FilePath = filePath
    Line = line }

/// Parse Freshness DU from server JSON (Case/Fields or plain string)
let parseFreshness (data: obj) : VscResultFreshness =
  match parseDuCase (data?Freshness) with
  | Some "StaleCodeEdited" -> VscResultFreshness.StaleCodeEdited
  | Some "StaleWrongGeneration" -> VscResultFreshness.StaleWrongGeneration
  | _ -> VscResultFreshness.Fresh

/// Parse test_results_batch → VscLiveTestEvent pair (discovery + results)
let parseResultsBatch (data: obj) : VscLiveTestEvent list =
  let entries = data?Entries
  tryOfObj entries
  |> Option.map (fun entries ->
    let freshness = parseFreshness data
    let entryArray : obj array = entries |> unbox
    let testInfos = entryArray |> Array.map parseTestInfo
    let testResults = entryArray |> Array.map parseTestResult
    [ VscLiveTestEvent.TestsDiscovered testInfos
      VscLiveTestEvent.TestResultBatch (testResults, freshness) ])
  |> Option.defaultValue []

// ── Listener lifecycle ───────────────────────────────────────

type LiveTestingCallbacks = {
  OnStateChange: VscStateChange list -> unit
  OnSummaryUpdate: VscTestSummary -> unit
  OnStatusRefresh: unit -> unit
  OnBindingsUpdate: obj array -> unit
  OnPipelineTraceUpdate: obj -> unit
}

type LiveTestingListener = {
  State: unit -> VscLiveTestState
  Summary: unit -> VscTestSummary
  Bindings: unit -> obj array
  PipelineTrace: unit -> obj option
  Dispose: unit -> unit
}

let start (port: int) (callbacks: LiveTestingCallbacks) : LiveTestingListener =
  let mutable state = VscLiveTestState.empty
  let mutable bindings: obj array = [||]
  let mutable pipelineTrace: obj option = None
  let url = sprintf "http://localhost:%d/events" port

  let processEvent (eventType: string) (data: obj) =
    match eventType with
    | "test_summary" ->
      let summary = parseSummary data
      callbacks.OnSummaryUpdate summary
    | "test_results_batch" ->
      let events = parseResultsBatch data
      let mutable allChanges = []
      for evt in events do
        let newState, changes = VscLiveTestState.update evt state
        state <- newState
        allChanges <- allChanges @ changes
      match allChanges.IsEmpty with
      | false -> callbacks.OnStateChange allChanges
      | true -> ()
    | "state" ->
      callbacks.OnStatusRefresh ()
    | "session" ->
      ()
    | "bindings_snapshot" ->
      tryOfObj data?Bindings
      |> Option.iter (fun arr ->
        bindings <- arr |> unbox
        callbacks.OnBindingsUpdate bindings)
    | "pipeline_trace" ->
      pipelineTrace <- Some data
      callbacks.OnPipelineTraceUpdate data
    | _ ->
      ()

  let disposable = subscribeTypedSse url processEvent

  { State = fun () -> state
    Summary = fun () -> VscLiveTestState.summary state
    Bindings = fun () -> bindings
    PipelineTrace = fun () -> pipelineTrace
    Dispose = fun () -> disposable.dispose () |> ignore }
