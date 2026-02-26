module SageFs.Tests.InstrumentationTests

open System.Diagnostics
open Expecto
open Expecto.Flip
open SageFs

// Tests use ActivityListeners that share global ActivitySources,
// so they must run sequentially to avoid cross-test interference.
[<Tests>]
let instrumentationTests = testSequenced (testList "Instrumentation" [

  test "SessionManager source name" {
    Instrumentation.sessionSource.Name
    |> Expect.equal "name" "SageFs.SessionManager"
  }
  test "Pipeline source name" {
    Instrumentation.pipelineSource.Name
    |> Expect.equal "name" "SageFs.Pipeline"
  }
  test "SessionManager meter name" {
    Instrumentation.sessionMeter.Name
    |> Expect.equal "name" "SageFs.SessionManager"
  }
  test "Pipeline meter name" {
    Instrumentation.pipelineMeter.Name
    |> Expect.equal "name" "SageFs.Pipeline"
  }
  test "counters are not null" {
    Instrumentation.sessionsCreated |> Expect.isNotNull "sessionsCreated"
    Instrumentation.sessionsStopped |> Expect.isNotNull "sessionsStopped"
    Instrumentation.sessionsRestarted |> Expect.isNotNull "sessionsRestarted"
    Instrumentation.standbySwaps |> Expect.isNotNull "standbySwaps"
    Instrumentation.coldRestarts |> Expect.isNotNull "coldRestarts"
    Instrumentation.activeSessions |> Expect.isNotNull "activeSessions"
  }
  test "histogram is not null" {
    Instrumentation.pipelineEndToEnd |> Expect.isNotNull "pipelineEndToEnd"
  }
  test "allSources has expected entries" {
    Instrumentation.allSources |> Expect.hasLength "4 sources" 4
    Instrumentation.allSources |> Expect.contains "has SessionManager" "SageFs.SessionManager"
    Instrumentation.allSources |> Expect.contains "has Pipeline" "SageFs.Pipeline"
    Instrumentation.allSources |> Expect.contains "has LiveTesting" "SageFs.LiveTesting"
    Instrumentation.allSources |> Expect.contains "has Marten" "Marten"
  }
  test "allMeters has expected entries" {
    Instrumentation.allMeters |> Expect.hasLength "4 meters" 4
  }

  test "traced returns correct value" {
    Instrumentation.traced
      Instrumentation.sessionSource "test.op" []
      (fun () -> 42)
    |> Expect.equal "return value" 42
  }

  test "traced preserves exceptions" {
    Expect.throwsT<System.InvalidOperationException>
      "should rethrow"
      (fun () ->
        Instrumentation.traced
          Instrumentation.sessionSource "test.fail" []
          (fun () -> raise (System.InvalidOperationException "boom"))
        |> ignore)
  }

  test "traced emits activity with tags to listener" {
    let mutable capturedTagKeys : string list = []
    let mutable capturedName = ""
    let listener = new ActivityListener()
    listener.ShouldListenTo <- fun src -> src.Name = "SageFs.SessionManager"
    listener.Sample <- fun _ -> ActivitySamplingResult.AllDataAndRecorded
    listener.ActivityStopped <- fun a ->
      capturedName <- a.DisplayName
      capturedTagKeys <- [ for t in a.TagObjects -> t.Key ]
    ActivitySource.AddActivityListener(listener)

    Instrumentation.traced
      Instrumentation.sessionSource
      "session.create"
      [("session.id", box "abc123")]
      (fun () -> "ok") |> ignore

    listener.Dispose()

    capturedName |> Expect.equal "activity name" "session.create"
    capturedTagKeys |> Expect.contains "has session.id" "session.id"
    capturedTagKeys |> Expect.contains "has duration_ms" "duration_ms"
  }

  test "tracedAsync returns correct value" {
    Instrumentation.tracedAsync
      Instrumentation.pipelineSource "test.async" []
      (fun () -> async { return 99 })
    |> Async.RunSynchronously
    |> Expect.equal "return value" 99
  }

  test "tracedAsync preserves exceptions" {
    Expect.throwsT<System.InvalidOperationException>
      "should rethrow"
      (fun () ->
        Instrumentation.tracedAsync
          Instrumentation.pipelineSource "test.fail" []
          (fun () -> async { return raise (System.InvalidOperationException "boom") })
        |> Async.RunSynchronously
        |> ignore)
  }

  // Span helper tests
  test "startSpan returns null when no listener" {
    let span = Instrumentation.startSpan Instrumentation.sessionSource "test.op" []
    span |> Expect.isNull "should be null with no listener"
  }

  test "succeedSpan handles null activity" {
    Instrumentation.succeedSpan null
  }

  test "failSpan handles null activity" {
    Instrumentation.failSpan null "some error"
  }

  test "startSpan creates activity with tags when listener attached" {
    let mutable captured : Activity option = None
    use listener = new ActivityListener(
      ShouldListenTo = (fun s -> s.Name = "SageFs.SessionManager"),
      Sample = (fun _ -> ActivitySamplingResult.AllData),
      ActivityStopped = (fun a -> captured <- Some a)
    )
    ActivitySource.AddActivityListener(listener)

    let span = Instrumentation.startSpan Instrumentation.sessionSource "test.create" [("session.id", "abc123" :> obj); ("rebuild", true :> obj)]
    span |> Expect.isNotNull "should create activity"
    Instrumentation.succeedSpan span

    captured |> Expect.isSome "should have captured stopped activity"
    let a = captured.Value
    a.OperationName |> Expect.equal "operation name" "test.create"
    let idTag = a.TagObjects |> Seq.tryFind (fun kv -> kv.Key = "session.id")
    idTag |> Expect.isSome "should have session.id tag"
    idTag.Value.Value |> Expect.equal "session.id value" ("abc123" :> obj)
  }

  test "failSpan sets error status and tags" {
    let mutable captured : Activity option = None
    use listener = new ActivityListener(
      ShouldListenTo = (fun s -> s.Name = "SageFs.SessionManager"),
      Sample = (fun _ -> ActivitySamplingResult.AllData),
      ActivityStopped = (fun a -> captured <- Some a)
    )
    ActivitySource.AddActivityListener(listener)

    let span = Instrumentation.startSpan Instrumentation.sessionSource "test.fail" []
    Instrumentation.failSpan span "something went wrong"

    captured |> Expect.isSome "should have captured"
    let a = captured.Value
    a.Status |> Expect.equal "should be error status" ActivityStatusCode.Error
    let errTag = a.TagObjects |> Seq.tryFind (fun kv -> kv.Key = "error")
    errTag |> Expect.isSome "should have error tag"
    errTag.Value.Value |> Expect.equal "error value" (true :> obj)
    let msgTag = a.TagObjects |> Seq.tryFind (fun kv -> kv.Key = "error.message")
    msgTag |> Expect.isSome "should have error.message tag"
    msgTag.Value.Value |> Expect.equal "error.message value" ("something went wrong" :> obj)
  }

  test "tracedAsync emits activity with tags to listener" {
    let mutable capturedTagKeys : string list = []
    let mutable capturedName = ""
    let listener = new ActivityListener()
    listener.ShouldListenTo <- fun src -> src.Name = "SageFs.Pipeline"
    listener.Sample <- fun _ -> ActivitySamplingResult.AllDataAndRecorded
    listener.ActivityStopped <- fun a ->
      capturedName <- a.DisplayName
      capturedTagKeys <- [ for t in a.TagObjects -> t.Key ]
    ActivitySource.AddActivityListener(listener)

    Instrumentation.tracedAsync
      Instrumentation.pipelineSource
      "pipeline.run"
      [("trigger", box "file_change")]
      (fun () -> async { return 42 })
    |> Async.RunSynchronously |> ignore

    listener.Dispose()

    capturedName |> Expect.equal "activity name" "pipeline.run"
    capturedTagKeys |> Expect.contains "has trigger" "trigger"
    capturedTagKeys |> Expect.contains "has duration_ms" "duration_ms"
  }
])
