namespace SageFs

open System.Diagnostics
open System.Diagnostics.Metrics

/// Centralized OTel instrumentation for SageFs.
/// SessionManager: session lifecycle spans and counters.
/// Pipeline: end-to-end file-change → test-result timing.
/// When no collector is attached, StartActivity returns null (~50ns no-op).
module Instrumentation =

  let sessionSource = new ActivitySource("SageFs.SessionManager")
  let pipelineSource = new ActivitySource("SageFs.Pipeline")

  let sessionMeter = new Meter("SageFs.SessionManager")
  let pipelineMeter = new Meter("SageFs.Pipeline")

  let sessionsCreated =
    sessionMeter.CreateCounter<int64>("sagefs.sessions.created_total", description = "Total sessions created")
  let sessionsStopped =
    sessionMeter.CreateCounter<int64>("sagefs.sessions.stopped_total", description = "Total sessions stopped")
  let sessionsRestarted =
    sessionMeter.CreateCounter<int64>("sagefs.sessions.restarted_total", description = "Total session restarts")
  let standbySwaps =
    sessionMeter.CreateCounter<int64>("sagefs.sessions.standby_swaps_total", description = "Restarts using standby pool")
  let coldRestarts =
    sessionMeter.CreateCounter<int64>("sagefs.sessions.cold_restarts_total", description = "Restarts without standby")
  let activeSessions =
    sessionMeter.CreateUpDownCounter<int64>("sagefs.sessions.active", description = "Currently active sessions")

  let pipelineEndToEnd =
    pipelineMeter.CreateHistogram<float>("sagefs.pipeline.end_to_end_ms", unit = "ms", description = "Pipeline end-to-end latency")

  /// All ActivitySource names for OTel registration in McpServer.
  let allSources =
    [ "SageFs.SessionManager"
      "SageFs.Pipeline"
      "SageFs.LiveTesting"
      "Marten" ]

  /// All Meter names for OTel registration in McpServer.
  let allMeters =
    [ "SageFs.SessionManager"
      "SageFs.Pipeline"
      "SageFs.LiveTesting"
      "Marten" ]

  /// Start an Activity with initial tags. Returns null when no listener attached.
  let startSpan (source: ActivitySource) (name: string) (tags: (string * obj) list) =
    let activity = source.StartActivity(name)
    if not (isNull activity) then
      for (k, v) in tags do
        activity.SetTag(k, v) |> ignore
    activity

  /// Stop an activity with success status.
  let succeedSpan (activity: Activity) =
    if not (isNull activity) then
      activity.Stop()
      activity.Dispose()

  /// Stop an activity with error status and message.
  let failSpan (activity: Activity) (message: string) =
    if not (isNull activity) then
      activity.SetTag("error", true) |> ignore
      activity.SetTag("error.message", message) |> ignore
      activity.SetStatus(ActivityStatusCode.Error, message) |> ignore
      activity.Stop()
      activity.Dispose()

  /// Wrap a synchronous operation with Activity tracing.
  /// Tags are set on the activity before it is stopped.
  /// Uses explicit Stop/Dispose for reliable ActivityStopped callbacks.
  let traced (source: ActivitySource) (name: string) (tags: (string * obj) list) (f: unit -> 'a) =
    let sw = Stopwatch.StartNew()
    let activity = source.StartActivity(name)
    try
      let result = f ()
      sw.Stop()
      if not (isNull activity) then
        for (k, v) in tags do
          activity.SetTag(k, v) |> ignore
        activity.SetTag("duration_ms", sw.Elapsed.TotalMilliseconds) |> ignore
        activity.Stop()
        activity.Dispose()
      result
    with ex ->
      sw.Stop()
      if not (isNull activity) then
        activity.SetTag("error", true) |> ignore
        activity.SetTag("error.type", ex.GetType().Name) |> ignore
        activity.SetTag("error.message", ex.Message) |> ignore
        activity.SetTag("duration_ms", sw.Elapsed.TotalMilliseconds) |> ignore
        activity.SetStatus(ActivityStatusCode.Error, ex.Message) |> ignore
        activity.Stop()
        activity.Dispose()
      raise ex

  /// Wrap an async operation with Activity tracing.
  let tracedAsync (source: ActivitySource) (name: string) (tags: (string * obj) list) (f: unit -> Async<'a>) =
    async {
      let sw = Stopwatch.StartNew()
      let activity = source.StartActivity(name)
      try
        let! result = f ()
        sw.Stop()
        if not (isNull activity) then
          for (k, v) in tags do
            activity.SetTag(k, v) |> ignore
          activity.SetTag("duration_ms", sw.Elapsed.TotalMilliseconds) |> ignore
          activity.Stop()
          activity.Dispose()
        return result
      with ex ->
        sw.Stop()
        if not (isNull activity) then
          activity.SetTag("error", true) |> ignore
          activity.SetTag("error.type", ex.GetType().Name) |> ignore
          activity.SetTag("error.message", ex.Message) |> ignore
          activity.SetTag("duration_ms", sw.Elapsed.TotalMilliseconds) |> ignore
          activity.SetStatus(ActivityStatusCode.Error, ex.Message) |> ignore
          activity.Stop()
          activity.Dispose()
        return raise ex
    }
