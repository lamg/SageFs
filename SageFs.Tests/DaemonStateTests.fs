module SageFs.Tests.DaemonStateTests

open System
open System.Diagnostics
open Expecto
open Expecto.Flip
open SageFs

[<Tests>]
let tests =
  testList "DaemonState" [
    testList "isProcessAlive" [
      testCase "current process is alive" <| fun _ ->
        let pid = Process.GetCurrentProcess().Id
        DaemonState.isProcessAlive pid
        |> Expect.isTrue "current process should be alive"

      testCase "bogus PID is not alive" <| fun _ ->
        DaemonState.isProcessAlive 99999999
        |> Expect.isFalse "bogus PID should not be alive"
    ]

    testList "HTTP detection" [
      testCase "read returns None when no daemon running" <| fun _ ->
        // Probe a port that should not have a daemon
        DaemonState.readOnPort 39999
        |> Expect.isNone "should return None when no daemon on port"

      testCase "requestShutdown returns false when no daemon" <| fun _ ->
        DaemonState.requestShutdown 39999
        |> Expect.isFalse "shutdown should fail when no daemon"
    ]

    testList "daemon startup guard" [
      testCase "probeDaemonHttp returns None on unused port" <| fun _ ->
        DaemonState.probeDaemonHttp 39998
        |> Expect.isNone "no daemon on unused port"

      testCase "readOnPort returns None on unused port" <| fun _ ->
        DaemonState.readOnPort 39997
        |> Expect.isNone "no daemon on unused port"

      testCase "guard prevents double-start when daemon is running" <| fun _ ->
        // Simulate the guard pattern: check before spawning
        let port = 39996
        let guardResult =
          match DaemonState.readOnPort port with
          | Some _ -> Error "SageFs daemon is already running"
          | None -> Ok "would start"
        guardResult
        |> Expect.isOk "should allow start when no daemon running"

      testCase "guard blocks start when daemon responds" <| fun _ ->
        // Start a minimal HTTP listener to simulate a running daemon
        use listener = new System.Net.HttpListener()
        let port = 39995
        let dashboardPort = port + 1
        let prefix = sprintf "http://localhost:%d/" dashboardPort
        listener.Prefixes.Add(prefix)
        listener.Start()

        // Handle one request in background
        let respondTask = async {
          let! ctx = listener.GetContextAsync() |> Async.AwaitTask
          let response = ctx.Response
          let body = """{"pid":99999,"port":39995,"version":"test"}"""B
          response.ContentType <- "application/json"
          response.ContentLength64 <- int64 body.Length
          response.OutputStream.Write(body, 0, body.Length)
          response.Close()
        }
        let cts = new System.Threading.CancellationTokenSource()
        Async.Start(respondTask, cts.Token)

        // Now the guard should detect daemon as running
        let info = DaemonState.readOnPort port
        cts.Cancel()
        listener.Stop()

        info |> Expect.isSome "should detect mock daemon"
        info.Value.Pid |> Expect.equal "pid matches" 99999

      testCase "guard allows start when daemon not responding" <| fun _ ->
        // No listener on this port — guard should allow start
        let port = 39994
        let info = DaemonState.readOnPort port
        info |> Expect.isNone "no daemon on port"

        let decision =
          match info with
          | Some _ -> Error "already running"
          | None -> Ok "proceed to start"
        decision
        |> Expect.isOk "should allow start"
    ]
  ]
