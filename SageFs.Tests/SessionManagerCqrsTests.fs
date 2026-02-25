module SageFs.Tests.SessionManagerCqrsTests

open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics
open Expecto
open Expecto.Flip
open SageFs
open SageFs.WorkerProtocol
open SageFs.SessionManager

// ═══════════════════════════════════════════════════════════════
// CQRS Read/Write Separation Tests for SessionManager
//
// Problem: SessionManager uses a single MailboxProcessor for
// ALL operations. During slow commands (dotnet build ~30s),
// ALL reads queue behind the write — SSE pushes hang, dashboard
// appears frozen.
//
// Fix: CQRS — publish an immutable QuerySnapshot after each
// command. Reads go to the snapshot (lock-free, instant).
// Writes stay on the mailbox (sequential, consistent).
// ═══════════════════════════════════════════════════════════════

// ── Test helpers ──────────────────────────────────────────────

let private mkSessionInfo id status =
  {
    Id = id
    Name = None
    Projects = [ "Test.fsproj" ]
    WorkingDirectory = "/test"
    SolutionRoot = None
    CreatedAt = DateTime.MinValue
    LastActivity = DateTime.MinValue
    Status = status
    WorkerPid = None
  }

let private mkManagedSession id status =
  let proxy : SessionProxy =
    fun _ -> async {
      return WorkerResponse.WorkerError (SageFsError.WorkerSpawnFailed "test")
    }
  {
    Info = mkSessionInfo id status
    Process = Process.GetCurrentProcess()
    Proxy = proxy
    WorkerBaseUrl = "http://localhost:0"
    Projects = [ "Test.fsproj" ]
    WorkingDir = "/test"
    RestartState = RestartPolicy.emptyState
  }

// ── Simulated actor for pattern testing ──────────────────────

type private SimCommand =
  | SlowWrite of id: string * AsyncReplyChannel<string>
  | MailboxRead of id: string * AsyncReplyChannel<string option>

type private SimState = { Items: Map<string, string> }

let private createSimActor (slowMs: int) (initial: SimState) =
  let snapshotRef = ref initial
  let agent = MailboxProcessor<SimCommand>.Start(fun inbox ->
    let rec loop (state: SimState) = async {
      let! cmd = inbox.Receive()
      match cmd with
      | SlowWrite(id, reply) ->
        do! Async.Sleep slowMs
        let newState = { Items = state.Items |> Map.add id "written" }
        snapshotRef.Value <- newState
        reply.Reply "ok"
        return! loop newState
      | MailboxRead(id, reply) ->
        reply.Reply(state.Items |> Map.tryFind id)
        return! loop state
    }
    loop initial)
  let mailboxRead id =
    agent.PostAndAsyncReply(fun reply -> MailboxRead(id, reply))
    |> Async.StartAsTask
  let snapshotRead id =
    snapshotRef.Value.Items |> Map.tryFind id
  agent, mailboxRead, snapshotRead

// ── CQRS pattern tests (isolated simulation) ────────────────

let cqrsPatternTests = testList "CQRS pattern" [

  test "mailbox read blocks behind slow write (demonstrates the problem)" {
    let initial = { Items = Map.ofList [ "s1", "ready" ] }
    let agent, mailboxRead, _snapshotRead = createSimActor 3000 initial

    let _slowTask =
      agent.PostAndAsyncReply(fun reply -> SlowWrite("s2", reply))
      |> Async.StartAsTask

    Thread.Sleep(50)

    let readTask = mailboxRead "s1"
    let completed = readTask.Wait(500)

    completed
    |> Expect.isFalse "mailbox read should be blocked behind slow write"
  }

  test "snapshot read completes instantly during slow write" {
    let initial = { Items = Map.ofList [ "s1", "ready" ] }
    let agent, _mailboxRead, snapshotRead = createSimActor 3000 initial

    let _slowTask =
      agent.PostAndAsyncReply(fun reply -> SlowWrite("s2", reply))
      |> Async.StartAsTask

    Thread.Sleep(50)

    let sw = Stopwatch.StartNew()
    let result = snapshotRead "s1"
    sw.Stop()

    result |> Expect.equal "should find item in snapshot" (Some "ready")
    (sw.ElapsedMilliseconds, 10L)
    |> Expect.isLessThan "snapshot read should be < 10ms"
  }

  test "snapshot eventually updates after write completes" {
    let initial = { Items = Map.ofList [ "s1", "ready" ] }
    let agent, _mailboxRead, snapshotRead = createSimActor 100 initial

    agent.PostAndAsyncReply(fun reply -> SlowWrite("s2", reply))
    |> Async.RunSynchronously
    |> ignore

    snapshotRead "s2"
    |> Expect.equal "new item visible after write" (Some "written")
  }

  test "snapshot is stale during write but returns old data" {
    let initial = { Items = Map.ofList [ "x", "original" ] }
    let agent, _mailboxRead, snapshotRead = createSimActor 3000 initial

    let _slowTask =
      agent.PostAndAsyncReply(fun reply -> SlowWrite("y", reply))
      |> Async.StartAsTask

    Thread.Sleep(50)

    snapshotRead "x"
    |> Expect.equal "existing item visible" (Some "original")

    snapshotRead "y"
    |> Expect.equal "new item not yet visible" None
  }

  test "100 concurrent snapshot reads complete instantly" {
    let items = [ for i in 1..100 -> sprintf "s%d" i, sprintf "val%d" i ] |> Map.ofList
    let initial = { Items = items }
    let agent, _mailboxRead, snapshotRead = createSimActor 5000 initial

    let _slowTask =
      agent.PostAndAsyncReply(fun reply -> SlowWrite("new", reply))
      |> Async.StartAsTask

    Thread.Sleep(50)

    let sw = Stopwatch.StartNew()
    let results =
      [| for i in 1..100 ->
          Task.Run(fun () -> snapshotRead (sprintf "s%d" i)) |]
      |> Task.WhenAll
      |> fun t -> t.Result
    sw.Stop()

    results |> Array.iter (fun r -> r |> Expect.isSome "should find item")
    (sw.ElapsedMilliseconds, 100L)
    |> Expect.isLessThan "100 concurrent reads in < 100ms"
  }
]

// ── QuerySnapshot projection tests ──────────────────────────

let querySnapshotTests = testList "QuerySnapshot projection" [

  test "fromState projects empty state" {
    let snap = QuerySnapshot.fromState ManagerState.empty StandbyInfo.NoPool
    snap.Sessions |> Expect.isEmpty "empty state has no sessions"
    snap.StandbyInfo |> Expect.equal "standby is NoPool" StandbyInfo.NoPool
  }

  test "fromState projects multiple sessions" {
    let s1 = mkManagedSession "a" SessionStatus.Ready
    let s2 = mkManagedSession "b" SessionStatus.Starting
    let state =
      ManagerState.empty
      |> ManagerState.addSession "a" s1
      |> ManagerState.addSession "b" s2
    let snap = QuerySnapshot.fromState state (StandbyInfo.Warming "building")

    snap.Sessions |> Map.count |> Expect.equal "two sessions" 2
    snap.StandbyInfo |> Expect.equal "standby is Warming" (StandbyInfo.Warming "building")
  }

  test "tryGetSession returns existing session info" {
    let s = mkManagedSession "x" SessionStatus.Ready
    let state = ManagerState.empty |> ManagerState.addSession "x" s
    let snap = QuerySnapshot.fromState state StandbyInfo.NoPool

    let result = QuerySnapshot.tryGetSession "x" snap
    result |> Expect.isSome "should find session"
    (result |> Option.get).Status
    |> Expect.equal "status is Ready" SessionStatus.Ready
  }

  test "tryGetSession returns None for missing session" {
    let snap = QuerySnapshot.fromState ManagerState.empty StandbyInfo.NoPool
    QuerySnapshot.tryGetSession "nonexistent" snap
    |> Expect.isNone "missing session returns None"
  }

  test "allSessions returns all session infos" {
    let s1 = mkManagedSession "a" SessionStatus.Ready
    let s2 = mkManagedSession "b" SessionStatus.Starting
    let state =
      ManagerState.empty
      |> ManagerState.addSession "a" s1
      |> ManagerState.addSession "b" s2
    let snap = QuerySnapshot.fromState state StandbyInfo.NoPool

    let all = QuerySnapshot.allSessions snap
    all |> List.length |> Expect.equal "two sessions" 2
  }

  test "snapshot is immutable — adding to state doesn't affect existing snapshot" {
    let s1 = mkManagedSession "a" SessionStatus.Ready
    let state1 = ManagerState.empty |> ManagerState.addSession "a" s1
    let snap1 = QuerySnapshot.fromState state1 StandbyInfo.NoPool

    // Add another session to state
    let s2 = mkManagedSession "b" SessionStatus.Starting
    let state2 = state1 |> ManagerState.addSession "b" s2
    let snap2 = QuerySnapshot.fromState state2 StandbyInfo.NoPool

    // snap1 should NOT see session "b"
    QuerySnapshot.tryGetSession "b" snap1
    |> Expect.isNone "old snapshot doesn't see new session"

    // snap2 should see both
    snap2.Sessions |> Map.count |> Expect.equal "new snapshot sees both" 2
  }

  test "snapshot reflects removal in new snapshot" {
    let s1 = mkManagedSession "a" SessionStatus.Ready
    let s2 = mkManagedSession "b" SessionStatus.Starting
    let state =
      ManagerState.empty
      |> ManagerState.addSession "a" s1
      |> ManagerState.addSession "b" s2
    let snap1 = QuerySnapshot.fromState state StandbyInfo.NoPool

    let state2 = state |> ManagerState.removeSession "a"
    let snap2 = QuerySnapshot.fromState state2 StandbyInfo.NoPool

    snap1.Sessions |> Map.count |> Expect.equal "old snapshot still has 2" 2
    snap2.Sessions |> Map.count |> Expect.equal "new snapshot has 1" 1
    QuerySnapshot.tryGetSession "a" snap2
    |> Expect.isNone "removed session not in new snapshot"
  }
]

// ── Combined test list ───────────────────────────────────────

[<Tests>]
let allCqrsTests = testList "SessionManager CQRS" [
  cqrsPatternTests
  querySnapshotTests
]
