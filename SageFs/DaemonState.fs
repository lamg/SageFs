namespace SageFs.Server

/// Typed state-change events for SSE subscribers.
/// Replaces stringly-typed JSON routing — compiler catches missing handlers.
type DaemonStateChange =
  | StandbyProgress
  | SessionReady of sessionId: string
  | HotReloadChanged
  | ModelChanged of json: string

// DaemonInfo and DaemonState are now in SageFs namespace (SageFs.Core).
// This module re-exports functions so existing code using SageFs.Server.DaemonState compiles.
module DaemonState =
  let SageFsDir = SageFs.DaemonState.SageFsDir
  let isProcessAlive = SageFs.DaemonState.isProcessAlive
  let read = SageFs.DaemonState.read
  let readOnPort = SageFs.DaemonState.readOnPort
  let requestShutdown = SageFs.DaemonState.requestShutdown
