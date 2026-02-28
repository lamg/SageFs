module SageFs.Vscode.SageFsClient

open Fable.Core
open Fable.Core.JsInterop

[<Emit("console.warn('[SageFs]', $0, $1)")>]
let logWarn (context: string) (err: obj) : unit = jsNative

/// Command result: either succeeded with optional message, or failed with error.
/// Replaces the old { success; result; error } bag-of-optionals — illegal states now unrepresentable.
type ApiOutcome =
  | Succeeded of message: string option
  | Failed of error: string

module ApiOutcome =
  let message = function Succeeded m -> m | Failed _ -> None
  let error = function Failed e -> Some e | Succeeded _ -> None
  let isOk = function Succeeded _ -> true | Failed _ -> false
  let messageOrDefault fallback = function Succeeded (Some m) -> m | Succeeded None -> fallback | Failed e -> e

type SageFsStatus =
  { connected: bool
    healthy: bool option
    status: string option }

type SystemStatus =
  { supervised: bool
    restartCount: int
    version: string }

type HotReloadFile =
  { path: string
    watched: bool }

type HotReloadState =
  { files: HotReloadFile array
    watchedCount: int }

type SessionInfo =
  { id: string
    name: string option
    workingDirectory: string
    status: string
    projects: string array
    evalCount: int }

type LoadedAssemblyInfo =
  { Name: string
    Path: string
    NamespaceCount: int
    ModuleCount: int }

type OpenedBindingInfo =
  { Name: string
    IsModule: bool
    Source: string }

type WarmupContextInfo =
  { SourceFilesScanned: int
    AssembliesLoaded: LoadedAssemblyInfo array
    NamespacesOpened: OpenedBindingInfo array
    FailedOpens: string array array
    WarmupDurationMs: int }

[<Emit("new Promise((resolve, reject) => { const http = require('http'); const req = http.get($0, { timeout: $1 }, (res) => { let data = ''; res.on('data', (chunk) => data += chunk); res.on('end', () => resolve({ statusCode: res.statusCode || 0, body: data })); }); req.on('error', reject); req.on('timeout', () => { req.destroy(); reject(new Error('timeout')); }); })")>]
let httpGetRaw (url: string) (timeout: int) : JS.Promise<{| statusCode: int; body: string |}> = jsNative

[<Emit("new Promise((resolve, reject) => { const http = require('http'); const url = new URL($0); const req = http.request({ hostname: url.hostname, port: url.port, path: url.pathname, method: 'POST', headers: { 'Content-Type': 'application/json' }, timeout: $2 }, (res) => { let data = ''; res.on('data', (chunk) => data += chunk); res.on('end', () => resolve({ statusCode: res.statusCode || 0, body: data })); }); req.on('error', reject); req.on('timeout', () => { req.destroy(); reject(new Error('timeout')); }); req.write($1); req.end(); })")>]
let httpPostRaw (url: string) (body: string) (timeout: int) : JS.Promise<{| statusCode: int; body: string |}> = jsNative

[<Emit("JSON.parse($0)")>]
let jsonParse (s: string) : obj = jsNative

[<Emit("JSON.stringify($0)")>]
let jsonStringify (o: obj) : string = jsNative

type Client =
  { mutable mcpPort: int
    mutable dashboardPort: int }

let create (mcpPort: int) (dashboardPort: int) =
  { mcpPort = mcpPort; dashboardPort = dashboardPort }

let baseUrl (c: Client) = sprintf "http://localhost:%d" c.mcpPort
let dashboardUrl (c: Client) = sprintf "http://localhost:%d/dashboard" c.dashboardPort

let updatePorts (mcpPort: int) (dashboardPort: int) (c: Client) =
  c.mcpPort <- mcpPort
  c.dashboardPort <- dashboardPort

let httpGet (c: Client) (path: string) (timeout: int) =
  httpGetRaw (sprintf "%s%s" (baseUrl c) path) timeout

let httpPost (c: Client) (path: string) (body: string) (timeout: int) =
  httpPostRaw (sprintf "%s%s" (baseUrl c) path) body timeout

let dashHttpGet (c: Client) (path: string) (timeout: int) =
  httpGetRaw (sprintf "http://localhost:%d%s" c.dashboardPort path) timeout

let dashHttpPost (c: Client) (path: string) (body: string) (timeout: int) =
  httpPostRaw (sprintf "http://localhost:%d%s" c.dashboardPort path) body timeout

// ── JSON field helpers (null-safe boundary parsing) ──────────────────────
let tryField<'T> (name: string) (obj: obj) : 'T option =
  let v = obj?(name)
  match isNull v with
  | true -> None
  | false -> Some (unbox<'T> v)

let parseOutcome (parsed: obj) : ApiOutcome =
  let success = tryField<bool> "success" parsed |> Option.defaultValue false
  match success with
  | true ->
    tryField<string> "message" parsed
    |> Option.orElse (tryField<string> "result" parsed)
    |> Succeeded
  | false ->
    tryField<string> "error" parsed
    |> Option.defaultValue "Unknown error"
    |> Failed

/// POST a command, parse the standard { success, message/result, error } response.
let postCommand (c: Client) (path: string) (body: string) (timeout: int) : JS.Promise<ApiOutcome> =
  promise {
    try
      let! resp = httpPost c path body timeout
      return jsonParse resp.body |> parseOutcome
    with err ->
      return Failed (string err)
  }

let isRunning (c: Client) =
  promise {
    try
      let! resp = httpGet c "/health" 3000
      return resp.statusCode > 0
    with _ ->
      return false
  }

let getStatus (c: Client) =
  promise {
    try
      let! resp = httpGet c "/health" 3000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        return
          { connected = true
            healthy = tryField<bool> "healthy" parsed |> Option.orElse (Some false)
            status = tryField<string> "status" parsed }
      | _ ->
        return { connected = true; healthy = Some false; status = Some "no session" }
    with _ ->
      return { connected = false; healthy = None; status = None }
  }

let evalCode (code: string) (workingDirectory: string option) (c: Client) =
  let wd = workingDirectory |> Option.defaultValue ""
  postCommand c "/exec" (jsonStringify {| code = code; working_directory = wd |}) 30000

let resetSession (c: Client) =
  postCommand c "/reset" "{}" 15000

let hardReset (rebuild: bool) (c: Client) =
  postCommand c "/hard-reset" (jsonStringify {| rebuild = rebuild |}) 60000

let listSessions (c: Client) =
  promise {
    try
      let! resp = httpGet c "/api/sessions" 5000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        let sessions: obj array = parsed?sessions |> unbox
        return
          sessions |> Array.map (fun s ->
            { id = s?id |> unbox<string>
              name = None
              workingDirectory = s?workingDirectory |> unbox<string>
              status = s?status |> unbox<string>
              projects = tryField<string array> "projects" s |> Option.defaultValue [||]
              evalCount = tryField<int> "evalCount" s |> Option.defaultValue 0 })
      | _ ->
        return [||]
    with ex ->
      logWarn "listSessions" ex
      return [||]
  }

let createSession (projects: string) (workingDirectory: string) (c: Client) =
  postCommand c "/api/sessions/create" (jsonStringify {| projects = [| projects |]; workingDirectory = workingDirectory |}) 30000

let switchSession (sessionId: string) (c: Client) =
  promise {
    try
      let payload = {| sessionId = sessionId |}
      let! resp = httpPost c "/api/sessions/switch" (jsonStringify payload) 5000
      let parsed = jsonParse resp.body
      return parsed?success |> unbox<bool>
    with ex ->
      logWarn "switchSession" ex
      return false
  }

let stopSession (sessionId: string) (c: Client) =
  promise {
    try
      let payload = {| sessionId = sessionId |}
      let! resp = httpPost c "/api/sessions/stop" (jsonStringify payload) 10000
      let parsed = jsonParse resp.body
      return parsed?success |> unbox<bool>
    with ex ->
      logWarn "stopSession" ex
      return false
  }

let getSystemStatus (c: Client) =
  promise {
    try
      let! resp = httpGet c "/api/system/status" 3000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        return
          Some
            { supervised = parsed?supervised |> unbox<bool>
              restartCount = parsed?restartCount |> unbox<int>
              version = parsed?version |> unbox<string> }
      | _ ->
        return None
    with ex ->
      logWarn "getSystemStatus" ex
      return None
  }

let getHotReloadState (sessionId: string) (c: Client) =
  promise {
    try
      let! resp = dashHttpGet c (sprintf "/api/sessions/%s/hotreload" sessionId) 5000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        let rawFiles = parsed?files
        match isNull rawFiles with
        | true ->
          return Some { files = [||]; watchedCount = 0 }
        | false ->
          let files =
            rawFiles
            |> unbox<obj array>
            |> Array.choose (fun f ->
              tryField<string> "path" f
              |> Option.map (fun p ->
                { path = p
                  watched = tryField<bool> "watched" f |> Option.defaultValue false }))
          let wc = tryField<int> "watchedCount" parsed |> Option.defaultValue 0
          return Some { files = files; watchedCount = wc }
      | _ ->
        return None
    with ex ->
      logWarn "getHotReloadState" ex
      return None
  }

let toggleHotReload (sessionId: string) (path: string) (c: Client) =
  promise {
    try
      let! _ = dashHttpPost c (sprintf "/api/sessions/%s/hotreload/toggle" sessionId) (jsonStringify {| path = path |}) 5000
      return true
    with ex ->
      logWarn "toggleHotReload" ex
      return false
  }

let watchAllHotReload (sessionId: string) (c: Client) =
  promise {
    try
      let! _ = dashHttpPost c (sprintf "/api/sessions/%s/hotreload/watch-all" sessionId) "{}" 5000
      return true
    with ex ->
      logWarn "watchAllHotReload" ex
      return false
  }

let unwatchAllHotReload (sessionId: string) (c: Client) =
  promise {
    try
      let! _ = dashHttpPost c (sprintf "/api/sessions/%s/hotreload/unwatch-all" sessionId) "{}" 5000
      return true
    with ex ->
      logWarn "unwatchAllHotReload" ex
      return false
  }

let watchDirectoryHotReload (sessionId: string) (directory: string) (c: Client) =
  promise {
    try
      let! _ = dashHttpPost c (sprintf "/api/sessions/%s/hotreload/watch-directory" sessionId) (jsonStringify {| directory = directory |}) 5000
      return true
    with ex ->
      logWarn "watchDirectoryHotReload" ex
      return false
  }

let unwatchDirectoryHotReload (sessionId: string) (directory: string) (c: Client) =
  promise {
    try
      let! _ = dashHttpPost c (sprintf "/api/sessions/%s/hotreload/unwatch-directory" sessionId) (jsonStringify {| directory = directory |}) 5000
      return true
    with ex ->
      logWarn "unwatchDirectoryHotReload" ex
      return false
  }

let getWarmupContext (sessionId: string) (c: Client) =
  promise {
    try
      let! resp = dashHttpGet c (sprintf "/api/sessions/%s/warmup-context" sessionId) 5000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        let assemblies =
          parsed?AssembliesLoaded
          |> unbox<obj array>
          |> Array.map (fun a ->
            { Name = a?Name |> unbox<string>
              Path = a?Path |> unbox<string>
              NamespaceCount = a?NamespaceCount |> unbox<int>
              ModuleCount = a?ModuleCount |> unbox<int> })
        let opened =
          parsed?NamespacesOpened
          |> unbox<obj array>
          |> Array.map (fun b ->
            { Name = b?Name |> unbox<string>
              IsModule = b?IsModule |> unbox<bool>
              Source = b?Source |> unbox<string> })
        let failed =
          parsed?FailedOpens
          |> unbox<obj array>
          |> Array.map (fun f -> f |> unbox<string array>)
        return
          Some
            { SourceFilesScanned = parsed?SourceFilesScanned |> unbox<int>
              AssembliesLoaded = assemblies
              NamespacesOpened = opened
              FailedOpens = failed
              WarmupDurationMs = parsed?WarmupDurationMs |> unbox<int> }
      | _ ->
        return None
    with ex ->
      logWarn "getWarmupContext" ex
      return None
  }

type CompletionResult =
  { label: string
    kind: string
    insertText: string }

let getCompletions (code: string) (cursorPosition: int) (workingDirectory: string option) (c: Client) =
  promise {
    try
      let payload =
        {| code = code
           cursor_position = cursorPosition
           working_directory = workingDirectory |> Option.defaultValue "" |}
      let! resp = dashHttpPost c "/dashboard/completions" (jsonStringify payload) 10000
      match resp.statusCode with
      | 200 ->
        let parsed = jsonParse resp.body
        let items = parsed?completions |> unbox<obj array>
        return
          items
          |> Array.map (fun item ->
            { label = item?label |> unbox<string>
              kind = item?kind |> unbox<string>
              insertText = item?insertText |> unbox<string> })
      | _ ->
        return [||]
    with ex ->
      logWarn "getCompletions" ex
      return [||]
  }

let runTests (pattern: string) (c: Client) =
  postCommand c "/api/live-testing/run" (jsonStringify {| pattern = pattern; category = "" |}) 60000

let enableLiveTesting (c: Client) =
  postCommand c "/api/live-testing/enable" "{}" 5000

let disableLiveTesting (c: Client) =
  postCommand c "/api/live-testing/disable" "{}" 5000

let setRunPolicy (category: string) (policy: string) (c: Client) =
  postCommand c "/api/live-testing/policy" (jsonStringify {| category = category; policy = policy |}) 5000

let explore(name: string) (c: Client) =
  promise {
    try
      let! resp = httpPost c "/api/explore" (jsonStringify {| name = name |}) 10000
      match resp.statusCode with
      | 200 -> return Some resp.body
      | _ -> return None
    with ex ->
      logWarn "explore" ex
      return None
  }

let getRecentEvents (count: int) (c: Client) =
  promise {
    try
      let! resp = httpGet c (sprintf "/api/recent-events?count=%d" count) 10000
      match resp.statusCode with
      | 200 -> return Some resp.body
      | _ -> return None
    with ex ->
      logWarn "getRecentEvents" ex
      return None
  }

let getDependencyGraph (symbol: string) (c: Client) =
  promise {
    try
      let path =
        match symbol with
        | "" -> "/api/dependency-graph"
        | s -> sprintf "/api/dependency-graph?symbol=%s" (JS.encodeURIComponent s)
      let! resp = httpGet c path 10000
      match resp.statusCode with
      | 200 -> return Some resp.body
      | _ -> return None
    with ex ->
      logWarn "getDependencyGraph" ex
      return None
  }

let cancelEval (c: Client) =
  postCommand c "/api/cancel-eval" "{}" 5000

let loadScript (filePath: string) (c: Client) =
  let code = sprintf "#load @\"%s\";;" filePath
  postCommand c "/exec" (jsonStringify {| code = code; working_directory = "" |}) 30000

let getPipelineTrace (c: Client) =
  promise {
    try
      let! resp = httpGet c "/api/live-testing/pipeline-trace" 5000
      match resp.statusCode with
      | 200 -> return Some resp.body
      | _ -> return None
    with ex ->
      logWarn "getPipelineTrace" ex
      return None
  }

type ExportResult =
  { content: string
    evalCount: int }

let exportSessionAsFsx (sessionId: string) (c: Client) =
  promise {
    try
      let! resp = httpGet c (sprintf "/api/sessions/%s/export-fsx" (JS.encodeURIComponent sessionId)) 15000
      match resp.statusCode with
      | 200 ->
        let parsed: ExportResult = !!JS.JSON.parse(resp.body)
        return Some parsed
      | _ -> return None
    with ex ->
      logWarn "exportSessionAsFsx" ex
      return None
  }
