# SageFs HTTP API — Neovim Integration Reference

> **Current State**: SageFs daemon runs on `localhost:37749` with the following endpoints. This document maps what EXISTS today vs what the plugin NEEDS.

---

## Existing Endpoints (Verified from Source)

### POST /exec
**Purpose**: Submit F# code for evaluation
**Request**:
```json
{
  "code": "let x = 1 + 1\n;;",
  "request_id": "req_123"  // TODO: Add this parameter
}
```
**Response** (synchronous, not ideal):
```json
{
  "success": true,
  "result": "val x : int = 2"
}
```

**Current issue**: Returns synchronously, blocks HTTP thread. Plugin needs fire-and-forget + SSE response.

**What plugin needs**: 
1. Accept `request_id` parameter
2. Return `202 Accepted` immediately
3. Emit `EvalCompleted` event via SSE with matching `request_id`

---

### GET /events
**Purpose**: SSE stream of Elm state changes
**Response**: Server-Sent Events format
```
event: state
data: {"type":"EvalCompleted","payload":{...}}

event: state
data: {"type":"SessionCreated","payload":{...}}
```

**Current state**: Emits Elm model changes as JSON
**What plugin needs**: Document the actual event schema (see below)

---

### GET /api/sessions
**Purpose**: List all active sessions
**Response**:
```json
{
  "sessions": [
    {
      "id": "abc123",
      "status": "Ready",
      "projects": ["MyApp.fsproj"],
      "workingDirectory": "/home/user/code/MyApp",
      "evalCount": 42,
      "avgDurationMs": 15.2
    }
  ]
}
```

**Plugin usage**: Called ONCE on startup to populate `_G.sagefs_model.sessions`

---

### GET /api/status
**Purpose**: Rich session status (includes Elm regions, PID, uptime)
**Query params**: `?sessionId=abc123`
**Response**:
```json
{
  "version": "1.0.0",
  "sessionId": "abc123",
  "sessionState": "Ready",
  "evalCount": 42,
  "avgDurationMs": 15.2,
  "workingDirectory": "/home/user/code/MyApp",
  "projects": ["MyApp.fsproj"],
  "regions": [...],
  "pid": 12345,
  "uptime": 3600.5
}
```

**Plugin usage**: Optional, for detailed debug info (`:SageFsStatus` command)

---

### POST /api/sessions/switch
**Purpose**: Switch active session for a client
**Request**:
```json
{
  "sessionId": "abc123"
}
```

**Plugin usage**: NOT NEEDED (plugin tracks session per-buffer in `vim.b.sagefs_session`)

---

### POST /reset
**Purpose**: Soft reset FSI session
**Response**:
```json
{
  "success": true,
  "message": "Session reset successfully"
}
```

---

### POST /hard-reset
**Purpose**: Hard reset with optional rebuild
**Request**:
```json
{
  "rebuild": true
}
```

---

### GET /diagnostics
**Purpose**: SSE stream of diagnostics updates
**Response**: Server-Sent Events
```
event: diagnostics
data: []

event: diagnostics
data: [{"line":5,"column":10,"severity":"Error","message":"Type mismatch"}]
```

**Current issue**: Separate stream from `/events`. Should these be unified?

**Plugin usage**: Subscribe to this stream OR get diagnostics via `EvalCompleted` event payload

---

### POST /diagnostics
**Purpose**: Fire-and-forget diagnostics check
**Request**:
```json
{
  "code": "let x = 1 + \"hello\""
}
```
**Response**: `202 Accepted` (results via SSE)

---

## Missing Endpoints (Plugin Needs)

### GET /completions
**Purpose**: Synchronous code completions for cursor position
**Query params**: 
- `session=abc123`
- `code=let x = System.Console.` (URL-encoded)
- `position=25` (cursor offset)

**Response**:
```json
{
  "completions": [
    {
      "name": "WriteLine",
      "kind": "Method",
      "type_signature": "string -> unit",
      "description": "Writes a line to console"
    }
  ]
}
```

**Implementation priority**: HIGH (needed for blink.cmp integration)

---

### POST /api/sessions (create)
**Purpose**: Create a new session
**Request**:
```json
{
  "projects": ["MyApp.fsproj"],
  "working_dir": "/home/user/code/MyApp"
}
```
**Response**:
```json
{
  "session_id": "abc123"
}
```

**Current workaround**: Plugin might use the MCP tool `create_session` via localhost:37749 if available

---

## SSE Event Schema (To Be Documented)

The plugin subscribes to `GET /events` and processes these message types:

### EvalCompleted
```json
{
  "type": "EvalCompleted",
  "payload": {
    "request_id": "req_123",
    "success": true,
    "output": "val x : int = 2",
    "diagnostics": [
      {
        "line": 1,
        "column": 5,
        "end_line": 1,
        "end_column": 10,
        "severity": "Warning",
        "message": "Unused value"
      }
    ],
    "duration_ms": 15.2,
    "session_id": "abc123"
  }
}
```

### SessionCreated
```json
{
  "type": "SessionCreated",
  "payload": {
    "id": "abc123",
    "status": "Ready",
    "projects": ["MyApp.fsproj"],
    "working_dir": "/home/user/code/MyApp"
  }
}
```

### SessionStopped
```json
{
  "type": "SessionStopped",
  "payload": {
    "id": "abc123"
  }
}
```

### TestRunCompleted
```json
{
  "type": "TestRunCompleted",
  "payload": {
    "session_id": "abc123",
    "total": 10,
    "passed": 7,
    "failed": 3,
    "duration_ms": 1200,
    "failures": [
      {
        "test_name": "Math tests / addition",
        "message": "Expected 3 but got 4",
        "stack_trace": "at MyApp.Tests.addition in /path/Tests.fsx:line 15\n..."
      }
    ]
  }
}
```

---

## Implementation Tasks for SageFs

### Priority 1: Plugin Can Ship
1. **POST /exec accepts request_id** (add parameter, don't break existing clients)
2. **EvalCompleted event emitted via SSE** (when eval completes, emit event with request_id)
3. **GET /completions endpoint** (synchronous, <100ms latency target)
4. **Document actual SSE event schema** (what JSON shape for each event type?)

### Priority 2: Nice to Have
1. **Unify /events and /diagnostics streams** (single SSE stream for all events)
2. **POST /api/sessions endpoint** (create session via HTTP, not just MCP tool)
3. **TestRunCompleted event** (when Expecto tests finish, emit results)

### Priority 3: Future
1. **Session scoped completions** (completions aware of session context, not just code snippet)
2. **Incremental diagnostics** (emit diagnostics as code changes, not just after eval)

---

## Testing the SSE Stream

### Monitor events manually
```bash
curl --no-buffer -N http://localhost:37749/events
```

### Parse in shell (for debugging)
```bash
curl -N http://localhost:37749/events | while IFS= read -r line; do
  echo "Received: $line"
done
```

### Trigger events
```bash
# In another terminal, eval some code
curl -X POST http://localhost:37749/exec \
  -H "Content-Type: application/json" \
  -d '{"code":"let x = 1 + 1\n;;"}'

# You should see an event appear in the SSE stream
```

---

## Plugin Implementation Notes

### When to use synchronous GET vs SSE subscription

**Use synchronous GET for:**
- Completions (need immediate response for blink.cmp)
- Initial session list (one-time on plugin startup)
- Status queries (`:SageFsStatus` command)

**Use SSE subscription for:**
- Eval results (async, can take 10-150ms)
- Session lifecycle (created/stopped)
- Test results (async, can take seconds)
- Diagnostics (async, background checks)

**Never poll** — if it changes over time, use SSE

---

## Request Correlation Pattern

The plugin generates unique request IDs and stores them in the eval queue:

```lua
-- On <Alt-Enter>
local request_id = string.format("req_%d_%d", os.time(), math.random(100000))

_G.sagefs_model.eval_queue[request_id] = {
  buffer = buf,
  cell = "cell_1",
  sent_at = vim.loop.hrtime()
}

-- POST with request_id
curl POST /exec {"code":"...", "request_id":"req_123"}

-- On SSE event
if msg.type == "EvalCompleted" then
  local req = _G.sagefs_model.eval_queue[msg.payload.request_id]
  -- Match request, update buffer state
  _G.sagefs_model.eval_queue[msg.payload.request_id] = nil  -- Clear
end
```

**Key invariants:**
1. Every POST gets a unique request_id
2. SageFs echoes request_id in the SSE event
3. Plugin matches event to pending request
4. Plugin clears request from queue (no leaks)

**Edge cases:**
- Plugin restarts while evals pending → queue is lost, events ignored (acceptable)
- Duplicate request_id → last one wins (random() makes this ~impossible)
- Missing request_id in event → log warning, no update (diagnostic)

---

## Latency Optimization

### Target: <50ms typical, <150ms worst case

**Bottlenecks (in order of impact):**
1. **FSI eval time** (5-150ms) — can't optimize, but can cache
2. **Network/TCP** (3-8ms) — localhost, minimal
3. **SSE emit + JSON serialize** (1-2ms) — already fast
4. **Lua JSON decode** (1-2ms) — native, fast
5. **Extmark update** (1-2ms) — native, fast

**Optimizations:**
- Cache module opens (don't re-open `System` every time)
- Batch multiple evals (if user hits `<Alt-Enter>` on 3 cells fast, send all 3)
- Preemptive completions (fetch on `.` keystroke, cache for 100ms)
- Incremental parsing (don't re-parse entire buffer on every keystroke)

**Non-optimizations (premature):**
- Local Lua cache of eval results (adds complexity, no measured win)
- WebSocket instead of SSE (more complex protocol, no latency win)
- Binary protocol (JSON is fast enough, costs dev time)

---

## Error Handling

### SSE connection lost
- **Symptom**: curl exits with non-zero code
- **Response**: Exponential backoff reconnect (1s → 2s → 4s → ... → 32s max)
- **UI**: Show "⚠ SSE disconnected" in statusline

### Eval timeout
- **Symptom**: Request in queue for >30s with no response
- **Response**: Show warning, leave request in queue (might still arrive)
- **UI**: Cell shows "⏳ (timeout?)"

### Invalid SSE event
- **Symptom**: JSON decode fails
- **Response**: Log warning, skip event, continue
- **UI**: No change (silent failure acceptable for malformed events)

### Session not found
- **Symptom**: buffer bound to session that no longer exists
- **Response**: Unbind buffer (`vim.b.sagefs_session = nil`), show warning
- **UI**: Statusline shows "SageFs: —"

---

## Security Considerations

### Localhost only (no remote connections)
- SageFs daemon binds to `127.0.0.1:37749` (not `0.0.0.0`)
- Plugin only connects to localhost
- No authentication needed (trust localhost)

### Code injection via completions
- Completions come from FSAC (trusted)
- No user input in completion labels
- No eval() or code execution from completion data

### Path traversal in file operations
- Plugin uses `vim.api.nvim_buf_get_name()` (trusted)
- No user-provided file paths sent to SageFs
- Session working directories are validated (must exist)

---

## Appendix: Example HTTP Calls

### Create session (manual test)
```bash
curl -X POST http://localhost:37749/api/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "projects": ["MyApp.fsproj"],
    "working_dir": "/home/user/code/MyApp"
  }'
```

### Eval code (manual test)
```bash
curl -X POST http://localhost:37749/exec \
  -H "Content-Type: application/json" \
  -d '{
    "code": "let x = 1 + 1\n;;",
    "request_id": "test_123"
  }'
```

### Get completions (manual test)
```bash
curl -G http://localhost:37749/completions \
  --data-urlencode "session=abc123" \
  --data-urlencode "code=System.Console." \
  --data-urlencode "position=16"
```

### Subscribe to SSE (manual test)
```bash
curl --no-buffer -N http://localhost:37749/events
```

---

*API reference for sagefs.nvim development*
*Based on SageFs source code inspection*
*Last updated: 2025*
