# SageFs.nvim Quick Reference — Core Patterns

## SSE Subscription Pattern (Neovim + curl)

```lua
-- sse.lua — The idiomatic Neovim SSE client
local M = {}
local job_id = nil
local buffer = ""

local function parse_sse(lines, callback)
  local event_type, data = nil, nil
  for _, line in ipairs(lines) do
    if line:match("^event: ") then
      event_type = line:match("^event: (.+)")
    elseif line:match("^data: ") then
      data = line:match("^data: (.+)")
    elseif line == "" and event_type and data then
      local ok, payload = pcall(vim.json.decode, data)
      if ok then callback({ type = event_type, payload = payload }) end
      event_type, data = nil, nil
    end
  end
end

M.start = function(url, callback)
  if job_id then vim.fn.jobstop(job_id) end
  
  job_id = vim.fn.jobstart(
    {"curl", "--no-buffer", "-N", url},
    {
      on_stdout = function(_, data, _)
        for _, chunk in ipairs(data) do
          buffer = buffer .. chunk
          local lines = vim.split(buffer, "\n", { plain = true })
          buffer = table.remove(lines) or ""
          parse_sse(lines, callback)
        end
      end,
      on_exit = function(_, code, _)
        if code ~= 0 then
          vim.defer_fn(function() M.start(url, callback) end, 2000)
        end
      end,
    }
  )
end

return M
```

**Key insight**: `on_stdout` accumulates partial chunks, splits on newlines, keeps last incomplete line in buffer. No async/await needed — callbacks all the way.

---

## Elmish Reducer Pattern

```lua
-- model.lua — Single reducer for all state changes
local M = {}

_G.sagefs_model = {
  sessions = {},      -- id → {status, projects, ...}
  eval_queue = {},    -- request_id → {buffer, cell, sent_at}
  test_index = {},    -- source ↔ test mappings
}

M.dispatch = function(msg)
  local model = _G.sagefs_model
  
  if msg.type == "EvalCompleted" then
    local req = model.eval_queue[msg.payload.request_id]
    if req and vim.api.nvim_buf_is_valid(req.buffer) then
      local cells = vim.b[req.buffer].sagefs_cells or {}
      cells[req.cell] = {
        status = msg.payload.success and "Success" or "Error",
        output = msg.payload.output,
        eval_time_ms = msg.payload.duration_ms,
      }
      vim.b[req.buffer].sagefs_cells = cells
      require("sagefs.extmarks").update_cell(req.buffer, req.cell)
    end
    model.eval_queue[msg.payload.request_id] = nil
    
  elseif msg.type == "SessionCreated" then
    model.sessions[msg.payload.id] = msg.payload
    
  elseif msg.type == "TestRunCompleted" then
    model.test_index = require("sagefs.tests").parse_results(msg.payload)
    require("sagefs.tests").update_gutters()
  end
end

return M
```

**Key insight**: Global model for cross-buffer state, buffer-local (`vim.b`) for ephemeral per-buffer state. Reducer is pure — no side effects except updating model.

---

## Fire-and-Forget Eval Pattern

```lua
-- eval.lua — <Alt-Enter> handler
M.eval_current_cell = function()
  local buf = vim.api.nvim_get_current_buf()
  local cell = require("sagefs.cells").find_current_cell(buf)
  if not cell then return end
  
  local code = table.concat(
    vim.api.nvim_buf_get_lines(buf, cell.start, cell.end_, false),
    "\n"
  )
  
  -- Generate unique request ID
  local request_id = string.format("req_%d_%d", os.time(), math.random(100000))
  
  -- Store in queue (for response correlation)
  _G.sagefs_model.eval_queue[request_id] = {
    buffer = buf,
    cell = string.format("cell_%d", cell.start),
    sent_at = vim.loop.hrtime(),
  }
  
  -- Flash cell (immediate visual feedback)
  require("sagefs.extmarks").flash_cell(buf, cell)
  
  -- Fire-and-forget POST
  vim.fn.system(string.format(
    'curl -s -X POST http://localhost:37749/exec ' ..
    '-H "Content-Type: application/json" ' ..
    '-d \'{"code":"%s","request_id":"%s"}\'',
    code:gsub('"', '\\"'), request_id
  ))
  
  -- Update cell to "Evaluating" (optimistic UI)
  local cells = vim.b[buf].sagefs_cells or {}
  cells[string.format("cell_%d", cell.start)] = { status = "Evaluating" }
  vim.b[buf].sagefs_cells = cells
  require("sagefs.extmarks").update_cell(buf, cell)
end
```

**Key insight**: POST is fire-and-forget (async `vim.fn.system`). User sees flash immediately. Response comes via SSE, matched by request_id. Optimistic UI shows "Evaluating" status immediately.

---

## Cell Boundary Detection (Regex Fallback)

```lua
-- cells.lua — Find ;; boundaries
M.find_boundaries = function(buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local cells = {}
  local start = 0
  
  for i, line in ipairs(lines) do
    if line:match("^%s*;;%s*$") then  -- Line with only ;; and whitespace
      table.insert(cells, { start = start, end_ = i - 1 })
      start = i
    end
  end
  
  -- Last cell (if file doesn't end with ;;)
  if start < #lines then
    table.insert(cells, { start = start, end_ = #lines })
  end
  
  return cells
end

M.find_current_cell = function(buf)
  local cells = M.find_boundaries(buf)
  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1] - 1  -- 0-indexed
  
  for _, cell in ipairs(cells) do
    if line >= cell.start and line <= cell.end_ then
      return cell
    end
  end
  return nil
end
```

**Key insight**: Simple regex for Phase 1. Tree-sitter in Phase 2. Works for 95% of real code.

---

## Extmark Rendering (Pure Function of State)

```lua
-- extmarks.lua — Visual state from model
local ns = vim.api.nvim_create_namespace("sagefs")

M.update_cell = function(buf, cell_id)
  local cells = vim.b[buf].sagefs_cells or {}
  local cell_state = cells[cell_id]
  if not cell_state then return end
  
  local cell_num = tonumber(cell_id:match("cell_(%d+)"))
  if not cell_num then return end
  
  -- Gutter sign
  local sign = "SageFsEvalStale"
  if cell_state.status == "Success" then sign = "SageFsEvalOk"
  elseif cell_state.status == "Error" then sign = "SageFsEvalErr"
  elseif cell_state.status == "Evaluating" then sign = "SageFsEvalPending"
  end
  
  vim.fn.sign_place(0, "sagefs", sign, buf, { lnum = cell_num + 1 })
  
  -- Inline output (virtual line)
  if cell_state.output then
    vim.api.nvim_buf_set_extmark(buf, ns, cell_num, 0, {
      virt_lines = {
        {{ "  " .. cell_state.output, "Comment" }}
      },
      virt_lines_above = false,
    })
  end
  
  -- Eval time (right-aligned on ;; line)
  if cell_state.eval_time_ms then
    vim.api.nvim_buf_set_extmark(buf, ns, cell_num, 0, {
      virt_text = {{ string.format("(%dms)", cell_state.eval_time_ms), "Comment" }},
      virt_text_pos = "right_align",
    })
  end
end

M.flash_cell = function(buf, cell)
  -- Brief highlight for visual feedback
  local hl_group = "SageFsFlash"
  vim.api.nvim_buf_set_extmark(buf, ns, cell.start, 0, {
    end_row = cell.end_,
    hl_group = hl_group,
    hl_mode = "combine",
    ephemeral = true,
  })
  
  vim.defer_fn(function()
    vim.api.nvim_buf_clear_namespace(buf, ns, cell.start, cell.end_)
  end, 200)  -- Flash for 200ms
end
```

**Key insight**: All visual state derives from `vim.b.sagefs_cells`. Extmarks are PROJECTIONS, not state. Can be cleared and redrawn at any time.

---

## Diagnostics Integration

```lua
-- diagnostics.lua — SSE → vim.diagnostic
local ns = vim.api.nvim_create_namespace("sagefs_diagnostics")

M.set = function(buf, diagnostics)
  local vim_diagnostics = {}
  
  for _, diag in ipairs(diagnostics) do
    table.insert(vim_diagnostics, {
      lnum = diag.line - 1,  -- 0-indexed
      col = diag.column - 1,
      end_lnum = diag.end_line - 1,
      end_col = diag.end_column - 1,
      severity = diag.severity == "Error" and vim.diagnostic.severity.ERROR
                 or vim.diagnostic.severity.WARN,
      message = diag.message,
      source = "SageFs",
    })
  end
  
  vim.diagnostic.set(ns, buf, vim_diagnostics, {
    virtual_text = true,
    signs = true,
    underline = true,
  })
end
```

**Key insight**: Use Neovim's native diagnostic system. Works with `:DiagnosticList`, `]d`/`[d` navigation, etc.

---

## Test Index (Bidirectional Mapping)

```lua
-- tests.lua — Source ↔ Test navigation
M.parse_test_results = function(test_output)
  -- Parse Expecto XML or JSON output
  -- Extract: test name, status, source file + line (from stack trace)
  local index = {
    source_to_tests = {},  -- "src/Math.fs:15" → ["Test1", "Test2"]
    test_to_sources = {},  -- "Test1" → ["src/Math.fs:15", "src/Math.fs:22"]
  }
  
  -- Example: parse stack trace line
  -- "at MyApp.Tests.addition in /path/Tests.fsx:line 15"
  local pattern = "at .+ in (.+):line (%d+)"
  for line in test_output:gmatch("[^\n]+") do
    local file, line_num = line:match(pattern)
    if file and line_num then
      local source_key = string.format("%s:%s", file, line_num)
      -- Build bidirectional index...
    end
  end
  
  return index
end

M.goto_test = function()
  local buf = vim.api.nvim_get_current_buf()
  local cursor = vim.api.nvim_win_get_cursor(0)
  local source_key = string.format("%s:%d", 
    vim.api.nvim_buf_get_name(buf), cursor[1])
  
  local tests = _G.sagefs_model.test_index.source_to_tests[source_key]
  if tests and #tests > 0 then
    -- Jump to first test
    -- Parse test location, open buffer, set cursor
  end
end
```

**Key insight**: Test index is GLOBAL (in `_G.sagefs_model`), updated on every test run via SSE. Navigation commands read from index.

---

## Completion Source (Synchronous)

```lua
-- completions.lua — blink.cmp source
local M = {}

M.register = function()
  local source = {}
  
  source.get_trigger_characters = function()
    return {"."}
  end
  
  source.get_completions = function(params, callback)
    local buf = params.context.bufnr
    local session_id = vim.b[buf].sagefs_session
    if not session_id then
      callback({ is_incomplete_forward = false, is_incomplete_backward = false, items = {} })
      return
    end
    
    local line = params.context.line
    local cursor_col = params.context.cursor.col
    
    -- Sync GET /completions
    local response = vim.fn.system(string.format(
      'curl -s "http://localhost:37749/completions?session=%s&code=%s&position=%d"',
      session_id,
      vim.fn.shellescape(line),
      cursor_col
    ))
    
    local ok, data = pcall(vim.json.decode, response)
    if not ok then
      callback({ is_incomplete_forward = false, is_incomplete_backward = false, items = {} })
      return
    end
    
    local items = {}
    for _, item in ipairs(data.completions or {}) do
      table.insert(items, {
        label = item.name,
        kind = item.kind,  -- Function, Variable, Type, etc.
        detail = item.type_signature,
        documentation = item.description,
      })
    end
    
    callback({
      is_incomplete_forward = false,
      is_incomplete_backward = false,
      items = items
    })
  end
  
  require("blink.cmp").register_source("sagefs", source)
end

return M
```

**Key insight**: Completions MUST be synchronous. Use `vim.fn.system` (blocking), accept the 20-50ms latency. blink.cmp will timeout if >100ms, but that's acceptable.

---

## Session Auto-Binding

```lua
-- workspace.lua — Detect project, bind to session
M.bind_buffer_to_session = function(buf)
  -- Skip if already bound
  if vim.b[buf].sagefs_session then return end
  
  local file = vim.api.nvim_buf_get_name(buf)
  if file == "" then return end
  
  -- Find nearest .fsproj
  local dir = vim.fn.fnamemodify(file, ":h")
  local project = M.find_nearest_project(dir)
  if not project then return end
  
  -- Find or create session for this project
  local sessions = _G.sagefs_model.sessions
  local session_id = nil
  
  for id, session in pairs(sessions) do
    for _, proj in ipairs(session.projects) do
      if proj == project then
        session_id = id
        break
      end
    end
    if session_id then break end
  end
  
  if not session_id then
    -- Create new session
    local response = vim.fn.system(string.format(
      'curl -s -X POST http://localhost:37749/api/sessions ' ..
      '-H "Content-Type: application/json" ' ..
      '-d \'{"projects":["%s"],"working_dir":"%s"}\'',
      project, vim.fn.getcwd()
    ))
    local ok, data = pcall(vim.json.decode, response)
    if ok and data.session_id then
      session_id = data.session_id
    end
  end
  
  vim.b[buf].sagefs_session = session_id
end

M.find_nearest_project = function(start_dir)
  local dir = start_dir
  while dir ~= "/" and dir ~= "" do
    local candidates = vim.fn.globpath(dir, "*.fsproj", false, true)
    if #candidates > 0 then
      return candidates[1]
    end
    dir = vim.fn.fnamemodify(dir, ":h")
  end
  return nil
end
```

**Key insight**: Auto-bind on BufEnter, cache binding in `vim.b[buf].sagefs_session`. One session per project (reuse if exists).

---

## Statusline Component

```lua
-- statusline.lua — Export for lualine/heirline
local M = {}

M.status = function()
  local buf = vim.api.nvim_get_current_buf()
  local session_id = vim.b[buf].sagefs_session
  if not session_id then
    return "SageFs: —"
  end
  
  local session = _G.sagefs_model.sessions[session_id]
  if not session then
    return "SageFs: ?"
  end
  
  local icon = "✓"
  if session.status == "Error" then icon = "✖"
  elseif session.status == "Evaluating" then icon = "⏳"
  end
  
  local project_name = vim.fn.fnamemodify(session.projects[1] or "", ":t:r")
  local stale_count = 0
  local cells = vim.b[buf].sagefs_cells or {}
  for _, cell in pairs(cells) do
    if cell.status == "Stale" then stale_count = stale_count + 1 end
  end
  
  local status_parts = {
    string.format("%s SageFs: %s", icon, project_name),
    string.format("%d evals", session.eval_count or 0),
  }
  if stale_count > 0 then
    table.insert(status_parts, string.format("%d stale", stale_count))
  end
  
  return table.concat(status_parts, " | ")
end

return M
```

**Usage in lualine:**
```lua
require('lualine').setup {
  sections = {
    lualine_x = { 'sagefs_status' }
  }
}
```

---

## Testing Strategy

### Unit Tests (Lua)
Use `plenary.nvim` test harness:

```lua
-- tests/sse_spec.lua
local sse = require("sagefs.sse")

describe("SSE parser", function()
  it("parses complete event", function()
    local events = {}
    sse.parse_sse({
      "event: test",
      "data: {\"foo\":\"bar\"}",
      ""
    }, function(e) table.insert(events, e) end)
    
    assert.equals(1, #events)
    assert.equals("test", events[1].type)
    assert.equals("bar", events[1].payload.foo)
  end)
end)
```

### Integration Tests
Spawn SageFs daemon, run evals, verify output:

```lua
-- tests/integration_spec.lua
describe("End-to-end eval", function()
  before_each(function()
    -- Spawn SageFs daemon
    vim.fn.system("SageFs --proj TestProject.fsproj &")
    vim.wait(2000)  -- Wait for warmup
  end)
  
  it("evaluates simple arithmetic", function()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {"let x = 1 + 1", ";;"})
    
    require("sagefs.eval").eval_current_cell()
    
    -- Wait for SSE response
    vim.wait(500, function()
      local cells = vim.b[buf].sagefs_cells or {}
      return cells["cell_0"] and cells["cell_0"].status == "Success"
    end)
    
    local cells = vim.b[buf].sagefs_cells
    assert.equals("Success", cells["cell_0"].status)
    assert.equals("val x : int = 2", cells["cell_0"].output)
  end)
end)
```

---

## Debugging Tips

### Monitor SSE stream
```bash
curl --no-buffer -N http://localhost:37749/events
```

### Inspect model state
```lua
:lua print(vim.inspect(_G.sagefs_model))
```

### Measure latency
```lua
local start = vim.loop.hrtime()
-- ... operation ...
local elapsed = (vim.loop.hrtime() - start) / 1e6  -- Convert to ms
print(string.format("Took %.2fms", elapsed))
```

### Enable verbose logging
```lua
require("sagefs").setup({
  log_level = vim.log.levels.DEBUG
})
```

---

*Quick reference compiled from architecture analysis*
*Last updated: 2025*
