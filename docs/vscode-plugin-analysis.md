# VSCode Plugin Landscape Analysis

## Executive Summary

This document analyzes FsiX's VSCode extension architecture and best-in-class REPL/interactive coding extensions to inform SageFs's VSCode plugin design. The analysis reveals that while FsiX provides a solid notebook-based foundation, there are significant opportunities to leverage SageFs's unique capabilities (MCP server, event sourcing, multi-session support) to create a more powerful and distinctive developer experience.

## FsiX VSCode Extension Architecture

### Core Components

**Extension Entry Point (`extension.ts`)**
- Registers custom notebook type (`fsix-notebook`, `*.fsixb` files)
- Registers interactive REPL window controller
- Provides language features (completions, diagnostics)
- Registers commands for REPL/notebook creation and evaluation

**Connection Management (`fsixManager.ts`)**
- Manages TCP connections per notebook/REPL instance
- Connection map: `Record<NotebookUri, FsiXConnection>`
- First cell (index 0) is always initialization cell that spawns fsix-daemon
- Lifecycle: spawn daemon → TCP connect → track in map → dispose on close

**JSON-RPC Protocol (`rpc.ts`)**
- Methods: `eval`, `autocomplete`, `diagnostics`
- Notifications: `logging`, `initialized`
- Bi-directional over TCP socket
- Stateful connection per notebook

**Notebook Serialization (`fsixNotebookCore.ts`)**
- Custom file format: `*.fsixb` (JSON with cell array)
- Cell types: init (index 0), eval (rest)
- Metadata: hot reload flag per cell
- Two controllers: notebook and interactive REPL

**Language Provider (`languageProvider.ts`)**
- Completions from live REPL state (debounced 300ms)
- Diagnostics from type checker
- Hovers (basic)
- Scoped to notebook cells only

**Daemon Location (`fsixDllLocator.ts`)**
- Auto-detect .sln/.slnx/.fsproj in workspace
- Find fsix-daemon: local → global → install
- Run `dotnet build` before first use
- Spawn daemon with project path

### FsiX Commands

| Command | Description |
|---------|-------------|
| `fsix.startRepl` | Create new interactive REPL window |
| `fsix.createNotebook` | Create new `.fsixb` notebook |
| `fsix.openRepl` | Open existing REPL |
| `fsix.openNotebook` | Open existing notebook |
| `fsix.sendToRepl` | Send selection to REPL |
| `fsix.sendToNotebook` | Send selection to notebook |
| `fsix.evaluateCell` | Run cell (notebook) |
| `fsix.evaluateCellWithHotReload` | Run cell with hot reload |

### FsiX Strengths

1. **Notebook UX** — Familiar Jupyter-like interface for F# developers
2. **Inline hot reload toggle** — Per-cell control of reloading
3. **Live language features** — Completions/diagnostics from actual REPL state
4. **Project integration** — Auto-detect and load .fsproj/.sln
5. **Interactive REPL** — Uses VSCode's native interactive window API
6. **Custom serialization** — Notebooks persist properly as JSON

### FsiX Limitations

1. **Single session per notebook** — Cannot run multiple FSI sessions simultaneously
2. **No session persistence** — Daemon restart = lost state
3. **No history** — Past evals are gone
4. **No type exploration** — Limited to autocomplete
5. **TCP socket only** — No extensibility for other clients
6. **Init cell required** — Cell 0 always spawns daemon (coupling)
7. **No dashboard** — No visibility into REPL internals
8. **Terminal-based FSI** — Not a daemon architecture

## Best-in-Class Interactive Extensions

### Jupyter (Microsoft)

**Architecture:**
- Kernel protocol (ZMQ, Jupyter spec)
- Kernels are separate processes
- Extension is kernel manager + notebook renderer
- Supports local and remote kernels

**Key Features:**
- Rich output (HTML, images, LaTeX, interactive widgets)
- Execution order tracking
- Variable explorer panel
- Kernel management (start, stop, restart, interrupt)
- Multi-language via kernel switching
- Cell outputs persist in notebook
- Export to PDF/HTML

**What They Got Right:**
- Clear separation: UI (extension) vs compute (kernel)
- Standard protocol enables ecosystem (JupyterLab, nteract, etc.)
- Rich outputs make data exploration delightful
- Variable explorer for state inspection

**What's Missing:**
- No inline evaluation outside notebooks
- Heavy UX for quick experiments
- No live diagnostics/completions from kernel state

### Ionide (F# Community)

**Architecture:**
- F# Language Server (FSAC) via LSP
- FSI integration via terminal automation
- Separate processes: extension, FSAC, FSI terminal

**Key Features:**
- Full F# language features (diagnostics, completions, refactorings)
- "Send to FSI" (Alt+Enter) — sends selection to terminal
- Solution explorer
- CodeLens (references, tests)
- Inline type hints
- FAKE/Paket integration

**What They Got Right:**
- Rock-solid language features via FSAC
- Fast "Send to FSI" workflow
- Doesn't reinvent F# tooling
- Works with any project structure

**What's Missing:**
- FSI is just a terminal — no state awareness
- No completions from REPL state
- No notebook UX
- No hot reload
- Results only in terminal output
- Can't query REPL state

### Polyglot Notebooks (.NET Interactive)

**Architecture:**
- .NET Interactive kernel (multi-language)
- Extension renders `.dib` files
- Kernels per language, variable sharing

**Key Features:**
- Multi-language notebooks (`#!fsharp`, `#!csharp`, etc.)
- Variable sharing across languages
- Rich outputs (HTML, SVG, charts via libraries)
- Magic commands (`#!share`, `#!who`, etc.)
- Integration with data libraries (Plotly.NET, XPlot)

**What They Got Right:**
- Multi-language is genuinely useful for data work
- Rich outputs for charts/plots
- Variable introspection
- Good documentation and examples

**What's Missing:**
- Heavy for quick REPL exploration
- Not optimized for web development workflow
- No hot reload
- Kernel startup can be slow

### Quokka.js / Wallaby.js

**Architecture:**
- Babel/TypeScript transpiler integration
- Inline execution engine
- Coverage tracking
- VSCode extension renders inline results

**Key Features:**
- **Inline results** — Values appear next to code in real-time
- Live execution as you type (debounced)
- Coverage indicators in gutter (green/yellow/red)
- Time travel debugging
- Console.log output inline
- Expression evaluation on hover
- Works with test files

**What They Got Right:**
- **Immediacy** — Results where you write code, not elsewhere
- Low ceremony — just start typing
- Visual coverage feedback
- Fast (incremental execution)

**What's Missing:**
- JavaScript/TypeScript only
- Proprietary (Wallaby especially)
- No REPL interaction
- No notebook UX

### Calva (Clojure, nREPL)

**Architecture:**
- nREPL server (Clojure REPL protocol)
- Extension connects as nREPL client
- Paredit for structural editing

**Key Features:**
- Inline evaluation (`Ctrl+Enter` on expression)
- Results shown inline (CodeLens) and in output panel
- Paredit for s-exp manipulation
- Namespace browser
- Test runner integration
- REPL history
- Debugger integration

**What They Got Right:**
- Inline eval without notebook ceremony
- REPL is always available, connected to editor
- Paredit makes Lisp editing pleasant
- Test runner integration

**What's Missing:**
- Lisp-specific (s-exp structure)
- nREPL protocol not applicable to F#

### REST Client

**Architecture:**
- Parse HTTP requests in `.http` files
- Execute via Node HTTP client
- Render response in side panel

**Key Features:**
- Send HTTP requests from editor
- Response preview (formatted JSON, HTML, etc.)
- Environment variables
- Request history
- GraphQL support

**What They Got Right:**
- No context switching (requests are in editor)
- Response formatting
- History is useful

**What's Missing:**
- Single-purpose tool
- Not applicable to general REPL

## Comparative Analysis: What Makes a Great Interactive Coding Extension?

### Critical Success Factors

| Factor | Jupyter | Ionide | Polyglot | Quokka | Calva |
|--------|---------|--------|----------|--------|-------|
| **Inline results** | No (below cell) | No (terminal) | No (below cell) | **Yes** | **Yes** |
| **Live completions from state** | No | No | No | No | **Yes** |
| **Rich outputs** | **Yes** | No | **Yes** | Partial | No |
| **Hot reload** | No | No | No | No | No |
| **Notebook UX** | **Yes** | No | **Yes** | No | No |
| **Session persistence** | Kernel restart | No | Kernel restart | N/A | No |
| **Type exploration** | Limited | **Yes** (FSAC) | Limited | Limited | **Yes** |
| **Multiple sessions** | **Yes** | No | No | N/A | **Yes** |
| **History** | In notebook | Terminal | In notebook | No | Output panel |

### Key Insights

**1. Inline vs. Notebook is a False Dichotomy**
- Quokka shows inline is powerful for exploratory coding
- Jupyter shows notebooks are powerful for reproducible analysis
- Best extension would support BOTH

**2. REPL State Awareness is Rare and Valuable**
- Calva's inline eval with state-aware completions is beloved
- Ionide's terminal FSI has no state awareness
- This is a major opportunity for SageFs

**3. Rich Outputs Matter for Data Work**
- Jupyter's HTML/image rendering is essential for data science
- F# has Plotly.NET, Deedle — rich outputs would unlock these
- Web dev: rendering HTML responses is valuable

**4. Low Ceremony for Quick Experiments**
- Quokka: just write code, see results
- Jupyter: must create notebook, start kernel, create cells
- Ionide: must start terminal, send to FSI
- Lower ceremony = more experimentation

**5. Multiple Sessions are Underrated**
- Jupyter supports it, rarely used in practice
- But for microservices, testing against multiple environments, or different library versions, it's essential

**6. Hot Reload is Missing Everywhere**
- Web dev (Falco, Giraffe) needs it
- SageFs's file watcher is unique
- No existing extension handles this well

**7. History/Time Travel is Underexplored**
- REPL history is usually just "scroll back"
- Event sourcing enables: search, replay, share sessions
- No extension has explored this deeply

## F# Ecosystem Pain Points Today

### Developer Interviews (Synthesized)

**Pain Point 1: REPL is Disconnected**
> "I use Ionide for editing and terminal FSI for testing. But FSI doesn't know about my code changes until I reload. And when I reload, I lose state. So I end up copy-pasting setup code constantly." — Backend dev

**Pain Point 2: Web Dev Hot Reload is Manual**
> "I'm building a Falco app. I change a handler, have to recompile, restart the server, refresh the browser. Giraffe's hot reload is better but still not seamless. I want Vite's DX for F#." — Web dev

**Pain Point 3: No Notebook for Demos**
> "I want to show F# features to my team. Jupyter with .NET Interactive works but is clunky. I want something that feels native to F#, not bolted on." — Tech lead

**Pain Point 4: Testing is Tedious**
> "I write a test in Expecto, run it, it fails, I fix it, run again. I want Quokka's inline results for tests." — TDD practitioner

**Pain Point 5: Documentation is Scattered**
> "I'm learning a new library. I bounce between browser docs, `#help`, and trial-and-error in FSI. I want type exploration in the editor." — Junior dev

**Pain Point 6: State Management in FSI**
> "FSI is stateful but I can't see what's defined. I have to remember or grep my terminal history. Variable explorer would be huge." — Data scientist

**Pain Point 7: Sharing Knowledge**
> "I solve a problem in FSI, want to share with team. I copy-paste into Slack, loses context. I want to export the session as a notebook or shareable link." — Senior dev

### Unmet Needs

1. **REPL state awareness** — Completions, diagnostics, variable explorer from live FSI
2. **Hot reload for web dev** — File change → recompile → server restart → browser refresh, automated
3. **Inline results** — See values without switching to terminal/notebook output
4. **Type exploration** — Browse types, namespaces, API docs without leaving editor
5. **Session persistence** — REPL state survives restarts
6. **History search** — Find past evals, replay them
7. **Notebook export** — Turn REPL session into shareable notebook
8. **Test runner integration** — Run tests inline, see results immediately
9. **Multiple sessions** — Test against different environments/configs simultaneously
10. **Rich outputs** — Render HTML, images, charts in editor

## Competitive Positioning

### How SageFs Could Differentiate

**vs. Ionide:**
- Keep Ionide's language features (FSAC)
- Add: REPL state awareness, inline results, hot reload, notebooks

**vs. Polyglot Notebooks:**
- Simpler startup (no kernel ceremony)
- Faster (native daemon, not .NET Interactive)
- F#-first (not multi-language compromise)

**vs. FsiX:**
- Multiple sessions
- Event sourcing (history, replay, search)
- Type exploration
- Dashboard/observability
- Hot reload for web dev

**vs. Quokka:**
- F# instead of JS
- REPL interaction (not just inline eval)
- Notebook support (not just inline)

### The Unoccupied Niche

**"Live F# Development Environment"**

Not just a REPL, not just a notebook — a **live coding environment** where:
- Code and REPL state are always in sync
- Changes are hot-reloaded automatically
- Results appear inline and in notebooks
- Type exploration is effortless
- History is searchable and shareable
- Multiple sessions enable parallel workflows

This doesn't exist in any language ecosystem. SageFs has the primitives to build it.

## Recommendations for Analysis Phase

### Must Study Deeply

1. **Quokka.js** — Inline results UX, performance model
2. **Calva** — nREPL integration, inline eval workflow
3. **Jupyter** — Kernel protocol, rich outputs
4. **Ionide** — F# ecosystem integration, extension points

### Key Questions for Design Phase

1. **Protocol:** MCP vs. custom vs. LSP?
2. **Primary UX:** Inline eval (Quokka) vs. notebook (Jupyter) vs. REPL window (Ionide)?
3. **Ionide relationship:** Complement or replace?
4. **Multi-session:** How to surface in UI?
5. **Event sourcing:** What UX does history enable?
6. **Hot reload:** For whom? (web devs, all devs, optional?)
7. **Type exploration:** Tree view, hover, command palette?
8. **Rich outputs:** Which formats? (HTML, SVG, images?)
9. **Dashboard:** Webview panel vs. native UI elements?
10. **The killer feature:** What makes this unmissable?

## Appendix: Extension Feature Matrix

| Feature | FsiX | Ionide | Polyglot | Quokka | Calva | **SageFs Potential** |
|---------|------|--------|----------|--------|-------|---------------------|
| Notebook UX | ✅ | ❌ | ✅ | ❌ | ❌ | ✅ |
| Inline eval | ❌ | ❌ | ❌ | ✅ | ✅ | ✅ |
| REPL window | ✅ | ✅ | ❌ | ❌ | ✅ | ✅ |
| Live completions | ✅ | ✅ (FSAC) | ❌ | ❌ | ✅ | ✅ |
| Live diagnostics | ✅ | ✅ (FSAC) | ❌ | ❌ | ✅ | ✅ |
| Rich outputs | ❌ | ❌ | ✅ | Partial | ❌ | ✅ |
| Hot reload | Per-cell | ❌ | ❌ | ❌ | ❌ | Auto (file watcher) |
| Multiple sessions | ❌ | ❌ | ❌ | ❌ | ✅ | ✅ |
| History search | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ (event sourcing) |
| Type exploration | ❌ | ✅ (FSAC) | ❌ | ❌ | ✅ | ✅ (MCP tools) |
| Variable explorer | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ |
| Session persistence | ❌ | ❌ | ❌ | N/A | ❌ | ✅ |
| Time travel | ❌ | ❌ | ❌ | ✅ | ❌ | ✅ (event sourcing) |
| Dashboard | ❌ | ❌ | ❌ | ❌ | ❌ | ✅ (port 37750) |
| Test runner | ❌ | ✅ (FSAC) | ❌ | ✅ | ✅ | ✅ |

**Legend:**
- ✅ = Fully supported
- Partial = Limited support
- ❌ = Not supported
- N/A = Not applicable

## Conclusion

The VSCode extension landscape reveals significant opportunities for SageFs:

1. **FsiX provides a solid notebook foundation** but lacks session persistence, multi-session support, and observability
2. **Ionide has excellent language features** via FSAC but minimal REPL integration
3. **Jupyter's notebook UX is proven** but heavy for quick experiments
4. **Quokka's inline results are beloved** but JS-only
5. **Calva's inline eval with state awareness** shows the power of editor-REPL integration

**SageFs's unique capabilities** (MCP server, event sourcing, multi-session, hot reload) enable building something that doesn't exist today: a **live F# development environment** that combines the best of notebooks, inline eval, and REPL integration, with event sourcing unlocking novel features like history search and session replay.

The design phase must answer: **What's the right balance of familiarity (notebook UX) vs. novelty (inline eval, event sourcing UX)?** And: **How do we integrate with Ionide rather than compete?**

---

**Next:** `vscode-plugin-design.md` — Architecture deliberation and recommended design.
