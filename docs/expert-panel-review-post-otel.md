# Expert Panel Review: Post-OTEL Wiring (v0.5.131)

**Date**: 2026-02-24
**Context**: Live Unit Testing feature — all OTEL instrumentation wired, 1,594 tests green.

## Panel Recommendations (Synthesized)

### Priority 1: Prove in REPL — TDD Before Files

| # | Item | Time Est | Key Insight |
|---|------|----------|-------------|
| 1 | AdaptiveDebounce scenarios | 20 min | Pure state machine, zero tests today. 5 concrete scenarios. |
| 2 | `buildFromSymbolUses` correctness | 45 min | Keystone for FCS dependency graph. Fabricated inputs. |
| 3 | Gutter transition state machine | 30 min | `RunGeneration` counter replaces `IsRunning: bool`. |
| 4 | Enriched `TestResultsBatch` type | 15 min | Replace `count: int` with `TestStatusEntry array`. |

### Priority 2: Wire After Proving

| # | Item | Dependency |
|---|------|------------|
| 5 | FCS symbol graph as primary affected-test resolver | Item 2 proven |
| 6 | Conservative fallback as safety net (already done) | — |
| 7 | SSE push with enriched payloads | Item 4 proven |
| 8 | Optimistic gutter transitions | Item 3 proven |

### Deliberately Deferred

- **Incremental FCS** — OTEL will tell us when it matters
- **Actor pipeline refactor** — Keep flat `PipelineEffect` DU as refactoring seam
- **AttributeBased executor** — Returns `NotRun`; decide: finish or remove
- **Coverage display** — Types exist, wiring doesn't, not critical path

## Key Domain Logic Gaps Found

| Gap | Severity | Expert |
|-----|----------|--------|
| AdaptiveDebounce untested | Medium | Seemann, Muratori |
| `buildFromSymbolUses` untested | High | Wlaschin, Syme |
| `IsRunning: bool` loses Running state on edit | Medium | Wlaschin, Muratori |
| `TestResultsBatch` carries only count | High | Gillilan, Brouwers |
| `SymbolReference` vs `ExtractedSymbolUse` seam | Low | Syme |
| AttributeBased executor returns NotRun | Medium | Bill |

## Key Disagreements (Productive Tensions)

### Conservative Fallback: Good Enough?
- **Carmack + Primeagen**: Yes for now. Measure with OTEL first.
- **Stannard**: No — it's destroying the value proposition at scale (50+ tests).
- **Miller (synthesis)**: Keep fallback as safety net, wire symbol graph as primary path.

### Property Tests vs Concrete Tests for AdaptiveDebounce
- **Seemann**: Property tests catch config-change edge cases.
- **Muratori**: Module is 60 lines — concrete specs are clearer.
- **Resolution**: Concrete tests at this scale; revisit if module grows.

### Pipeline Architecture: Elm vs Actors
- **Haynes**: Pipeline stages should be independent actors (future-proofing).
- **Brouwers**: 3 DU cases don't warrant actor overhead.
- **Stannard (synthesis)**: Keep flat DU — it's the mechanical refactoring seam.

### MCP vs Human-Facing Design
- **Holden**: MCP tools designed for LLM agents; invest in human SSE path instead.
- **DeVries**: MCP IS the editor integration layer — humans see gutters through it.
- **Resolution**: Both paths matter. Enrich SSE so editors don't need to poll MCP.

## Architecture Concerns

### Concurrent Test Run Races (Stannard, Carmack)
`Async.Start` is fire-and-forget. Two rapid `RunAffectedTests` effects could overlap.
- **Mitigation**: `RunGeneration` counter discards stale results
- **Risk accepted**: Wasted CPU on cancelled runs (monitor via `execution_ms` histogram)

### O(n²) Result Processing (Muratori)
Per-test dispatching means n Elm updates for n results, each scanning n tests.
- **Fix**: Collect all results in effect handler, dispatch single `TestResultsBatch`

### Gutter State Gap (Wlaschin)
`onKeystroke` sets `IsRunning = false` while tests are still executing.
- **Fix**: Replace `IsRunning: bool` with `RunGeneration` + `TestRunPhase` DU
