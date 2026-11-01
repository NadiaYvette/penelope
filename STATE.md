<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# penelope — Operational State (`STATE.md`)

**Status:** ACTIVE OPERATIONAL FRONTIER · October 2026  
**Implementation Plan:** [`docs/notes/IMPLEMENTATION_PLAN.md`](docs/notes/IMPLEMENTATION_PLAN.md)  
**Strategic Roadmap:** [`PLAN.md`](PLAN.md)  
**Design Monograph:** [`docs/notes/PENELOPE_DESIGN.md`](docs/notes/PENELOPE_DESIGN.md)

---

## Where We Are
 
 1. **Workspace Skeleton Grounded:**
    - Multi-package hierarchy (`penelope-core`, `penelope-static`, `penelope-effects`) operational under GHC 9.14.1.
    - All physical verification gates passing cleanly via `./bin/penelope-verify` (0 HLint hints, full test suite passing, 0 broken links, valid Tagref references, >= 50% comment density).
    - Remote replication topology configured across sovereign (`rad`) and commercial/community mirrors (`github`, `gitcode`, `framagit`, `disroot`).
 
 2. **Canonical Architecture & Implementation Plan Blessed:**
    - Grounded canonical implementation plan in [`docs/notes/IMPLEMENTATION_PLAN.md`](docs/notes/IMPLEMENTATION_PLAN.md) spanning all six engineering phases.
    - Grounded canonical design decisions (D1–D10) and Tagref-anchored notes in [`docs/notes/PENELOPE_DESIGN.md`](docs/notes/PENELOPE_DESIGN.md).
    - Operational roadmap updated in [`PLAN.md`](PLAN.md).

 3. **Phase 1 Polytropos Prerequisites Complete:**
    - Upstream `polytropos` incorporates `RowMorph`, `KnownRowMorph`, `FrameWire`, and poly-kinded `ConstructorIndex` solvers.
    - Cleanly integrated into local Penelope builds via `cabal.project.local`.

 4. **Phases 2–5 Task Execution Complete & Merged:**
    - All 14 standalone `yamaarashi-exec` task packets executed, verified, and fast-forward merged into `master`:
      - **Phase 2 (Core Syntax & Representation):** `phase2-01-syntax-gadt`, `phase2-02-route-optimizer`, `phase2-03-smart-constructors`
      - **Phase 3 (Polycategorical Wiring & Frame Rule):** `phase3-01-sequential-composition`, `phase3-02-frame-rule-wiring`, `phase3-03-disjunctive-fanout`, `phase3-04-row-weakening`
      - **Phase 4 (Two-Phase Weaving & Interpretation Engine):** `phase4-01-static-analysis`, `phase4-02-higher-order-elaboration`, `phase4-03-vector-interpretation`, `phase4-04-partial-interpretation`
      - **Phase 5 (Surface Facades & Standard Effect Battery):** `phase5-01-heftia-unary-facade`, `phase5-02-kernmantle-binary-facade`, `phase5-03-effect-battery`
    - All test suites passing cleanly with zero compiler warnings under `-Wall -Wcompat -Wredundant-constraints` and zero partial functions (`head`, `tail`, `fromJust`, `!!`).

---

## Active Frontier

- [ ] **Phase 6: Formal Verification, Soundness Certification & Benchmarking**
  - **Milestone 6.1: GHC Core Inspection (`inspection-testing`)**
    - Verify `sendPolyF` and `sendPolyH` compile to literal unboxed `Word#` constants with zero residual dictionaries.
    - Verify `elaborateAll` and `interpretAll` compile to direct `indexSmallArray#` primitive reads without linear branch cascades.
  - **Milestone 6.2: Institutional Satisfaction & Reduct Property Tests (`QuickCheck`)**
    - Property-test Reduct-Evaluation Identity: $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$ across randomized row permutations.
    - Property-test Static-Elaboration Commutativity: verify metadata extracted by `StaticPenelope` is identical before and after `elaborateAll`.
  - **Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite**
    - Port Heftia's `UnliftIO`, `Concurrent`, and `Coroutine` + `Local`/`Catch` interaction test suites.
    - Certify scoped effects unrolled during `elaborateAll` never leak handles or corrupt state across multi-shot coroutines.
  - **Milestone 6.4: Allocation & Throughput Benchmarks (`tasty-bench`)**
    - Benchmark deep effect stacks ($D = 30$ first-order, $H = 15$ higher-order) and wide register frames ($m, n = 20$ ports).
    - Compare throughput, RSS, and nursery allocations against `heftia`, `effectful`, and `kernmantle`.
