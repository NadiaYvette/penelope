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
 5. **Phase 6 Milestone 6.1 (GHC Core Inspection) Complete & Merged:**
    - Established `inspection` test suite in `packages/penelope-core` running under GHC 9.14.1 with `-O1`.
    - Formally certified 10 inspection obligations via `inspection-testing`:
      - `sendPolyF` and `sendPolyH` compile to literal unboxed `Word#` constants with zero residual dictionaries (`ConstructorIndex`, `KnownRowMorph`) and pass zero dictionaries in Core (`hasNoTypeClasses`).
      - `elaborateAll` and `interpretAll` dispatch via branchless `indexSmallArray#` vector reads without linear search cascades (`doesNotUse 'eqWord#`).
    - Fast-forward merged via `yamaarashi-exec` task packet `phase6-01-core-inspection` (commit `55638fc`).

 6. **Phase 6 Milestone 6.2 (Institutional Satisfaction & Reduct Property Tests) Complete & Merged:**
    - Established `properties` test suite in `packages/penelope-static` running 500 QuickCheck randomized trials.
    - Formally certified Goguen-Burstall satisfaction condition across randomized row embeddings and permutations: $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$.
    - Certified Functorial Reduct Transitivity: $(\sigma_2 \circ \sigma_1)^*(\mathcal{H}) \equiv \sigma_1^*(\sigma_2^*(\mathcal{H}))$.
    - Certified Static-Elaboration Commutativity and structural port liveness invariance under `elaborateAll`.
    - Fast-forward merged via `yamaarashi-exec` task packet `phase6-02-reduct-properties` (commit `86a9ef2`).

---

## Active Frontier

- **Phase 6: Formal Verification, Soundness Certification & Benchmarking**
  - [x] **Milestone 6.1: GHC Core Inspection (`inspection-testing`)**
    - Verify `sendPolyF` and `sendPolyH` compile to literal unboxed `Word#` constants with zero residual dictionaries.
    - Verify `elaborateAll` and `interpretAll` compile to direct `indexSmallArray#` primitive reads without linear branch cascades.
  - [x] **Milestone 6.2: Institutional Satisfaction & Reduct Property Tests (`QuickCheck`)**
    - Property-test Reduct-Evaluation Identity: $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$ across randomized row permutations.
    - Property-test Static-Elaboration Commutativity: verify metadata extracted by `StaticPenelope` is identical before and after `elaborateAll`.
  - [ ] **Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite**
    - Port Heftia's `UnliftIO`, `Concurrent`, and `Coroutine` + `Local`/`Catch` interaction test suites.
    - Certify scoped effects unrolled during `elaborateAll` never leak handles or corrupt state across multi-shot coroutines.
  - [ ] **Milestone 6.4: Allocation & Throughput Benchmarks (`tasty-bench`)**
    - Benchmark deep effect stacks ($D = 30$ first-order, $H = 15$ higher-order) and wide register frames ($m, n = 20$ ports).
    - Compare throughput, RSS, and nursery allocations against `heftia`, `effectful`, and `kernmantle`.

