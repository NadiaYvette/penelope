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

 4. **Phases 2–5 Task Packets & Verification Gates Ready:**
    - Authored 14 standalone `yamaarashi-exec` task packets in [`docs/task_packets/`](docs/task_packets/) covering all milestones in Phases 2, 3, 4, and 5.
    - Authored 14 corresponding verification gate scripts in [`scripts/tasks/`](scripts/tasks/).
    - All packets validated against `yamaarashi-flow` YAML parser and all 114 Tagref references pass mechanical audit.

---

## Active Frontier

- [ ] **Phase 2: Core $(m,n,k,r)$-ary Syntax & Unpacked Representation (`penelope-core`)**
  - Execute `phase2-01-syntax-gadt`: GADT with unpacked `Word#` variant tags and `Any` payloads in `Penelope.Core.Syntax`.
  - Execute `phase2-02-route-optimizer`: Pure route peephole composition and identity elision in `Penelope.Core.Route`.
  - Execute `phase2-03-smart-constructors`: Zero-overhead smart constructors `sendPolyF` / `sendPolyH` in `Penelope.Core.Send`.
- [ ] **Phase 3: Polycategorical Composition & The Frame Rule (`Penelope.Core.Wiring`)**
  - Execute `phase3-01-sequential-composition`, `phase3-02-frame-rule-wiring`, `phase3-03-disjunctive-fanout`, `phase3-04-row-weakening`.
- [ ] **Phase 4: Two-Phase Weaving & Interpretation Engine (`Penelope.Core.Engine` & `penelope-static`)**
  - Execute `phase4-01-static-analysis`, `phase4-02-higher-order-elaboration`, `phase4-03-vector-interpretation`, `phase4-04-partial-interpretation`.
- [ ] **Phase 5: Surface Ergonomics & Compatibility Facades (`penelope-effects`)**
  - Execute `phase5-01-heftia-unary-facade`, `phase5-02-kernmantle-binary-facade`, `phase5-03-effect-battery`.
