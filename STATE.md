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
   - All 5 physical verification gates passing cleanly via `./bin/penelope-verify` (0 HLint hints, full test suite passing, 0 broken links, valid Tagref references, >= 50% comment density).
   - Remote replication topology configured across sovereign (`rad`) and commercial/community mirrors (`github`, `gitcode`, `framagit`, `disroot`).

2. **Canonical Architecture & Implementation Plan Blessed:**
   - Grounded canonical implementation plan in [`docs/notes/IMPLEMENTATION_PLAN.md`](docs/notes/IMPLEMENTATION_PLAN.md) spanning all six engineering phases.
   - Grounded canonical design decisions (D1–D10) and Tagref-anchored notes in [`docs/notes/PENELOPE_DESIGN.md`](docs/notes/PENELOPE_DESIGN.md).
   - Operational roadmap updated in [`PLAN.md`](PLAN.md).

---

## Active Frontier

- [ ] **Phase 1: `polytropos-core` & `polytropos-plugin` Prerequisites**
  - Implement `RowMorph` representation and primitive gather/scatter loops in `polytropos-core`.
  - Implement `KnownRowMorph` and `FrameWire` solvers in `polytropos-plugin`.
  - Extend index synthesis to higher-kinded effect rows (`Row PolyEffect`, `Row PolyHEffect`).
- [ ] **Phase 2: Core $(m,n,k,r)$-ary Syntax & Unpacked Representation (`penelope-core`)**
  - Implement `Penelope` GADT with unpacked `Word#` variant tags and `Any` payloads in `Penelope.Core.Syntax`.
  - Implement pure route peephole composition and smart constructors `sendPolyF` / `sendPolyH`.
- [ ] **Continuous Gate Verification:**
  - Maintain 5-gate physical verification via `./bin/penelope-verify`.
