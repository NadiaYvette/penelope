<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# docs/INDEX.md — Corpus Inventory and Reading Map

**Status:** LIVING DOCUMENT · v1.0 · 2026-11-03  
**Parent Strategy:** [`notes/DOC_STRATEGY.md`](notes/DOC_STRATEGY.md)  
**Quick Start:** [`../README.md`](../README.md) · [`NEWBIE_GUIDE.md`](NEWBIE_GUIDE.md) · [`TUTORIAL.md`](TUTORIAL.md) · [`MAINTAINER_GUIDE.md`](MAINTAINER_GUIDE.md)

---

## Part 1 — Inventory

| Path | Genre | Stage | Tier | Flags | Description |
| :--- | :--- | :--- | :--- | :--- | :--- |
| `../README.md` | orientation | live | cross-cutting | — | Root architectural overview, Homeric hexameter framing, matrimonial Polytropos pairing, memory physics, and quick start |
| `NEWBIE_GUIDE.md` | guide | live | cross-cutting | — | Token-efficient progressive disclosure funnel, `./tags` symbol jump, port/row mental model, and first 15 minutes walkthrough |
| `TUTORIAL.md` | tutorial | live | cross-cutting | — | 6-lesson hands-on interactive tutorial: Heftia unary track (`Eff`), Kernmantle binary track (`Rope`), 7 polycategorical superpowers, and full capstone (`OrderProcessing`) |
| `MAINTAINER_GUIDE.md` | guide | live | cross-cutting | — | Engineering governance, 5-gate physical verification (`./bin/penelope-verify`), GHC Core inspection testing, and tasty-bench regression harness |
| `notes/PENELOPE_DESIGN.md` | design | blessed | cross-cutting | — | Canonical design monograph: decisions D1–D10, in-code verification invariants, and unboxed memory physics |
| `notes/IMPLEMENTATION_PLAN.md` | specification | blessed | cross-cutting | — | Detailed 6-phase implementation roadmap, category-theoretic foundations, and 7 architectural invariants |
| `notes/DOC_STRATEGY.md` | strategy | blessed | cross-cutting | — | Documentation governance, 4-audience map, canonicity per fact class, §4 precedence rule, and Classical Hermeneutic Pattern |
| `notes/COMMIT_ATTRIBUTION.md` | convention | blessed | cross-cutting | — | Linux kernel RFC 2822 trailer format (`Assisted-by:`), DCO boundaries, and hook enforcement |
| `transcripts/penelope-effect-system.md` | transcript | archive | cross-cutting | — | Foundational ideation dialogue establishing the $(m, n, k, r)$-ary polycategorical effect system model |
| `transcripts/gemini-heftia-kernmantle-polytropos.md` | transcript | archive | cross-cutting | — | Research dialogue synthesizing Heftia scoped semantics, Kernmantle arrow routing, and Polytropos unboxed rows |
| `task_packets/phase2-01-syntax-gadt.yaml` | task | implemented | core | — | Task packet: Unpacked $(m,n,k,r)$-ary polycategorical syntax GADT |
| `task_packets/phase2-02-route-optimizer.yaml` | task | implemented | core | — | Task packet: `RowMorph` static route peephole optimizer and permutation arrays |
| `task_packets/phase2-03-smart-constructors.yaml` | task | implemented | core | — | Task packet: Category, Profunctor, and Arrow smart constructors |
| `task_packets/phase3-01-sequential-composition.yaml` | task | implemented | core | — | Task packet: Sequential polycategorical composition combinators |
| `task_packets/phase3-02-frame-rule-wiring.yaml` | task | implemented | core | — | Task packet: Frame-rule automatic register wiring combinator `(>&>)` |
| `task_packets/phase3-03-disjunctive-fanout.yaml` | task | implemented | core | — | Task packet: Multi-exit disjunctive fan-out and labeled branch switching (`Switch`) |
| `task_packets/phase3-04-row-weakening.yaml` | task | implemented | core | — | Task packet: Zero-traversal effect row weakening via institutional satisfaction |
| `task_packets/phase4-01-static-analysis.yaml` | task | implemented | static | — | Task packet: Generalized Cayley hypergraph analyzer (`StaticPenelope`) |
| `task_packets/phase4-02-higher-order-elaboration.yaml` | task | implemented | core | — | Task packet: Bottom-up higher-order region elaboration engine (`elaborateAll`) |
| `task_packets/phase4-03-vector-interpretation.yaml` | task | implemented | core | — | Task packet: Bulk vector-dispatched first-order algebraic interpreter (`interpretAll`) |
| `task_packets/phase4-04-partial-interpretation.yaml` | task | implemented | core | — | Task packet: Partial interpretation and static circuit residualization (`residualize`) |
| `task_packets/phase5-01-heftia-unary-facade.yaml` | task | implemented | effects | — | Task packet: Ergonomic Heftia-style unary monadic facade (`Eff eh ef a`) |
| `task_packets/phase5-02-kernmantle-binary-facade.yaml` | task | implemented | effects | — | Task packet: Ergonomic Kernmantle-style binary arrow facade (`Rope eh ef a b`) |
| `task_packets/phase5-03-effect-battery.yaml` | task | implemented | effects | — | Task packet: Multi-region control battery (`While`, `Switch`, `Saga`, `Local`, `Catch`) |
| `task_packets/phase6-01-core-inspection.yaml` | task | implemented | core | — | Task packet: GHC Core inspection test suite (`packages/penelope-core/test/Inspection.hs`) |
| `task_packets/phase6-02-reduct-properties.yaml` | task | implemented | static | — | Task packet: Institutional satisfaction and reduct property tests (`Properties.hs`) |
| `task_packets/phase6-03-scoped-semantics-concurrency.yaml` | task | implemented | effects | — | Task packet: Heftia scoped semantics and delimited concurrency test battery |
| `task_packets/phase6-04-benchmarks.yaml` | task | implemented | effects | — | Task packet: Allocation and throughput benchmark suite (`packages/penelope-effects/bench/Main.hs`) |
| `../packages/penelope-core/README.md` | orientation | live | core | — | Package monograph for `penelope-core`: syntax, routing, wiring, and two-phase engine |
| `../packages/penelope-static/README.md` | orientation | live | static | — | Package monograph for `penelope-static`: Cayley analysis, schemas, and port liveness |
| `../packages/penelope-effects/README.md` | orientation | live | effects | — | Package monograph for `penelope-effects`: surface facades, control battery, and benchmarks |

---

## Part 2 — Reading Paths

### 1. Newcomer to Penelope
`README.md` (Homeric context, architectural overview, memory physics)  
→ `NEWBIE_GUIDE.md` (progressive disclosure funnel, `./tags` lookup, port/row mental model)  
→ `TUTORIAL.md` (hands-on 6-lesson tutorial from basic `Eff` and `Rope` to capstone order processing)  
→ `notes/PENELOPE_DESIGN.md` (decisions D1–D10 and verification invariants).

### 2. Maintainer & Release Lead
`MAINTAINER_GUIDE.md` (governance, 5-gate verification runner `./bin/penelope-verify`, inspection testing, benchmarks)  
→ `notes/DOC_STRATEGY.md` (documentation lifecycle, canonicity per fact class, §4 precedence rule)  
→ `notes/PENELOPE_DESIGN.md` (decisions D1–D10, open questions)  
→ `notes/IMPLEMENTATION_PLAN.md` (milestone verification and PVP release discipline).

### 3. Effect System Author & Library Porter
`TUTORIAL.md` §2 (The Heftia Track)  
→ `packages/penelope-effects/src/Penelope/Effects/Unary.hs` (zero-cost `Eff` newtype projection)  
→ `notes/PENELOPE_DESIGN.md` §1 Decision D5 (Two-phase weaving vs unweaving)  
→ `packages/penelope-core/src/Penelope/Core/Engine/Elaborate.hs` (higher-order region elaboration)  
→ `packages/penelope-core/src/Penelope/Core/Engine/Interpret.hs` ($O(1)$ jump-table dispatch).

### 4. Pipeline & Systems Architect
`TUTORIAL.md` §3 (The Kernmantle Track) & §4 (Polycategorical Superpowers)  
→ `packages/penelope-core/src/Penelope/Core/Route.hs` (`RowMorph` index arrays and construction-time peephole fusion)  
→ `packages/penelope-core/src/Penelope/Core/Wiring.hs` (`FrameWire` register frame gather and scatter)  
→ `packages/penelope-static/src/Penelope/Static.hs` (generalized Cayley hypergraph reflection and port liveness)  
→ `packages/penelope-core/src/Penelope/Core/Engine/Partial.hs` (circuit residualization).

### 5. Category Theorist & Formal Logician
`notes/PENELOPE_DESIGN.md` §1 Decision D6 (Zero-traversal weakening via Goguen-Burstall institutional satisfaction)  
→ `packages/penelope-static/test/Properties.hs` (QuickCheck property tests verifying $\operatorname{eval}_J(\mathcal{H}, \sigma_*(P)) = \operatorname{eval}_I(\sigma^*(\mathcal{H}), P)$)  
→ `transcripts/penelope-effect-system.md` (mathematical formulation of the $(m, n, k, r)$-ary polycategory and Hom-adjunction)  
→ `transcripts/gemini-heftia-kernmantle-polytropos.md` (categorical synthesis of freer monads, profunctor arrows, and unboxed rows).

### 6. AI Coding Assistant
`AGENTS.md` (bounded ~4 KB orientation projection)  
→ `NEWBIE_GUIDE.md` (progressive disclosure funnel, `./tags` navigation, `<15` token lookup)  
→ `docs/INDEX.md` (this inventory)  
→ `notes/PENELOPE_DESIGN.md` §2 (In-code design notes and verification invariants).

### 7. Anyone Modifying Documentation
`notes/DOC_STRATEGY.md` §4 (Precedence rule: code wins at runtime; notes corrected regardless)  
→ `notes/DOC_STRATEGY.md` §12 (The Classical Hermeneutic Pattern: 4-step template for classical textual anchors and philological root verb analysis)  
→ `notes/COMMIT_ATTRIBUTION.md` (RFC 2822 git trailers: `Assisted-by:`, never `Co-authored-by:` for AI tools)  
→ `./bin/penelope-linkcheck` and `./bin/penelope-tagref` (deterministic validation).

### 8. Task Packet Implementer & Autonomous Worker
`notes/IMPLEMENTATION_PLAN.md` (milestone roadmap)  
→ `docs/task_packets/` (`phase2-01-syntax-gadt.yaml` through `phase6-04-benchmarks.yaml`)  
→ Executed sequentially via `bin/yamaarashi-exec run <packet.yaml>` and verified with `./bin/penelope-verify`.

---

## Part 3 — Conventions

1. **Path Vocabulary:** Defined in `notes/DOC_STRATEGY.md` §5. `notes/` contains design monographs and plans; `task_packets/` contains machine-executable task definitions; `transcripts/` contains historical ideation dialogues.
2. **Stable Identifiers:** Design decisions (`D1`–`D10`) and verification tags (`tag:note-...`) are canonical identifiers surviving renames.
3. **Precedence:** Code wins at runtime; documentation notes must be kept current in the same commit set (`notes/DOC_STRATEGY.md` §4).
