<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# penelope — Design Decisions & Invariants (`PENELOPE_DESIGN.md`)

**Status:** CANONICAL DESIGN MONOGRAPH · v1.0 · 2026-10-31 · blessed  
**Parent Strategy:** [`DOC_STRATEGY.md`](DOC_STRATEGY.md)  
**Implementation Plan:** [`IMPLEMENTATION_PLAN.md`](IMPLEMENTATION_PLAN.md)  
**Documentation Index:** [`../INDEX.md`](../INDEX.md)

---

## 1. Architectural Decisions

### Decision D1: Modular Multi-Package Decomposition
The workspace is cleanly divided into three packages:
- `penelope-core`: Syntax, routing, Coyoneda leaf fusion, frame wire, and two-phase execution engine.
- `penelope-static`: Generalized Cayley static analysis, schema extraction, and per-port liveness analysis.
- `penelope-effects`: Surface compatibility facades (`Eff`, `Rope`) and standard $(m,n,k,r)$-ary effect battery.

### Decision D2: Unpacked GADT Representation on the Hot Path
To prevent intermediate 1-element wrapper allocations during monadic or polycategorical binding, the variant tag (`Word#`) and payload (`Any`) are unpacked directly into `Penelope` GADT constructors (`OpF`, `OpH`), while Polytropos `SmallArray` records are reserved for multi-branch continuations, region bundles, and handler dispatch tables.

### Decision D3: Pure Static Routing via Reified Row Morphisms
Pure wire permutations are reified as compile-time synthesized `RowMorph` index arrays (`PrimArray Int`). Sequential composition of pure routes (`Route m1 >>> Route m2`) immediately fuses at construction time via `composeRowMorph` in $O(|outs|)$ without runtime allocations.

### Decision D4: Frame-Rule Automatic Port Wiring
Stages communicate over an explicit register frame. `(>&>)` utilizes `FrameWire` to pull inputs from the live frame, scatter updated outputs, and preserve untouched ports in $O(|frame|)$ time.

### Decision D5: Two-Phase Separation (Weaving vs Unweaving)
Execution is strictly partitioned into:
1. **Weaving (`elaborateAll`):** Single-pass bottom-up elaboration of higher-order scoped sub-regions in `eh` into pure first-order operations in `ef`.
2. **Unweaving (`interpretAll`):** Bulk vector-dispatched algebraic interpretation over `ef`.  
This guarantees Heftia's soundness property: scoped state and dynamic handlers cannot leak across multi-shot continuations.

### Decision D6: Zero-Traversal Weakening via Institutional Satisfaction
Subprograms are embedded into larger effect contexts without walking the syntax tree to rewrite tags. At `ScopeRow` boundaries, the handler vector is reduced contravariantly ($\sigma^*(\mathcal{H}) = \text{gatherRecord } \sigma \text{ } \mathcal{H}$) in $O(|eh_1| + |ef_1|)$ time, satisfying the institutional satisfaction condition with zero tree reallocation.

### Decision D7: Hypergraph Pre-Execution Static Analysis
Because branch continuations and scoped sub-regions remain explicit nodes in the GADT outside opaque closures, a generalized Cayley analyzer (`StaticPenelope`) traverses the hypergraph prior to execution to extract database schemas, capability strands, CLI arguments, and calculate exact per-port liveness (last-use lifetimes).

### Decision D8: Dual Surface Specializations
Heftia's unary monadic style (`Eff eh ef a`) and Kernmantle's binary profunctor arrow style (`Rope eh ef a b`) are provided as zero-cost `newtype` projections over the $(m,n,k,r)$-ary core engine.

### Decision D9: Strict Physical Verification Discipline
All changes must satisfy the five ecosystem quality gates: HLint (0 hints), full Cabal test suite, Tagref cross-reference integrity (0 dangling tags), Markdown link integrity (0 broken links), and CLOC comment density (>= 50% in `src/`).

### Decision D10: GHC Plugin-Assisted Index Synthesis
`polytropos-plugin` synthesizes literal indices, `RowMorph` permutation arrays, and `FrameWire` plans during typechecking, eliminating runtime dictionary search and type-level Peano cascades.

---

## 2. In-Code Design Notes & Verification Invariants

### Note [Core Architecture]
[tag:note-core-architecture]

Defines foundational design invariants for `penelope`: an $(m, n, k, r)$-ary polycategorical effect system generalizing Heftia's higher-order algebraic effects and Kernmantle's braided binary pipelines using Polytropos unboxed storage.

### Note [Unpacked Polycategorical Representation]
[tag:note-unpacked-representation]

Unpacks the Polytropos variant tag (`Word#`) and `Any` payload directly into GADT constructors (`OpF`, `OpH`) to avoid nursery allocations of 1-element wrapper heap objects on the hot bind path.

### Note [Reified Row Morphisms and Gather-Scatter]
[tag:note-row-morph-routing]

Reifies port wire permutations as unboxed index arrays (`RowMorph`), enabling $O(|outs|)$ route peephole composition and $O(1)$ gather/scatter operations over live register frames.

### Note [Frame-Rule Automatic Wire Synthesis]
[tag:note-frame-rule-wiring]

Synthesizes compile-time register frame gather and scatter plans (`FrameWire`), allowing stages to pull arguments from and return outputs to an active register frame without tuple-nesting plumbing.

### Note [Two-Phase Weaving and Unweaving]
[tag:note-two-phase-engine]

Partitions execution into single-pass bottom-up higher-order region elaboration (`elaborateAll`) followed by bulk vector-dispatched first-order algebraic interpretation (`interpretAll`), guaranteeing soundness across multi-shot continuations.

### Note [Institutional Satisfaction and Zero-Traversal Weakening]
[tag:note-satisfaction-weakening]

Implements zero-traversal row weakening via Goguen-Burstall institutional reducts ($\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$), performing an $O(|eh_1| + |ef_1|)$ contravariant handler gather at `ScopeRow` boundaries rather than walking the AST.

### Note [Static Reflection and Cayley Analysis]
[tag:note-static-cayley]

Provides generalized Cayley static analysis (`StaticPenelope`) over transparent GADT nodes to extract schemas, capability strands, and per-port borrow lifespans prior to pipeline execution.

### Note [Zero-Cost Surface Specializations]
[tag:note-surface-facades]

Provides zero-overhead bidirectional embedding of Heftia's unary `Eff` and Kernmantle's binary `Rope` facades as `newtype` wrappers around `Penelope`.
