<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# penelope — Implementation Plan (`IMPLEMENTATION_PLAN.md`)

**Status:** SPECIFICATION · v1.0 · 2026-10-31 · blessed  
**Scope:** Canonical Implementation Plan for the Penelope Polycategorical Effect System.  
**Theoretical Heritage:** Generalization of Heftia's two-phase higher-order algebraic effects and Kernmantle's braided binary pipelines into a Polytropos-backed $(m, n, k, r)$-ary polycategorical effect system.  
**Canonical Anchors:**
- Strategic Roadmap: [`PLAN.md`](../../PLAN.md)
- Operational State: [`STATE.md`](../../STATE.md)
- Design Decisions & Invariants: [`PENELOPE_DESIGN.md`](PENELOPE_DESIGN.md) (Decisions D1–D10)
- Research Archive: [`transcripts/penelope-effect-system.md`](../transcripts/penelope-effect-system.md)
- Documentation Strategy: [`DOC_STRATEGY.md`](DOC_STRATEGY.md) §4 (Code wins at runtime; notes corrected regardless)

---

## 1. Guiding Principles & Category-Theoretic Foundations

The name **`penelope`** invokes the Homeric heroine of the Odyssey who maintained sovereignty by weaving a shroud by day and unweaving it by night. In our effect system architecture, `penelope` mirrors this mythic rhythm:
1. **Weaving:** Statically analyzing the pipeline hypergraph and unrolling higher-order scoped sub-regions into first-order algebraic operations.
2. **Unweaving:** Executing first-order algebraic operations with branchless, bulk-vector-dispatched interpretation.

Penelope generalizes existing effect systems along four dimensional axes:
- **Heftia** provides sound higher-order scoped algebraic effect semantics via two-phase elaboration, but restricts computations to unary monadic continuations ($1 \to 1$).
- **Kernmantle** provides multi-strand pipelines and pre-execution static analysis via profunctors and arrows, but suffers from tuple-nesting arrow plumbing (`(a, (b, c))`), $O(N)$ type-level search (`vinyl`), and restricted higher-order scoping.
- **Polytropos** collapses structural products and sums into a flat generic representation (`Rep !Word !(SmallArray Any)`), enabling $O(1)$ literal slot indexing and compile-time dictionary elimination.

Penelope fuses these three paradigms into an **$(m, n, k, r)$-ary polycategorical effect engine**:

```
        r Higher-Order Scoped Regions (regions :: Row PortSig)
                          │
                          ▼
m Input Ports      ┌──────────────┐      n Output Ports
─────────────────► │   Penelope   │ ─────────────────► (Conjunctive Frame)
(ins :: Row Type)  │  eh  ef  ins │ (outs :: Row Type)
                   │     outs     │
                   └──────────────┘
                          │
                          ▼
     k Disjunctive Control Exits (exits :: Row (Row Type))
     (Hom-adjoined to Record of k explicit Branch Continuations)
```

### The Seven Architectural Invariants

1. **Unpacked Nursery Representation:**  
   Monadic binding occurs on the hot path. Wrapping every effect invocation in a heap-allocated `Rep` (`Rep !Word !(SmallArray (f Any))`) would allocate a 1-element `SmallArray#` per bind. Penelope unrolls the `Word#` variant tag and `Any` payload directly into GADT constructors (`OpF`, `OpH`), reserving flat `SmallArray` records for multi-branch continuations, region bundles, and dispatch vectors.
   (Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation]).

2. **Pure Static Port Routing & Peephole Fusion:**  
   Tuple shuffling (`arr (\(a, (b, c)) -> (b, a))`) is eliminated. Pure wire permutations are reified as compile-time synthesized `RowMorph` index arrays (`PrimArray Int`). Composition of routes (`Route m1 >>> Route m2`) immediately fuses at construction time in $O(|outs|)$ via `composeRowMorph` with zero runtime allocations.
   (Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing]).

3. **Frame-Rule Automatic Port Wiring (`>&>`):**  
   Operations communicate across an explicit register frame. Given `stage1` producing `outs1` and `stage2` requiring `ins2` (a subset of `ins1 \cup outs1`), Penelope synthesizes static gather and scatter maps via `FrameWire` to pull arguments from the live register frame and write back results, leaving unmentioned ports untouched.
   (Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring]).

4. **Two-Phase Soundness Separation:**  
   Execution is strictly partitioned into:
   - **Phase 1 (Weaving / Elaboration):** Single-pass bottom-up elaboration (`elaborateAll`) that translates all higher-order scoped effects in `eh` down to pure first-order operations in `ef`.
   - **Phase 2 (Unweaving / Interpretation):** Vector-dispatched interpretation (`interpretAll`) over first-order `ef`.  
   This separation mathematically prevents scoped dynamic handler state from leaking or corrupting across multi-shot coroutine or backtracking continuations.
   (Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine]).

5. **Zero-Traversal Weakening via Institutional Satisfaction:**  
   Subprograms are embedded into wider effect rows without traversing the syntax tree to re-tag `Word#` indices. Using Goguen-Burstall institutions and the satisfaction condition ($\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$), embedding is realized as an $O(|eh_1| + |ef_1|)$ contravariant gathering of the handler record (`gatherRecord σ H`) at `ScopeRow` boundary nodes.
   (Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening]).

6. **Static Reflection & Cayley Analysis:**  
   Because branch continuations and scoped sub-regions are explicit nodes outside opaque closures, a generalized Cayley analyzer (`StaticPenelope`) traverses the hypergraph prior to execution to accumulate database schemas, capability strands, CLI options, and calculate exact per-port liveness (last-use lifetimes).
   (Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley]).

7. **Zero-Cost Surface Specializations:**  
   Standard unary monadic code (`Eff eh ef a`) and binary profunctor arrows (`Rope eh ef a b`) are provided as zero-cost `newtype` projections over the exact same $(m,n,k,r)$-ary core engine.
   (Note [Zero-Cost Surface Specializations] [ref:note-surface-facades]).

---

## 2. Multi-Package Architecture & Package Topology

The repository is structured as a fine-grained, modular multi-package workspace:

| Package | Role & Domain Boundary | Key Dependencies |
| :--- | :--- | :--- |
| **`penelope-core`** | Unpacked $(m,n,k,r)$-ary syntax, static `RowMorph` routing, frame-rule wiring, Coyoneda leaf fusion, contravariant handler reducts, and two-phase elaboration/interpretation engine. | `polytropos-core`, `polytropos-plugin`, `primitive` |
| **`penelope-static`** | Generalized Cayley static analyzer (`StaticPenelope f eh ef ins outs`) for pre-execution extraction of schemas, capability requirements, and per-port liveness/borrow lifespans. | `penelope-core`, `selective` |
| **`penelope-effects`** | Surface facades (`Eff`, `Rope`), router derivation (`deriveRouter`), and standard $(m,n,k,r)$-ary effect battery (`While`, `Switch`, `Saga`, `Coroutine`, `NonDet`, `UnliftIO`, `Provider`). | `penelope-core`, `penelope-static`, `profunctors` |

---

## 3. The Six-Phase Engineering Roadmap

```
Phase 1: Polytropos Prerequisites (RowMorph, FrameWire, Poly-Kinded Rows)
   │
   ▼
Phase 2: Core (m,n,k,r)-ary Syntax & Unpacked Data Representation (penelope-core)
   │
   ▼
Phase 3: Polycategorical Composition & The Frame Rule (Penelope.Core.Wiring)
   │
   ▼
Phase 4: Two-Phase Weaving & Interpretation Engine (Penelope.Core.Engine & penelope-static)
   │
   ▼
Phase 5: Surface Ergonomics & Compatibility Facades (penelope-effects)
   │
   ▼
Phase 6: Formal Verification, Soundness Certification & Benchmarking
```

### Phase 1: `polytropos-core` & `polytropos-plugin` Prerequisites

Before assembling `penelope-core`, extend `polytropos-core` and `polytropos-plugin` with three primitives required for $O(1)$ port wiring and multi-sorted effect rows:

- [ ] **Milestone 1.1: Reified Row Morphisms (`RowMorph`)**
  - Implement unboxed representation: `newtype RowMorph (r1 :: Row k) (r2 :: Row k) = RowMorph (PrimArray Int)` in `Polytropos.RowMorph`.
  - Extend `Polytropos.Plugin` to solve `KnownRowMorph r1 r2` by computing compile-time slot permutation vectors and emitting static unboxed CAF constants.
  - Implement primitive gather/scatter loops over `Polytropos.Core.Rep`:
    - `gatherRep :: RowMorph sub full -> Rep f full -> Rep f sub`
    - `scatterRep :: RowMorph out full -> Rep f out -> Rep f full -> Rep f full`
  - Implement $O(|r1|)$ route composition: `composeRowMorph :: RowMorph r1 r2 -> RowMorph r2 r3 -> RowMorph r1 r3`.

- [ ] **Milestone 1.2: Frame-Rule Unification Constraint Solver (`FrameWire`)**
  - Define `FramePlan` encoding input gather map, output scatter map, and untouched passthrough slots.
  - Extend `Polytropos.Plugin` to solve `FrameWire frameIn opIn opOut frameOut`, unifying `frameOut` with `RowOverwrite frameIn opOut`.
  - Teach `Polytropos.Plugin` to defer unsolved `IsLabel` / `HasField` constraints when the target row is an ununified `TyVarTy` inside polycategorical `proc` blocks.

- [ ] **Milestone 1.3: Poly-Kinded Effect Row Indexing**
  - Generalize `FieldIndex` and `ConstructorIndex` in `Polytropos.Plugin` to support higher-kinded effect rows:
    - `Row PolyEffect` (where `PolyEffect = Row Type -> Row (Row Type) -> Type`)
    - `Row PolyHEffect` (where `PolyHEffect = (PortSig -> Type) -> Row PortSig -> Row Type -> Row (Row Type) -> Type`)
  - Ensure index solvers emit unboxed `Word#` / `Int#` literals directly into Core without residual dictionaries.

---

### Phase 2: Core $(m,n,k,r)$-ary Syntax & Unpacked Data Representation (`penelope-core`)

Implement foundational data structures in `packages/penelope-core/src/Penelope/Core/Syntax.hs`:

- [x] **Milestone 2.1: Type-Level Signatures & Core Syntax GADT**
  - Define `PortSig`: `data PortSig = PortSig (Row Type) (Row Type)`.
  - Define `BranchCont`: `newtype BranchCont m (outs :: Row Type) = BranchCont { runBranchCont :: Record Identity outs -> m }`.
  - Define `Region`: `newtype Region eh ef sig = Region (Penelope eh ef rIns rOuts)`.
  - Implement `Penelope eh ef ins outs` GADT with unpacked variant tags:
    - `Route !(RowMorph outs ins)`
    - `LiftPure !(RowMorph opIns ins) !(Record Identity opIns -> Record Identity opOuts) !(BranchCont (Penelope eh ef ins outs) opOuts)`
    - `OpF Word# !(RowMorph opIns ins) Any !(Record (BranchCont (Penelope eh ef ins outs)) exits)`
    - `OpH Word# !(RowMorph opIns ins) !(Record (Region eh ef) regions) Any !(Record (BranchCont (Penelope eh ef ins outs)) exits)`
    - `ScopeRow !(RowMorph ehSub ehFull) !(RowMorph efSub efFull) !(Penelope ehSub efSub ins outs)`

- [x] **Milestone 2.2: Pure Route Peephole Optimizer**
  - Implement smart constructor `route :: RowMorph outs ins -> Penelope eh ef ins outs`.
  - Implement peephole fusion in sequential composition: `Route m1 >>> Route m2` fuses to `Route (composeRowMorph m1 m2)`.
  - Implement identity elision: if `m` is the identity morphism, `Route m` acts as an immediate no-op.

- [x] **Milestone 2.3: Zero-Overhead Smart Constructors (`sendPolyF`, `sendPolyH`)**
  - Implement `sendPolyF`: compile-time tag lookup via `ConstructorIndex l ef`, injecting operation payload via `unsafeCoerce#` with static `RowMorph` gather wiring.
  - Implement `sendPolyH`: compile-time tag lookup via `ConstructorIndex l eh`, packaging sub-regions into `Record (Region eh ef) regions`.

---

### Phase 3: Polycategorical Composition & The Frame Rule (`Penelope.Core.Wiring`)

Implement the polycategorical wiring combinators in `packages/penelope-core/src/Penelope/Core/Wiring.hs`:

- [x] **Milestone 3.1: Sequential Composition & Coyoneda Leaf Fusion (`>>>`)**
  - Implement `seqPenelope :: Penelope eh ef a b -> Penelope eh ef b c -> Penelope eh ef a c`.
  - Implement Coyoneda-style continuation fusion over `Record (BranchCont ...) exits` using `Polytropos.Record.rmapFused`, avoiding intermediate `SmallArray#` allocations during chained binds.

- [x] **Milestone 3.2: Frame-Rule Automatic Port Wiring (`>&>`)**
  - Implement `(>&>) :: (FrameWire frameIn ins1 outs1 frameMid, FrameWire frameMid ins2 outs2 frameOut) => Penelope eh ef ins1 outs1 -> Penelope eh ef ins2 outs2 -> Penelope eh ef frameIn frameOut`.
  - Execute register frame preservation: automatically pull named inputs from the live frame, scatter output results, and preserve untouched ports in $O(|frame|)$ time.

- [x] **Milestone 3.3: Disjunctive Multi-Exit Fanout (`>?<`)**
  - Implement $k$-way exit branching combinator `(>?<)` binding an operation's `exits` row with a `Record` of $k$ downstream handler stages in $O(1)$ by zipping with the slot array.

- [x] **Milestone 3.4: Zero-Traversal Row Weakening (`ScopeRow`)**
  - Implement `weakenRow :: (KnownRowMorph ehSub ehFull, KnownRowMorph efSub efFull) => Penelope ehSub efSub ins outs -> Penelope ehFull efFull ins outs`.
  - Wrap computation in `ScopeRow rowMorph rowMorph` in $O(1)$ without walking the syntax tree.

---

### Phase 4: Two-Phase Weaving & Interpretation Engine (`Penelope.Core.Engine` & `penelope-static`)

Implement the static analysis, higher-order elaboration, and first-order interpretation engines:

- [x] **Milestone 4.1: Static Pre-Execution Analysis (`penelope-static`)**
  - Implement `StaticPenelope f eh ef ins outs = StaticPenelope !(f ()) !(Penelope eh ef ins outs)` in `packages/penelope-static/src/Penelope/Static/Analysis.hs`.
  - Implement AST traversal extracting capability strands, configuration requirements, and schema manifests.
  - Implement per-port liveness analysis (last-use lifetime extraction) across `RowMorph` edges to enable buffer reuse.

- [x] **Milestone 4.2: Phase 1 — Single-Pass Higher-Order Elaboration (`elaborateAll`)**
  - Implement `newtype Elaborator eh' ef h = Elaborator { runElaborator :: ... }`.
  - Implement `elaborateAll :: Record (Elaborator '[] ef) eh -> Penelope eh ef ins outs -> Penelope '[] ef ins outs`.
  - Walk the AST bottom-up in a single pass; at each `OpH tag# inMap regions opAny conts`, look up elaborator in $O(1)$ via `indexSmallArray#` and lower scoped regions into pure first-order operations in `ef`.

- [x] **Milestone 4.3: Phase 2 — Bulk Vector-Dispatched Interpretation (`interpretAll`)**
  - Implement `newtype Handler ef m e = Handler { runHandler :: ... }`.
  - Implement `interpretAll :: Category m => (forall i o. RowMorph o i -> m i o) -> Record (Handler '[] m) ef -> Penelope '[] ef ins outs -> m ins outs`.
  - Dispatch each `OpF tag# inMap opAny conts` via branchless `indexSmallArray#` in $O(1)$ with zero `eqWord#` cascaded comparisons.

- [x] **Milestone 4.4: Partial / Layered Interpretation (`interpretSub`)**
  - Implement `interpretSub` for partial effect stack peeling (e.g., interpreting `State` and `Reader` while preserving `IO`).
  - Utilize `Polytropos.Plugin`'s `RowPartition` to partition handler vectors in $O(1)$ without linear search.

---

### Phase 5: Surface Ergonomics & Compatibility Facades (`penelope-effects`)

Ground user-facing abstractions in `packages/penelope-effects/`:

- [x] **Milestone 5.1: The Heftia Unary Facade (`Penelope.Effects.Unary`)**
  - Define `newtype Eff eh ef a = Eff (Penelope eh ef '[] '["val" := a])`.
  - Implement `Functor`, `Applicative`, and `Monad` for `Eff eh ef`.
  - Provide adapters embedding standard unary GADTs (`f :: Type -> Type`) into `PolyEffect`.

- [x] **Milestone 5.2: The Kernmantle Binary Facade (`Penelope.Effects.Binary`)**
  - Define `newtype Rope eh ef a b = Rope (Penelope eh ef '["in" := a] '["out" := b])`.
  - Implement `Category`, `Arrow`, `ArrowChoice`, `Profunctor`, `Strong`, and `Choice` for `Rope`.
  - Implement `deriveRouter` compiling a `Record` of `Rope` handlers into an $O(1)$ jump table over `Polytropos.Variant`.

- [x] **Milestone 5.3: Native Multi-Region & Multi-Exit Effect Battery**
  - `Penelope.Effects.Control`: Multi-region `While` (`"cond"` + `"body"`), multi-exit `Switch`, and `Saga` (`"try"` + `"compensate"` + `"catch"`).
  - `Penelope.Effects.Coroutine` & `NonDet`: Delimited continuations with frame cloning and suspension.
  - `Penelope.Effects.UnliftIO` & `Provider`: Sound scoped resource management with zero dynamic handler leaks.

---

### Phase 6: Formal Verification, Soundness Certification & Benchmarking

Certify safety, correctness, and performance:

- [x] **Milestone 6.1: GHC Core Inspection (`inspection-testing`)**
  - Verify `sendPolyF` and `sendPolyH` compile to literal unboxed `Word#` constants with zero residual dictionaries.
  - Verify `elaborateAll` and `interpretAll` compile to direct `indexSmallArray#` indexing without linear search.

- [x] **Milestone 6.2: Institutional Satisfaction & Reduct Property Tests (`QuickCheck`)**
  - Property-test the Reduct-Evaluation Identity: $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$ across randomized row permutations.
  - Property-test Static-Elaboration Commutativity: verify metadata extracted by `StaticPenelope` is identical before and after `elaborateAll`.

- [ ] **Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite**
  - Port Heftia's `UnliftIO`, `Concurrent`, and `Coroutine` + `Local`/`Catch` interaction test suites.
  - Certify that scoped effects unrolled during `elaborateAll` never leak handles or corrupt state across multi-shot coroutines.

- [ ] **Milestone 6.4: Allocation & Throughput Benchmarks (`tasty-bench`)**
  - Benchmark deep effect stacks ($D = 30$ first-order, $H = 15$ higher-order) and wide register frames ($m, n = 20$ ports).
  - Compare throughput, RSS, and nursery allocations against `heftia`, `effectful`, and `kernmantle`.

---

## 4. In-Code Design Notes & Verification Invariants

All architectural invariants are grounded via Tagref:

- `Note [Core Architecture]` (`[ref:note-core-architecture]`):  
  Foundational multi-package layout and $(m,n,k,r)$-ary polycategorical contract.
- `Note [Unpacked Polycategorical Representation]` (`[ref:note-unpacked-representation]`):  
  Unpacking `Word#` variant tag and `Any` payload directly into GADT constructors.
- `Note [Reified Row Morphisms and Gather-Scatter]` (`[ref:note-row-morph-routing]`):  
  $O(1)$ static port permutation and Coyoneda route composition.
- `Note [Frame-Rule Automatic Wire Synthesis]` (`[ref:note-frame-rule-wiring]`):  
  Compile-time synthesis of register frame gathering and scattering (`FrameWire`).
- `Note [Two-Phase Weaving and Unweaving]` (`[ref:note-two-phase-engine]`):  
  Single-pass bottom-up region elaboration followed by vector-dispatched interpretation.
- `Note [Institutional Satisfaction and Zero-Traversal Weakening]` (`[ref:note-satisfaction-weakening]`):  
  Contravariant handler reduction at `ScopeRow` boundaries eliminating AST re-tagging.
- `Note [Static Reflection and Cayley Analysis]` (`[ref:note-static-cayley]`):  
  Pre-execution capability accumulation and per-port liveness extraction.
- `Note [Zero-Cost Surface Specializations]` (`[ref:note-surface-facades]`):  
  Isomorphic zero-overhead projections for Heftia unary (`Eff`) and Kernmantle binary (`Rope`).
