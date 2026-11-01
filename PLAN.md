<!--
SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
-->
# penelope — Strategic Roadmap (`PLAN.md`)

**Status:** ACTIVE STRATEGIC ROADMAP · October 2026  
**Scope:** Polytropos-backed $(m,n,k,r)$-ary polycategorical effect system generalizing Heftia and Kernmantle  
**Documentation:** [`docs/INDEX.md`](docs/INDEX.md)  
**Operational State:** [`STATE.md`](STATE.md)  
**Canonical Implementation Plan:** [`docs/notes/IMPLEMENTATION_PLAN.md`](docs/notes/IMPLEMENTATION_PLAN.md)  
**Design Monograph:** [`docs/notes/PENELOPE_DESIGN.md`](docs/notes/PENELOPE_DESIGN.md)

---

## Phase 1: `polytropos-core` & `polytropos-plugin` Prerequisites [ACTIVE]

Ground the low-level row permutation, frame-rule solving, and poly-kinded row indexing primitives in upstream `polytropos`:

- [x] **Milestone 1.1: Reified Row Morphisms (`RowMorph`)** [COMPLETED in polytropos]
  - Implement unboxed representation `newtype RowMorph r1 r2 = RowMorph (PrimArray Int)` in `Polytropos.RowMorph`.
  - Extend `Polytropos.Plugin` to solve `KnownRowMorph r1 r2` and emit static unboxed CAF constants.
  - Implement primitive gather/scatter loops over `Polytropos.Core.Rep` (`gatherRep`, `scatterRep`).
  - Implement $O(|r1|)$ route composition (`composeRowMorph`).
- [x] **Milestone 1.2: Frame-Rule Unification Constraint Solver (`FrameWire`)** [COMPLETED in polytropos]
  - Define `FramePlan` encoding input gather map, output scatter map, and untouched passthrough slots.
  - Extend `Polytropos.Plugin` to solve `FrameWire frameIn opIn opOut frameOut`, unifying with `RowOverwrite frameIn opOut`.
  - Defer unsolved `IsLabel` / `HasField` constraints when target rows are ununified `TyVarTy`s.
- [x] **Milestone 1.3: Poly-Kinded Effect Row Indexing** [COMPLETED in polytropos]
  - Generalize `FieldIndex` and `ConstructorIndex` in `Polytropos.Plugin` to support higher-kinded rows:
    - `Row PolyEffect` (`PolyEffect = Row Type -> Row (Row Type) -> Type`)
    - `Row PolyHEffect` (`PolyHEffect = (PortSig -> Type) -> Row PortSig -> Row Type -> Row (Row Type) -> Type`)
  - Emit unboxed `Word#` / `Int#` literals directly into Core without dictionary parameters.

---

## Phase 2: Core $(m,n,k,r)$-ary Syntax & Unpacked Representation (`penelope-core`) [COMPLETED]

Implement unpacked $(m,n,k,r)$-ary syntax and zero-overhead GADT constructors:

- [x] **Milestone 2.1: Type-Level Signatures & Core Syntax GADT**
  - Ground `PortSig`, `BranchCont`, and `Region` in `Penelope.Core.Syntax`.
  - Implement `Penelope eh ef ins outs` GADT with unpacked `Word#` variant tags and `Any` payloads:
    - `Route` (pure static wire permutation)
    - `LiftPure` (opaque pure transformations)
    - `OpF` (unpacked first-order operation with $k$ explicit branch continuations)
    - `OpH` (unpacked higher-order scoped operation with $r$ sub-regions and $k$ branch continuations)
    - `ScopeRow` (regional scope boundary for zero-traversal weakening)
- [x] **Milestone 2.2: Pure Route Peephole Optimizer**
  - Implement smart constructor `route :: RowMorph outs ins -> Penelope eh ef ins outs`.
  - Implement peephole fusion in sequential composition: `Route m1 >>> Route m2` fuses to `Route (composeRowMorph m1 m2)`.
  - Implement identity elision for identity wire permutations.
- [x] **Milestone 2.3: Zero-Overhead Smart Constructors (`sendPolyF`, `sendPolyH`)**
  - Implement `sendPolyF` using `ConstructorIndex l ef` to inject operations with `unsafeCoerce#` and static gather wiring.
  - Implement `sendPolyH` using `ConstructorIndex l eh` to package sub-regions into `Record (Region eh ef) regions`.

---

## Phase 3: Polycategorical Composition & The Frame Rule (`Penelope.Core.Wiring`) [COMPLETED]

Implement polycategorical wiring and zero-traversal weakening:

- [x] **Milestone 3.1: Sequential Composition & Coyoneda Leaf Fusion (`>>>`)**
  - Implement `seqPenelope :: Penelope eh ef a b -> Penelope eh ef b c -> Penelope eh ef a c`.
  - Implement Coyoneda-style continuation fusion over `Record (BranchCont ...) exits` using `Polytropos.Record.rmapFused`.
- [x] **Milestone 3.2: Frame-Rule Automatic Port Wiring (`>&>`)**
  - Implement `(>&>) :: (FrameWire frameIn ins1 outs1 frameMid, FrameWire frameMid ins2 outs2 frameOut) => Penelope eh ef ins1 outs1 -> Penelope eh ef ins2 outs2 -> Penelope eh ef frameIn frameOut`.
  - Pull named inputs from active register frame, scatter output results, and preserve untouched frame ports in $O(|frame|)$ time.
- [x] **Milestone 3.3: Disjunctive Multi-Exit Fanout (`>?<`)**
  - Implement $k$-way exit branching combinator `(>?<)` binding an operation's `exits` row with a `Record` of $k$ downstream handlers in $O(1)$.
- [x] **Milestone 3.4: Zero-Traversal Row Weakening (`ScopeRow`)**
  - Implement `weakenRow :: (KnownRowMorph ehSub ehFull, KnownRowMorph efSub efFull) => Penelope ehSub efSub ins outs -> Penelope ehFull efFull ins outs`.
  - Embed subprograms via `ScopeRow` in $O(1)$ without walking the syntax tree.

---

## Phase 4: Two-Phase Weaving & Interpretation Engine (`Penelope.Core.Engine` & `penelope-static`) [COMPLETED]

Implement two-phase macro-lowering and algebraic interpretation:

- [x] **Milestone 4.1: Static Pre-Execution Analysis (`penelope-static`)**
  - Implement `StaticPenelope f eh ef ins outs = StaticPenelope !(f ()) !(Penelope eh ef ins outs)`.
  - Implement AST traversal extracting capability strands, schemas, and CLI options.
  - Implement per-port liveness analysis (last-use lifetime extraction) across `RowMorph` edges.
- [x] **Milestone 4.2: Phase 1 — Single-Pass Higher-Order Elaboration (`elaborateAll`)**
  - Implement `newtype Elaborator eh' ef h = Elaborator { runElaborator :: ... }`.
  - Implement `elaborateAll :: Record (Elaborator '[] ef) eh -> Penelope eh ef ins outs -> Penelope '[] ef ins outs`.
  - Walk the AST bottom-up in a single pass, unrolling scoped regions into pure first-order operations in `ef`.
- [x] **Milestone 4.3: Phase 2 — Bulk Vector-Dispatched Interpretation (`interpretAll`)**
  - Implement `newtype Handler ef m e = Handler { runHandler :: ... }`.
  - Implement `interpretAll :: Category m => (forall i o. RowMorph o i -> m i o) -> Record (Handler '[] m) ef -> Penelope '[] ef ins outs -> m ins outs`.
  - Dispatch each `OpF tag# inMap opAny conts` via branchless `indexSmallArray#` in $O(1)$ time.
- [x] **Milestone 4.4: Partial / Layered Interpretation (`interpretSub`)**
  - Implement partial effect stack peeling via `Polytropos.Plugin`'s `RowPartition` in $O(1)$ without linear search cascades.

---

## Phase 5: Surface Ergonomics & Compatibility Facades (`penelope-effects`) [COMPLETED]

Ground user-facing facades and standard effect battery:

- [x] **Milestone 5.1: The Heftia Unary Facade (`Penelope.Effects.Unary`)**
  - Define `newtype Eff eh ef a = Eff (Penelope eh ef '[] '["val" := a])`.
  - Implement `Functor`, `Applicative`, and `Monad` for `Eff eh ef`.
  - Provide adapters embedding standard unary GADTs (`f :: Type -> Type`) into `PolyEffect`.
- [x] **Milestone 5.2: The Kernmantle Binary Facade (`Penelope.Effects.Binary`)**
  - Define `newtype Rope eh ef a b = Rope (Penelope eh ef '["in" := a] '["out" := b])`.
  - Implement `Category`, `Arrow`, `ArrowChoice`, `Profunctor`, `Strong`, and `Choice` for `Rope`.
  - Implement `deriveRouter` compiling a `Record` of `Rope` handlers into an $O(1)$ jump table over `Polytropos.Variant`.
- [x] **Milestone 5.3: Native Multi-Region & Multi-Exit Effect Battery**
  - `Penelope.Effects.Control`: Multi-region `While`, multi-exit `Switch`, and `Saga` (`"try"` + `"compensate"` + `"catch"`).
  - `Penelope.Effects.Coroutine` & `NonDet`: Delimited continuations with frame cloning and suspension.
  - `Penelope.Effects.UnliftIO` & `Provider`: Sound scoped resource management with zero dynamic handler leaks.

---

## Phase 6: Formal Verification, Soundness Certification & Benchmarking

Certify safety, correctness, and performance:

- [x] **Milestone 6.1: GHC Core Inspection (`inspection-testing`)**
  - Verify `sendPolyF` and `sendPolyH` compile to literal unboxed `Word#` constants with zero residual dictionaries.
  - Verify `elaborateAll` and `interpretAll` compile to direct `indexSmallArray#` indexing without linear search.
- [x] **Milestone 6.2: Institutional Satisfaction & Reduct Property Tests (`QuickCheck`)**
  - Property-test Reduct-Evaluation Identity: $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$ across randomized row permutations.
  - Property-test Static-Elaboration Commutativity: verify metadata extracted by `StaticPenelope` is identical before and after `elaborateAll`.
- [x] **Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite**
  - Port Heftia's `UnliftIO`, `Concurrent`, and `Coroutine` + `Local`/`Catch` interaction test suites.
  - Certify scoped effects unrolled during `elaborateAll` never leak handles or corrupt state across multi-shot coroutines.
- [ ] **Milestone 6.4: Allocation & Throughput Benchmarks (`tasty-bench`)**
  - Benchmark deep effect stacks ($D = 30$ first-order, $H = 15$ higher-order) and wide register frames ($m, n = 20$ ports).
  - Compare throughput, RSS, and nursery allocations against `heftia`, `effectful`, and `kernmantle`.
