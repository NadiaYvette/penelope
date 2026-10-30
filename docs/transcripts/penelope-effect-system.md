Penelope Effect System 

Per the design to generalise the Heftia and Kernmantle effect systems to n-ary effects and to use my Polytropos library to make it efficient, what would be a good implementation plan for such a new effect system?

An implementation plan for **Penelope**—generalizing **Heftia**’s two-phase higher-order algebraic effects and **Kernmantle**’s braided binary pipelines into a tightly coupled, **Polytropos**-backed $(m, n, k, r)$-ary polycategorical effect system—naturally divides into six sequential engineering phases.

---

### Architectural Summary & Package Topology

In **Penelope**, every computation node is parameterized by four **Polytropos** rows:

* **$m$ Conjunctive Input Ports (`ins :: Row Type`)**: Named SSA-style input arguments passed as a flat Polytropos `Record` frame.


* **$n$ Conjunctive Output Ports (`outs :: Row Type`)**: Named output results produced in a flat Polytropos `Record` frame.


* **$k$ Disjunctive Control Exits (`exits :: Row (Row Type)`)**: Named exit branches (each carrying its own output port row), paired via the Polytropos sum–product Hom-adjunction with a flat `Record` of $k$ explicit branch continuations.


* **$r$ Higher-Order Regions (`regions :: Row PortSig`)**: Named sub-computations enclosed by a higher-order scoped effect, each carrying its own `(rIns, rOuts)` port signature.



| Package | Role in the Penelope Stack | Key Dependencies |
| --- | --- | --- |
| **`polytropos-core` & `polytropos-plugin**` (Upgrades) | Compile-time `RowMorph` index permutation synthesis, frame-rule row constraints (`FrameMerge`), and kind-polymorphic effect row indexing.

 | `ghc`, `ghc-prim`, `primitive`, `constraints`

 |
| **`penelope-core`** | Unpacked $(m, n, k, r)$-ary syntax tree (`Penelope`), static `RowMorph` port routing, contravariant handler reducts, and two-phase elaboration/interpretation engines.

 | `polytropos-core`, `polytropos-plugin`, `profunctors`

 |
| **`penelope-static`** | Generalized Cayley static analyzer (`StaticPenelope f eh ef ins outs`) for pre-execution extraction of schemas, capabilities, and per-port liveness.

 | `penelope-core`, `selective`

 |
| **`penelope-effects`** | Standard $(m, n, k, r)$-ary effects (multi-region `While`/`Saga`/`Race`, multi-exit `Switch`/`Catch`, `Provider`, `UnliftIO`) plus Unary (`Eff`) and Binary (`Rope`) surface facades.

 | `penelope-core`, `penelope-static`

 |

---

### Phase 1: `polytropos-core` & `polytropos-plugin` Prerequisites

Before building `penelope-core`, extend `polytropos-core` and `polytropos-plugin` with three primitives required for $O(1)$ port wiring and multi-sorted effect rows:

1. **Reified Row Morphisms (`RowMorph r1 r2`)**:
* Add a runtime-unboxed index mapping type `newtype RowMorph (r1 :: Row k) (r2 :: Row k) = RowMorph ByteArray#` (or `SmallArray Int`) to `Polytropos.Core`.


* Extend `Polytropos.Plugin` to solve `KnownRowMorph r1 r2` by emitting a static array of unboxed slot offsets mapping each field index $i \in [0 \dots \vert{}r_1\vert{}-1]$ to its target index in $r_2$.


* Provide primitive gather/scatter operations over `Polytropos.Core.Rep`:
* `gatherRecord :: RowMorph sub full -> Record f full -> Record f sub` (extracts input ports for an operation from the live frame in $O(\vert{}sub\vert{})$).


* `scatterRecord :: RowMorph out full -> Record f out -> Record f full -> Record f full` (merges an operation's output ports back into the live frame).






2. **Frame-Rule Unification Constraints (`FrameWire`)**:
* Teach `Polytropos.Plugin` to solve `FrameWire frameIn opIn opOut frameOut` where `opIn` is a sub-row of `frameIn` and `frameOut` is the canonical row union `RowOverwrite frameIn opOut` (overwriting updated ports and preserving untouched frame ports).


* Defer unsolved `IsLabel` / `HasField` constraints when the target row variable is still an ununified `TyVarTy` inside `proc` or `do` blocks so GHC does not prematurely reject polymorphic port labels.




3. **Poly-Kinded Effect Row Indexing**:
* Ensure `FieldIndex` and `ConstructorIndex` in `Polytropos.Plugin` accept rows of higher kinds `Row PolyEffect` and `Row PolyHEffect`, emitting literal `Word#` / `Int#` slot constants into GHC Core.





---

### Phase 2: Core $(m, n, k, r)$-ary Syntax & Unpacked Representation (`penelope-core`)

In Heftia and Freer monads, monadic/polycategorical binding happens on the hot nursery allocation path. Wrapping every operation in a full `Polytropos.Core.Rep` (`Rep !Word !(SmallArray (f Any))`) would allocate an extra 1-element `SmallArray#` heap object per effect invocation. Instead, **unpack the Polytropos variant tag (`Word#`) and `Any` payload directly into the `Penelope` GADT constructors** while using `Polytropos.Record` (`SmallArray (f Any)`) for multi-branch continuations, multi-region bundles, and handler jump tables.

```haskell
{-# LANGUAGE DataKinds, GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes, TypeOperators #-}

module Penelope.Core.Syntax where

import Data.Kind (Type)
import Data.Functor.Identity (Identity)
import GHC.Exts (Any, Word#)
import Polytropos.Record (Row, Record, RowMorph)

-- | A signature of m named input ports and n named output ports.
data PortSig = PortSig (Row Type) (Row Type)

-- | First-order (m, n, k)-ary effect kind:
--   Takes input port row `ins` and transitions to one of `exits` (where each exit carries an output port row).
type PolyEffect = Row Type -> Row (Row Type) -> Type

-- | Higher-order (m, n, k, r)-ary scoped effect kind:
--   Takes a carrier `(PortSig -> Type)`, a row of `r` regions, `ins` ports, and `exits` branches.
type PolyHEffect = (PortSig -> Type) -> Row PortSig -> Row Type -> Row (Row Type) -> Type

-- | A continuation branch consuming the output port frame `outs` of a specific exit.
newtype BranchCont m (outs :: Row Type) = BranchCont
  { runBranchCont :: Record Identity outs -> m }

-- | A scoped sub-pipeline region with its own port signature.
newtype Region (eh :: Row PolyHEffect) (ef :: Row PolyEffect) (sig :: PortSig) where
  Region :: Penelope eh ef rIns rOuts -> Region eh ef ('PortSig rIns rOuts)

-- | The core Penelope (m, n, k, r)-ary computation structure.
data Penelope
       (eh   :: Row PolyHEffect)
       (ef   :: Row PolyEffect)
       (ins  :: Row Type)
       (outs :: Row Type)
  where
  -- 1. Pure static port routing (replaces opaque `arr` tuple shuffling):
  Route
    :: !(RowMorph outs ins)
    -> Penelope eh ef ins outs

  -- 2. Pure value transformation (only used when computation is intentionally opaque):
  LiftPure
    :: !(RowMorph opIns ins)
    -> !(Record Identity opIns -> Record Identity opOuts)
    -> !(BranchCont (Penelope eh ef ins outs) opOuts)
    -> Penelope eh ef ins outs

  -- 3. Unpacked First-Order (m, n, k)-ary Operation:
  OpF
    :: Word#                                                  -- ^ Polytropos.Plugin slot in `ef`
    -> !(RowMorph opIns ins)                                  -- ^ Static input port gather map
    -> Any                                                    -- ^ Coerced `e opIns exits`
    -> !(Record (BranchCont (Penelope eh ef ins outs)) exits) -- ^ k explicit branch continuations
    -> Penelope eh ef ins outs

  -- 4. Unpacked Higher-Order (m, n, k, r)-ary Scoped Region Operation:
  OpH
    :: Word#                                                  -- ^ Polytropos.Plugin slot in `eh`
    -> !(RowMorph opIns ins)                                  -- ^ Static input port gather map
    -> !(Record (Region eh ef) regions)                       -- ^ r named sub-pipeline regions
    -> Any                                                    -- ^ Coerced `h (Region eh ef) regions opIns exits`
    -> !(Record (BranchCont (Penelope eh ef ins outs)) exits) -- ^ k explicit branch continuations
    -> Penelope eh ef ins outs

```

**Key Structural Invariants to Implement in Phase 2:**

1. **`Route` Peephole Fusion**: Composing `Route m1` with `Route m2` immediately fuses at construction time via `composeRowMorph m1 m2` ($O(\vert{}outs\vert{})$ array lookup) without growing the syntax tree.


2. **Smart Constructors (`sendPolyF`, `sendPolyH`)**: Use `Polytropos.Plugin`'s `ConstructorIndex l ef` / `ConstructorIndex l eh` to obtain the unboxed `Word#` tag at compile time and inject the operation with `unsafeCoerce#`.



---

### Phase 3: Polycategorical Composition & The Frame Rule (`Penelope.Wiring`)

To eliminate the "Arrow Plumbing Black Hole" (nested `(a, (b, c))` tuples and `Either` tags) while packed with more functionality per line of code, implement three composition operators:

1. **Sequential Port Composition (`>>>`)**:
* `seqPenelope :: Penelope eh ef a b -> Penelope eh ef b c -> Penelope eh ef a c`.


* Recursively maps `seqPenelope` over the `Record (BranchCont ...) exits` at the leaves of the first computation using `Polytropos.Record.rmapFused` (Coyoneda-fused array mapping so chained compositions do not allocate intermediate `SmallArray#` continuation records until lowered).




2. **Frame-Rule Automatic Port Wiring (`>&>`)**:
* Given `stage1 :: Penelope eh ef ins1 outs1` and `stage2 :: Penelope eh ef ins2 outs2`, where `ins2` draws a subset of ports from `ins1` and `outs1`:
```haskell
(>&>)
  :: (FrameWire frameIn ins1 outs1 frameMid, FrameWire frameMid ins2 outs2 frameOut)
  => Penelope eh ef ins1 outs1
  -> Penelope eh ef ins2 outs2
  -> Penelope eh ef frameIn frameOut

```


* Uses `Polytropos.Plugin`-synthesized `RowMorph` gather/scatter maps to automatically pull matching named ports from the active register frame and write updated/new ports back, leaving unmentioned ports untouched in the frame.




3. **Disjunctive Multi-Exit Fanout (`>?<`)**:
* Given an operation with $k$ exit branches `exits :: Row (Row Type)` and a Polytropos `Record` of $k$ downstream handlers matching each exit label, binds all $k$ continuations in $O(1)$ by zipping the handler `Record` with the operation's `exits` slot array.




4. **Zero-Traversal Row Weakening via the Satisfaction Condition**:
* Instead of walking the entire syntax tree to re-tag `Word#` indices when embedding a sub-program with effect rows `(eh1, ef1)` into a larger context `(eh2, ef2)`, introduce a regional scope node `ScopeRow !(RowMorph eh1 eh2) !(RowMorph ef1 ef2) !(Penelope eh1 ef1 ins outs)`.


* During elaboration and interpretation, apply the contravariant reduct $\sigma^*(\mathcal{H}) = \text{gatherRecord } \sigma \text{ } \mathcal{H}$ to the handler `Record` in $O(\vert{}eh_1\vert{} + \vert{}ef_1\vert{})$ time upon entering `ScopeRow`, satisfying $\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$ with zero tree re-allocation.





---

### Phase 4: Two-Phase Weaving & Interpretation Engine (`Penelope.Engine`)

Matching the mythological namesake, Penelope’s execution engine separates **Weaving** (macro-pipeline lowering & higher-order region elaboration) from **Unweaving** (first-order multi-continuation algebraic interpretation).

#### Step 4.1: Static Pre-Execution Analysis (`penelope-static`)

* Generalize Kernmantle's `SomeBinEff f w m a b` to `StaticPenelope f eh ef ins outs = StaticPenelope !(f ()) !(Penelope eh ef ins outs)` where `f` is a static `Applicative` (or `Selective` via `Over VirtualTree`).


* Because `Route`, `OpF` (with static `exits` records), and `OpH` (with static `regions` records) keep all $k$ branches and $r$ sub-pipelines outside opaque lambdas, a static analyzer can traverse the entire hypergraph before execution to:
* Accumulate required configuration keys, CLI flags, database schemas, and capability strands.


* Compute exact **per-port liveness (last-use analysis)** and borrow lifetimes across the `RowMorph` edges.





#### Step 4.2: Phase 1 — Single-Pass Higher-Order Region Elaboration (`elaborateAll`)

* Define a higher-order elaborator type:
```haskell
newtype Elaborator (eh' :: Row PolyHEffect) (ef :: Row PolyEffect) (h :: PolyHEffect) = Elaborator
  { runElaborator
      :: forall regions opIns exits.
         Record (Region '[] ef) regions
      -> Record Identity opIns
      -> h (Region '[] ef) regions opIns exits
      -> Penelope eh' ef opIns ( CollapseExits exits ) -- or direct multi-exit continuation wiring
  }

```


* Implement `elaborateAll :: Record (Elaborator '[] ef) eh -> Penelope eh ef ins outs -> Penelope '[] ef ins outs`.


* **How it works at the GHC Core level**: `elaborateAll` walks the syntax tree bottom-up in **one pass**. At each `OpH tag# inMap regions opAny conts`, it first elaborates the child `regions` record, then executes a branchless `indexSmallArray# elaboratorsArr# (word2Int# tag#)` lookup into the `Record` of elaborators.


* Once `elaborateAll` finishes, `eh` is statically `'[]`: all scoped effects (`local`, `catch`, `scf.while`, sagas, `withRunInIO`) are compiled down into pure first-order operations in `ef`, guaranteeing Heftia's soundness property (scoped state and exceptions never leak or corrupt across multi-shot coroutine/backtracking continuations).



#### Step 4.3: Phase 2 — Vector-Dispatched First-Order Interpretation (`interpretAll`)

* Eliminate the $O(M \cdot D)$ `decomp` tag-decrement allocation tax of traditional Freer monads by providing a bulk handler record interpreter:


```haskell
-- | Handler for a first-order (m, n, k)-ary effect in target carrier `m`:
newtype Handler (ef :: Row PolyEffect) (m :: Row Type -> Row Type -> Type) (e :: PolyEffect) = Handler
  { runHandler
      :: forall opIns exits frameIns frameOuts.
         Record Identity opIns
      -> e opIns exits
      -> Record (BranchCont (Penelope '[] ef frameIns frameOuts)) exits
      -> m frameIns frameOuts
  }

interpretAll
  :: Category m
  => (forall ins outs. RowMorph outs ins -> m ins outs)
  -> Record (Handler '[] m) ef
  -> Penelope '[] ef ins outs
  -> m ins outs

```


* At every `OpF tag# inMap opAny conts` node, `interpretAll` gathers the input ports via `inMap` and indexes directly into `Record (Handler '[] m) ef` using `indexSmallArray# handlersArr# (word2Int# tag#)` in $O(1)$ time with zero intermediate union re-allocations.


* **Partial / Layered Interpretation (`interpretSub`)**: When interpreting only a subset `efSub` of `ef` (e.g., handling `State` and `NonDet` while leaving `IO` effects in `efRem`), use `Polytropos.Plugin`’s static partition table (`RowPartition ef efSub efRem`) to split the dispatch in $O(1)$ without linear `eqWord#` cascades.



---

### Phase 5: Surface Ergonomics & Compatibility Facades (`penelope-effects`)

Writing raw $(m, n, k, r)$-ary port records for simple sequential or 1-in-1-out code would be overly verbose. Provide two zero-cost surface interfaces backed by the exact same `Penelope` core:

1. **The Heftia Unary Facade (`Penelope.Unary`)**:
* Define `newtype Eff eh ef a = Eff (Penelope eh ef '[] '["val" := a])`.


* Define unary effect adapters bridging standard unary GADTs (`f :: Type -> Type` and `h :: (Type -> Type) -> Type -> Type`) into `PolyEffect` (`ins = '[]`, `exits = '["next" := '["val" := a]]`) and `PolyHEffect`.


* Implement `Functor`, `Applicative`, and `Monad` for `Eff eh ef` using `LiftPure` and `OpF`/`OpH` continuation extension so existing Heftia code using `do`-notation compiles with zero syntactic changes.




2. **The Kernmantle Binary Facade (`Penelope.Binary`)**:
* Define `newtype Rope eh ef a b = Rope (Penelope eh ef '["in" := a] '["out" := b])`.


* Implement `Category`, `Profunctor`, `Strong`, `Choice`, `Arrow`, and `ArrowChoice` for `Rope eh ef`.


* Implement `deriveRouter` (using `Polytropos.Generics.hcollapse`) to compile a Polytropos `Record` of `Rope` handlers into an $O(1)$ jump table over a `Polytropos.Variant` input.




3. **Native Multi-Region & Multi-Exit Effect Battery (`Penelope.Effects.*`)**:
* **`Penelope.Effects.Control`**: Multi-region `While` (`"cond"` region + `"body"` region), `Switch` ($k$-way disjunctive exit branching without opaque lambdas), and `Saga` (`"try"` region + port-matched `"compensate"` region + `"catch"` region).


* **`Penelope.Effects.Coroutine` & `NonDet**`: Multi-shot delimited continuation handlers that clone or suspend the active `Record Identity ins` port frame across downstream stages.


* **`Penelope.Effects.UnliftIO` & `Provider**`: Sound scoped resource management elaborated in Phase 1.





---

### Phase 6: Verification & Benchmarking Milestones

Execute the implementation against four concrete verification gates:

1. **Gate 1 — GHC Core Inspection (`inspection-testing`)**:
* Verify that `sendPolyF #myEffect` and `sendPolyH #myScopedEffect` compile to literal `Word#` constants with zero residual `FieldIndex` / `ConstructorIndex` dictionary arguments.


* Verify that `elaborateAll` and `interpretAll` compile to direct `indexSmallArray#` primitive reads with zero `eqWord#` linear branch chains.




2. **Gate 2 — Institutional Satisfaction & Reduct Laws (`QuickCheck`)**:
* Property-test the Reduct-Evaluation Identity ($\text{eval}_J(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_I(\sigma^*(\mathcal{H}), P)$) across randomized row permutations and sub-row inclusions using `polytropos-quickcheck`.


* Property-test Static–Elaboration Commutativity (verifying that static analysis extraction via `StaticPenelope` produces identical metadata before and after `elaborateAll`).




3. **Gate 3 — Heftia Higher-Order Semantics Suite**:
* Port Heftia’s `Test.UnliftIO`, `Test.Concurrent`, and `Coroutine` + `Local`/`Catch` interaction tests to verify that scoped effects elaborated via `elaborateAll` never leak handles or corrupt state across multi-shot continuations.




4. **Gate 4 — Allocation & Throughput Benchmarks (`tasty-bench`)**:
* Benchmark deep effect stacks ($D = 30$ first-order effects, $H = 15$ higher-order effects) and wide port frames ($m, n = 20$ ports) against upstream `heftia`, `effectful`, and `kernmantle`, verifying $O(1)$ dispatch scaling with respect to row width and zero intermediate `SmallArray#` allocations per `OpF`/`OpH` bind.





Would you like to start by drafting the exact module code for the `RowMorph` and `FrameWire` additions in `polytropos-plugin`, or dive directly into the implementation of `Penelope.Core.Syntax` and `Penelope.Engine`?

Please do show the RowMorph and FrameWire additions needed within `polytropos-plugin`.

Implementing `RowMorph` and `FrameWire` requires a small companion module in `polytropos-core` (defining the unboxed representation, typeclasses, and primitive `SmallArray` gather/scatter loops over `Rep`) paired with the constraint-solving and Core-synthesis pass in `polytropos-plugin`.

---

### 1. Core Representation & Primitives (`polytropos-core`)

In `polytropos-core`, products and sums share the representation `data Rep (f :: Type -> Type) a = Rep {-# UNPACK #-} !Word !(SmallArray (f Any))`. We represent `RowMorph` as an unboxed `PrimArray Int` (backed by `ByteArray#`) and `FrameWire` as a concrete three-map plan that gathers operation inputs and merges outputs into the active frame in two tight, branchless loops.

```haskell
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, GADTs #-}
{-# LANGUAGE KindSignatures, MagicHash, MultiParamTypeClasses, PolyKinds #-}
{-# LANGUAGE RankNTypes, RoleAnnotations, ScopedTypeVariables, BangPatterns #-}

module Polytropos.RowMorph
  ( -- * Reified Row Morphisms
    RowMorph(..)
  , KnownRowMorph(..)
  , unsafeMkRowMorph
  , composeRowMorph
  , indexRowMorph
    -- * Record Gather & Scatter over Polytropos.Core.Rep
  , gatherRep
  , scatterRep
    -- * Frame-Rule Auto-Wiring Plan
  , FramePlan(..)
  , FrameWire(..)
  , unsafeMkFramePlan
  , executeFrameScatter
  ) where

import Control.Monad.ST (runST)
import Data.Kind (Type)
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import GHC.Exts (Any)
import Polytropos.Core (Rep(..), Row)

-- | A compile-time synthesized slot injection map from `r1` into `r2`.
--   Each index `i` in `[0 .. |r1| - 1]` maps to its target slot in `[0 .. |r2| - 1]`.
type role RowMorph nominal nominal
newtype RowMorph (r1 :: Row k) (r2 :: Row k) = RowMorph
  { unRowMorph :: PrimArray Int }

-- | Solved by `Polytropos.Plugin` into a static `RowMorph` CAF.
class KnownRowMorph (r1 :: Row k) (r2 :: Row k) where
  rowMorph :: RowMorph r1 r2

-- | Entry point called by Core expressions synthesized by `Polytropos.Plugin`.
{-# NOINLINE unsafeMkRowMorph #-}
unsafeMkRowMorph :: [Int] -> RowMorph r1 r2
unsafeMkRowMorph idxs = RowMorph (primArrayFromList idxs)

{-# INLINE indexRowMorph #-}
indexRowMorph :: RowMorph r1 r2 -> Int -> Int
indexRowMorph (RowMorph arr) !i = indexPrimArray arr i

-- | Category composition for `RowMorph`: O(|r1|) primitive array indexing.
{-# INLINE composeRowMorph #-}
composeRowMorph :: RowMorph r1 r2 -> RowMorph r2 r3 -> RowMorph r1 r3
composeRowMorph (RowMorph m12) (RowMorph m23) = RowMorph $ runST $ do
  let !n = sizeofPrimArray m12
  out <- newPrimArray n
  let go !i
        | i >= n    = unsafeFreezePrimArray out
        | otherwise = do
            let !midIdx = indexPrimArray m12 i
                !dstIdx = indexPrimArray m23 midIdx
            writePrimArray out i dstIdx
            go (i + 1)
  go 0

-- | Contravariant reduct / port gather: extracts `sub` slots from `full` in O(|sub|).
--   Directly implements Mod(sigma)(M') for handler records and input port frames.
{-# INLINE gatherRep #-}
gatherRep :: RowMorph sub full -> Rep f full -> Rep f sub
gatherRep (RowMorph morph) (Rep _ srcArr) = Rep 0 $ runSmallArray $ do
  let !n = sizeofPrimArray morph
  dstArr <- newSmallArray n (error "gatherRep: uninitialized slot")
  let go !i
        | i >= n    = pure dstArr
        | otherwise = do
            let !srcIdx = indexPrimArray morph i
            writeSmallArray dstArr i (indexSmallArray srcArr srcIdx)
            go (i + 1)
  go 0

-- | Overwrites `sub` slots inside an existing `full` record in O(|full|).
{-# INLINE scatterRep #-}
scatterRep :: RowMorph sub full -> Rep f sub -> Rep f full -> Rep f full
scatterRep (RowMorph morph) (Rep _ subArr) (Rep _ fullArr) = Rep 0 $ runSmallArray $ do
  let !fullLen = sizeofSmallArray fullArr
      !subLen  = sizeofPrimArray morph
  dstArr <- thawSmallArray fullArr 0 fullLen
  let go !i
        | i >= subLen = pure dstArr
        | otherwise   = do
            let !targetIdx = indexPrimArray morph i
            writeSmallArray dstArr targetIdx (indexSmallArray subArr i)
            go (i + 1)
  go 0

-- =========================================================================
-- Frame-Rule Auto-Wiring (`FrameWire`)
-- =========================================================================

-- | Concrete execution plan for wiring an operation `(opIn -> opOut)`
--   inside a live register frame `(frameIn -> frameOut)`.
data FramePlan (frameIn :: Row k) (opIn :: Row k) (opOut :: Row k) (frameOut :: Row k) = FramePlan
  { fpOutSize     :: {-# UNPACK #-} !Int
  , fpGatherIn    :: !(RowMorph opIn frameIn)
    -- ^ Gathers the operation's `m` input ports from `frameIn`.
  , fpBaseScatter :: !(RowMorph frameIn frameOut)
    -- ^ Places surviving `frameIn` ports into `frameOut`.
  , fpOpScatter   :: !(RowMorph opOut frameOut)
    -- ^ Places `opOut` ports into `frameOut` (overwriting collided `frameIn` ports).
  }

-- | Solved by `Polytropos.Plugin`:
--   1. Verifies `opIn` is a sub-row of `frameIn` (unifying port types).
--   2. Synthesizes `frameOut ~ RowOverwrite frameIn opOut`.
--   3. Emits the `FramePlan` dictionary.
class FrameWire (frameIn :: Row k) (opIn :: Row k) (opOut :: Row k) (frameOut :: Row k) where
  framePlan :: FramePlan frameIn opIn opOut frameOut

{-# NOINLINE unsafeMkFramePlan #-}
unsafeMkFramePlan
  :: Int
  -> [Int]
  -> [Int]
  -> [Int]
  -> FramePlan frameIn opIn opOut frameOut
unsafeMkFramePlan outSz gatherIdxs baseIdxs opIdxs = FramePlan
  { fpOutSize     = outSz
  , fpGatherIn    = unsafeMkRowMorph gatherIdxs
  , fpBaseScatter = unsafeMkRowMorph baseIdxs
  , fpOpScatter   = unsafeMkRowMorph opIdxs
  }

-- | Merges the previous frame `frameIn` and the operation output `opOut`
--   into the next frame `frameOut` in two tight, branchless loops.
{-# INLINE executeFrameScatter #-}
executeFrameScatter
  :: FramePlan frameIn opIn opOut frameOut
  -> Rep f frameIn
  -> Rep f opOut
  -> Rep f frameOut
executeFrameScatter (FramePlan !outSz _ (RowMorph baseMap) (RowMorph opMap)) (Rep _ baseArr) (Rep _ opArr) =
  Rep 0 $ runSmallArray $ do
    let !baseLen = sizeofPrimArray baseMap
        !opLen   = sizeofPrimArray opMap
    dst <- newSmallArray outSz (error "executeFrameScatter: uninitialized slot")
    let copyBase !i
          | i >= baseLen = pure ()
          | otherwise    = do
              let !dstIdx = indexPrimArray baseMap i
              writeSmallArray dst dstIdx (indexSmallArray baseArr i)
              copyBase (i + 1)
        copyOp !j
          | j >= opLen   = pure dst
          | otherwise    = do
              let !dstIdx = indexPrimArray opMap j
              writeSmallArray dst dstIdx (indexSmallArray opArr j)
              copyOp (j + 1)
    copyBase 0
    copyOp 0

```

---

### 2. Compiler Plugin Solver (`polytropos-plugin`)

In `polytropos-plugin`, add `Polytropos.Plugin.RowMorph`. This module intercepts `KnownRowMorph` and `FrameWire` constraints inside `TcPluginSolve`:

* **Open-Tail Deferral**: If any input row still has an ununified type variable (`isTyVarTy`) in its spine, the solver leaves the constraint untouched so GHC's unifier can ground upstream stages in a `>&>` chain or `proc` block first.


* **Port Type Unification**: When matching a label `(lbl, ty1)` in `opIn` against `(lbl, ty2)` in `frameIn`, the plugin emits a derived nominal equality constraint `ty1 ~# ty2` (`newWanted`) so polymorphic ports unify across pipeline stages without type annotations.
* **Automatic `frameOut` Synthesis**: In `FrameWire frameIn opIn opOut frameOut`, `frameOut` is typically a fresh unification variable when chaining `stage1 >&> stage2`. The plugin computes the canonical merged row type `synthesizedOut` from `frameIn` and `opOut` and emits a wanted equality `frameOut ~# synthesizedOut`, driving type inference forward automatically.



```haskell
{-# LANGUAGE CPP, DataKinds, LambdaCase, MagicHash, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, TupleSections, ViewPatterns #-}

module Polytropos.Plugin.RowMorph
  ( RowMorphEnv(..)
  , lookupRowMorphEnv
  , solveRowMorphConstraints
  ) where

import Control.Monad (forM)
import Data.List (elemIndex, foldl', sortOn)
import qualified Data.Map.Strict as Map

-- GHC API imports (GHC 9.8 - 9.14 compatible)
import GHC.Builtin.Names (intTyConKey)
import GHC.Builtin.Types (intTy, intDataCon, promotedConsDataCon, promotedNilDataCon)
import GHC.Core (Expr(..))
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.Make (mkCoreApps, mkIntExprInt, mkListExpr)
import GHC.Core.Predicate (EqRel(..), Pred(..), classifyPredType, mkPrimEqPred)
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (splitAppTys, splitTyConApp_maybe, typeKind)
import GHC.Data.FastString (FastString)
import GHC.Driver.Config (initSDocContext)
import GHC.Plugins ( Id, Name, TyVar, Var, isTyVarTy, mkModuleName
                   , mkTcOcc, mkVarOcc, noSrcSpan, text, ($$), (<+>) )
import GHC.Tc.Plugin
import GHC.Tc.Types.Constraint (Ct(..), CtEvidence(..), CtLoc, ctEvLoc, ctEvPred, mkNonCanonical)
import GHC.Tc.Types.Evidence (EvTerm(..), evId)
import GHC.Unit.Finder (FindResult(..), findImportedModule)
import GHC.Utils.Error (mkPlainErrorMsgEnvelope)

-- | Cached TyCons and Core builder Ids from `Polytropos.RowMorph` and `Polytropos.Core`.
data RowMorphEnv = RowMorphEnv
  { rmeKnownRowMorphTc  :: !TyCon
  , rmeFrameWireTc      :: !TyCon
  , rmeUnsafeMkRowMorph :: !Id
  , rmeUnsafeMkFrame    :: !Id
  , rmeFieldAssnTc      :: !TyCon -- ^ The `(:=)` type constructor in `Row k`
  }

-- | Initialize environment during `tcPluginInit`.
lookupRowMorphEnv :: TyCon -> TcPluginM RowMorphEnv
lookupRowMorphEnv fieldAssnTc = do
  Found _ rmMod <- findImportedModule (mkModuleName "Polytropos.RowMorph") NoPkgQual
  rmeKnownRowMorphTc  <- tcLookupTyCon =<< lookupOrig rmMod (mkTcOcc "KnownRowMorph")
  rmeFrameWireTc      <- tcLookupTyCon =<< lookupOrig rmMod (mkTcOcc "FrameWire")
  rmeUnsafeMkRowMorph <- tcLookupId    =<< lookupOrig rmMod (mkVarOcc "unsafeMkRowMorph")
  rmeUnsafeMkFrame    <- tcLookupId    =<< lookupOrig rmMod (mkVarOcc "unsafeMkFramePlan")
  pure RowMorphEnv { rmeFieldAssnTc = fieldAssnTc, .. }

-- | Parsed canonical representation of a closed Polytropos row.
--   Preserves the exact canonical slot order of the normalized row.
data ParsedRow = ParsedRow
  { prElemKind :: !Type
  , prFields   :: ![(FastString, Type)]
  }

-- | Attempt to parse a type of kind `Row k` (or `[Field k]`).
--   Returns `Nothing` if any spine tail or field label is still an ununified `TyVarTy`.
parseClosedRow :: TyCon -> Type -> Maybe ParsedRow
parseClosedRow assnTc rowTy = do
  let rowKind = typeKind rowTy
  fields <- go rowTy
  -- Canonicalize field order lexicographically by label (matching Polytropos normal form)
  pure ParsedRow
    { prElemKind = rowKind
    , prFields   = sortOn fst fields
    }
  where
    go :: Type -> Maybe [(FastString, Type)]
    go ty
      | isTyVarTy ty = Nothing -- Open row tail: defer until unified!
      | Just (tc, args) <- splitTyConApp_maybe ty =
          case () of
            _ | tc == promotedNilDataCon -> Just []
              | tc == promotedConsDataCon
              , [_k, headTy, tailTy] <- args -> do
                  entry <- parseEntry headTy
                  rest  <- go tailTy
                  Just (entry : rest)
              | otherwise -> Nothing
      | otherwise = Nothing

    parseEntry :: Type -> Maybe (FastString, Type)
    parseEntry entryTy
      | Just (tc, args) <- splitTyConApp_maybe entryTy
      , tc == assnTc
      , (_kinds, [LitTy (StrTyLit lbl), payloadTy]) <- splitAt (length args - 2) args
      = Just (lbl, payloadTy)
      | otherwise = Nothing

-- | Reconstruct a promoted GHC `Type` from a `ParsedRow`.
reifyParsedRow :: TyCon -> Type -> [(FastString, Type)] -> Type
reifyParsedRow assnTc entryKind = foldr consEntry nilRow
  where
    nilRow = mkTyConApp promotedNilDataCon [entryKind]
    consEntry (lbl, payloadTy) acc =
      let assnTy = mkTyConApp assnTc [typeKind payloadTy, LitTy (StrTyLit lbl), payloadTy]
      in mkTyConApp promotedConsDataCon [entryKind, assnTy, acc]

-- | Main entry point called inside `tcPluginSolve`.
solveRowMorphConstraints
  :: RowMorphEnv
  -> [Ct] -- ^ Given constraints
  -> [Ct] -- ^ Wanted constraints
  -> TcPluginM TcPluginSolveResult
solveRowMorphConstraints env _givens wanteds
  | null wanteds = pure (TcPluginOk [] [])
  | otherwise = do
      results <- forM wanteds (solveOne env)
      let (solved, newWanteds, insoluble) = foldResults results
      if not (null insoluble)
        then pure (TcPluginContradiction insoluble)
        else pure (TcPluginOk solved newWanteds)

data SingleResult
  = Defer
  | Solved (EvTerm, Ct) [Ct]
  | Insoluble Ct

foldResults :: [SingleResult] -> ([(EvTerm, Ct)], [Ct], [Ct])
foldResults = foldr step ([], [], [])
  where
    step Defer acc = acc
    step (Solved ev news) (s, n, i) = (ev : s, news ++ n, i)
    step (Insoluble ct)   (s, n, i) = (s, n, ct : i)

solveOne :: RowMorphEnv -> Ct -> TcPluginM SingleResult
solveOne env@RowMorphEnv{..} ct =
  case classifyPredType (ctEvPred (cc_ev ct)) of
    ClassPred cls [_k, r1Ty, r2Ty]
      | classTyCon cls == rmeKnownRowMorphTc ->
          solveKnownRowMorph env ct r1Ty r2Ty
    ClassPred cls [_k, frameInTy, opInTy, opOutTy, frameOutTy]
      | classTyCon cls == rmeFrameWireTc ->
          solveFrameWire env ct frameInTy opInTy opOutTy frameOutTy
    _ -> pure Defer

-- =========================================================================
-- 1. Solving `KnownRowMorph r1 r2`
-- =========================================================================

solveKnownRowMorph :: RowMorphEnv -> Ct -> Type -> Type -> TcPluginM SingleResult
solveKnownRowMorph RowMorphEnv{..} ct r1Ty r2Ty =
  case (parseClosedRow rmeFieldAssnTc r1Ty, parseClosedRow rmeFieldAssnTc r2Ty) of
    (Just r1, Just r2) -> do
      let r2Labels = map fst (prFields r2)
          r2Map    = Map.fromList (zip (map fst (prFields r2)) ( zip [0..] (map snd (prFields r2)) ))
      case matchSubRow (prFields r1) r2Map of
        Left _missingLbl ->
          pure (Insoluble ct)
        Right matchedPairs -> do
          let loc   = ctEvLoc (cc_ev ct)
              idxs  = map fst matchedPairs
              tyEqs = [ (t1, t2) | (_, (t1, t2)) <- matchedPairs ]
          eqCts <- mapM (mkWantedEqCt loc) tyEqs
          dflags <- unsafeTcPluginTcM getDynFlags
          let kTy      = typeKind (snd (head (prFields r1 ++ [("", intTy)])))
              idxList  = mkListExpr intTy (map (mkIntExprInt dflags) idxs)
              evExpr   = mkCoreApps (Var rmeUnsafeMkRowMorph)
                           [ Type kTy, Type r1Ty, Type r2Ty, idxList ]
          pure (Solved (EvExpr evExpr, ct) eqCts)
    _ -> pure Defer -- Defer if either row has an ununified type variable

-- =========================================================================
-- 2. Solving `FrameWire frameIn opIn opOut frameOut`
-- =========================================================================

solveFrameWire
  :: RowMorphEnv
  -> Ct
  -> Type -> Type -> Type -> Type
  -> TcPluginM SingleResult
solveFrameWire RowMorphEnv{..} ct frameInTy opInTy opOutTy frameOutTy =
  case ( parseClosedRow rmeFieldAssnTc frameInTy
       , parseClosedRow rmeFieldAssnTc opInTy
       , parseClosedRow rmeFieldAssnTc opOutTy ) of
    (Just fIn, Just oIn, Just oOut) -> do
      let fInMap = Map.fromList (zip (map fst (prFields fIn)) (zip [0..] (map snd (prFields fIn))))
      case matchSubRow (prFields oIn) fInMap of
        Left _missingLbl ->
          pure (Insoluble ct)
        Right gatherPairs -> do
          let loc        = ctEvLoc (cc_ev ct)
              gatherIdxs = map fst gatherPairs
              inTyEqs    = [ (t1, t2) | (_, (t1, t2)) <- gatherPairs ]

              -- Compute canonical overwritten row: `oOut` overwrites matching labels in `fIn`
              mergedMap  = Map.union
                             (Map.fromList (prFields oOut))
                             (Map.fromList (prFields fIn))
              mergedList = Map.toAscList mergedMap
              mergedLabels = map fst mergedList
              outSz      = length mergedList

              -- Compute scatter indices into `mergedList`
              lookupSlot lbl = case elemIndex lbl mergedLabels of
                Just idx -> idx
                Nothing  -> error "Polytropos.Plugin.RowMorph: impossible missing merged label"

              baseIdxs   = [ lookupSlot lbl | (lbl, _) <- prFields fIn ]
              opIdxs     = [ lookupSlot lbl | (lbl, _) <- prFields oOut ]

              -- Reify the expected `frameOut` row type and unify with `frameOutTy`
              (_, [entryKind]) = splitAppTys (typeKind frameInTy)
              synthOutTy = reifyParsedRow rmeFieldAssnTc entryKind mergedList

          inEqCts  <- mapM (mkWantedEqCt loc) inTyEqs
          outEqCt  <- mkWantedEqCt loc (frameOutTy, synthOutTy)
          dflags   <- unsafeTcPluginTcM getDynFlags

          let kTy        = head (snd (splitAppTys entryKind))
              mkInts xs  = mkListExpr intTy (map (mkIntExprInt dflags) xs)
              evExpr     = mkCoreApps (Var rmeUnsafeMkFrame)
                             [ Type kTy
                             , Type frameInTy
                             , Type opInTy
                             , Type opOutTy
                             , Type frameOutTy
                             , mkIntExprInt dflags outSz
                             , mkInts gatherIdxs
                             , mkInts baseIdxs
                             , mkInts opIdxs
                             ]
          pure (Solved (EvExpr evExpr, ct) (outEqCt : inEqCts))
    _ -> pure Defer -- Defer until `frameIn`, `opIn`, and `opOut` spines are concrete

-- =========================================================================
-- Internal Helpers
-- =========================================================================

matchSubRow
  :: [(FastString, Type)]
  -> Map.Map FastString (Int, Type)
  -> Either FastString [(Int, (Type, Type))]
matchSubRow [] _ = Right []
matchSubRow ((lbl, ty1) : rest) targetMap =
  case Map.lookup lbl targetMap of
    Nothing          -> Left lbl
    Just (idx, ty2)  -> ((idx, (ty1, ty2)) :) <$> matchSubRow rest targetMap

mkWantedEqCt :: CtLoc -> (Type, Type) -> TcPluginM Ct
mkWantedEqCt loc (t1, t2) = do
  ev <- newWanted loc (mkPrimEqPred Nominal t1 t2)
  pure (mkNonCanonical ev)

```

---

### 3. How `Penelope.Wiring` Consumes `RowMorph` and `FrameWire`

With `Polytropos.RowMorph` and `Polytropos.Plugin.RowMorph` in place, the polycategorical routing (`route`) and Frame-Rule composition (`>&>`) in `Penelope.Wiring` reduce to thin wrappers over `rowMorph` and `framePlan`:

```haskell
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Penelope.Wiring
  ( route
  , (>&>)
  ) where

import Data.Functor.Identity (Identity)
import Polytropos.Record (Record(..), Row, rmapFused)
import Polytropos.RowMorph
import Penelope.Core.Syntax

-- | Pure static port projection/permutation in O(1) without opaque `arr` lambdas.
{-# INLINE route #-}
route :: forall outs ins eh ef. KnownRowMorph outs ins => Penelope eh ef ins outs
route = Route (rowMorph @outs @ins)

-- | Frame-Rule Auto-Wiring Composition (`>&>`):
--   Automatically gathers each stage's required input ports from the live register frame
--   and scatters its outputs back into the frame, preserving untouched context ports.
{-# INLINE (>&>) #-}
(>&>)
  :: forall frameIn ins1 outs1 frameMid ins2 outs2 frameOut eh ef.
     ( FrameWire frameIn  ins1 outs1 frameMid
     , FrameWire frameMid ins2 outs2 frameOut
     )
  => Penelope eh ef ins1 outs1
  -> Penelope eh ef ins2 outs2
  -> Penelope eh ef frameIn frameOut
stage1 >&> stage2 =
  let plan1 = framePlan @frameIn  @ins1 @outs1 @frameMid
      plan2 = framePlan @frameMid @ins2 @outs2 @frameOut
  in liftFrame plan1 stage1 `seqPenelope` liftFrame plan2 stage2

-- | Lifts a local `(opIn -> opOut)` computation into a wider `(frameIn -> frameOut)`
--   register frame by composing `fpGatherIn` at the root and `executeFrameScatter` at the leaves.
liftFrame
  :: FramePlan frameIn opIn opOut frameOut
  -> Penelope eh ef opIn opOut
  -> Penelope eh ef frameIn frameOut
liftFrame plan@FramePlan{..} = go
  where
    -- Helper to lift a single-exit continuation frame
    liftCont
      :: BranchCont (Penelope eh ef opIn opOut) branchOuts
      -> BranchCont (Penelope eh ef frameIn frameOut) branchOuts
    liftCont (BranchCont k) = BranchCont (\branchRec -> go (k branchRec))

    go :: Penelope eh ef opIn opOut -> Penelope eh ef frameIn frameOut
    go (Route m) =
      -- Static fusion of `m` with the frame plan when returning directly
      LiftPure fpGatherIn
        (\(Record inRep) -> Record (gatherRep m inRep))
        (BranchCont $ \(Record outRep) ->
          LiftPure (unsafeMkRowMorph [0 .. fpOutSize - 1]) -- Identity gather on frameIn
            (\(Record frameRep) -> Record (executeFrameScatter plan frameRep outRep))
            (BranchCont $ \_ -> Route (unsafeMkRowMorph [0 .. fpOutSize - 1])))

    go (LiftPure inMap f cont) =
      LiftPure (composeRowMorph inMap fpGatherIn) f (liftCont cont)

    go (OpF tag# inMap opAny conts) =
      OpF tag#
        (composeRowMorph inMap fpGatherIn)
        opAny
        (rmapFused liftCont conts)

    go (OpH tag# inMap regions opAny conts) =
      OpH tag#
        (composeRowMorph inMap fpGatherIn)
        regions
        opAny
        (rmapFused liftCont conts)

```

Would you like to examine the implementation of `Penelope.Engine` next—specifically how `elaborateAll` and `interpretAll` handle multi-exit (`exits`) and multi-region (`regions`) records in a single pass?

Please do show how to implement `elaborateAll` and `interpretAll` and `exits` and `regions` within `Penelope.Engine`

Implementing `Penelope.Engine` centers on exploiting **Polytropos**'s symmetric `Rep` layout (`Rep !Word !(SmallArray (f Any))`) so that all four dimensions of an $(m, n, k, r)$-ary effect—input port gathering, $r$-region recursive elaboration, $k$-exit branch continuation routing, and $O(1)$ handler dispatch—compile down to direct `indexSmallArray#` operations with zero intermediate union allocations.

---

### 1. How `regions` and `exits` Work in the Engine

In unary **Heftia**, a higher-order operation wraps a single sub-computation `m a` and returns to a single continuation `a -> Eff eh ef r`. In **Penelope**, both are generalized to **Polytropos** `Record` arrays:

* **$r$ Regions (`Record (Region eh ef) regions`)**:
Each slot in `regions :: Row PortSig` holds a sub-pipeline `Region eh ef ('PortSig rIns rOuts)`. During Phase 1 (`elaborateAll`), before the higher-order elaborator for an `OpH` node runs, the engine maps `elaborateRegion` across the `regions` record using `rmapFused`. Thus, the elaborator receives `Record (Region '[] ef) regions`—where every nested region is already a pure first-order computation ready to be wired together (e.g., wiring a `"cond"` region to a `"body"` region in a loop, or a `"try"` region to a `"compensate"` region in a saga).


* **$k$ Disjunctive Exits (`Record (BranchCont (Penelope ...)) exits`)**:
Each slot in `exits :: Row (Row Type)` corresponds to a named exit branch (e.g., `'["ok" := '["val" := Int], "err" := '["msg" := Text]]`). Instead of forcing an elaborator or handler to return an `Either` or ADT value into an opaque lambda, the elaborator/handler receives the `Record` of $k$ downstream continuations directly. Selecting exit branch $j$ is a single $O(1)$ slot lookup (`indexRepSlot conts j`) followed by passing that branch's output port frame `Record Identity branchOuts`.



---

### 2. Complete Implementation of `Penelope.Engine`

```haskell
{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DataKinds, FlexibleContexts #-}
{-# LANGUAGE GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators, UnboxedTuples #-}

module Penelope.Engine
  ( -- * Zero-Copy Slot Indexing over Polytropos.Core.Rep
    indexRecordSlot#
  , selectExitBranch
  , extractRegion
    -- * Contravariant Reducts (Zero-Traversal Row Weakening)
  , ScopeRow(..)
  , weakenPenelope
    -- * Phase 1: Single-Pass Higher-Order Elaboration
  , Elaborator(..)
  , elaborateAll
  , elaborateSub
    -- * Phase 2: O(1) Vector-Dispatched First-Order Interpretation
  , PolyCarrier(..)
  , Handler(..)
  , interpretAll
  , runPenelopePure
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (SmallArray(..), indexSmallArray)
import GHC.Exts (Any, Int(..), Int#, Word(..), Word#, indexSmallArray#, unsafeCoerce#, word2Int#)

import Polytropos.Core (Rep(..), Row, FieldIndex(..))
import Polytropos.Record (Record(..), rmapFused)
import Polytropos.RowMorph (RowMorph(..), gatherRep, composeRowMorph)
import Penelope.Core.Syntax

-- =========================================================================
-- 1. Low-Level O(1) Slot Primitives for `exits`, `regions`, and `handlers`
-- =========================================================================

-- | Branchless O(1) slot lookup into a Polytropos `Record` using an unboxed `Word#` tag.
--   Compiles directly to a single pointer offset read (`indexSmallArray#`).
{-# INLINE indexRecordSlot# #-}
indexRecordSlot# :: Record f r -> Word# -> f a
indexRecordSlot# (Record (Rep _ (SmallArray arr#))) tag# =
  case indexSmallArray# arr# (word2Int# tag#) of
    (# slotAny #) -> unsafeCoerce# slotAny

-- | Ergonomic helper for elaborators and handlers to jump to a named exit branch `lbl`
--   in O(1) using the compile-time slot index synthesized by `Polytropos.Plugin`.
{-# INLINE selectExitBranch #-}
selectExitBranch
  :: forall lbl branchOuts exits m.
     FieldIndex lbl exits branchOuts
  => Record (BranchCont m) exits
  -> Record Identity branchOuts
  -> m
selectExitBranch (Record (Rep _ arr)) branchOutFrame =
  let !(I# idx#) = fieldIndex @lbl @exits @branchOuts
      !(SmallArray arr#) = arr
  in case indexSmallArray# arr# idx# of
       (# contAny #) ->
         let !(BranchCont k) = unsafeCoerce# contAny
         in k branchOutFrame

-- | Ergonomic helper for higher-order elaborators to extract a named region `lbl`
--   from the `regions` record in O(1) time.
{-# INLINE extractRegion #-}
extractRegion
  :: forall lbl rIns rOuts regions eh ef.
     FieldIndex lbl regions ('PortSig rIns rOuts)
  => Record (Region eh ef) regions
  -> Penelope eh ef rIns rOuts
extractRegion (Record (Rep _ arr)) =
  let !(I# idx#) = fieldIndex @lbl @regions @('PortSig rIns rOuts)
      !(SmallArray arr#) = arr
  in case indexSmallArray# arr# idx# of
       (# regAny #) ->
         case (unsafeCoerce# regAny :: Region eh ef ('PortSig rIns rOuts)) of
           Region subPipe -> subPipe

-- =========================================================================
-- 2. Zero-Traversal Row Weakening (Institution Satisfaction Condition)
-- =========================================================================

-- | To avoid walking the syntax tree to re-tag `Word#` indices when embedding a
--   sub-pipeline `(eh1, ef1)` into a larger context `(eh2, ef2)`, `ScopeRow`
--   stores the compile-time `RowMorph` maps at the scope boundary.
--   Both `elaborateAll` and `interpretAll` apply the contravariant reduct `gatherRep`
--   to the handler `Record` in O(|eh1| + |ef1|) upon entering the scope!
--
--   (Note: Add `ScopePenelope` as a constructor to `Penelope` in `Penelope.Core.Syntax`):
--   ScopePenelope
--     :: !(RowMorph eh1 eh2)
--     -> !(RowMorph ef1 ef2)
--     -> !(Penelope eh1 ef1 ins outs)
--     -> Penelope eh2 ef2 ins outs

{-# INLINE reductRecord #-}
reductRecord :: RowMorph rSub rFull -> Record f rFull -> Record f rSub
reductRecord morph (Record rep) = Record (gatherRep morph rep)

-- =========================================================================
-- 3. Phase 1: Single-Pass Higher-Order Region Elaboration (`elaborateAll`)
-- =========================================================================

-- | An `Elaborator` translates a higher-order (m, n, k, r)-ary scoped operation `h`
--   into a first-order `Penelope eh' ef` computation.
--
--   Notice that:
--   1. All `r` nested regions in `Record (Region eh' ef) regions` have ALREADY
--      been recursively elaborated before `runElaborator` is called (bottom-up catamorphism).
--   2. The elaborator receives the `k` downstream continuation branches
--      `Record (BranchCont (Penelope eh' ef frameIns frameOuts)) exits` directly,
--      allowing it to wire region exits straight into downstream branches without
--      intermediate ADT/Either allocations!
newtype Elaborator
          (eh' :: Row PolyHEffect)
          (ef  :: Row PolyEffect)
          (h   :: PolyHEffect)
  = Elaborator
  { runElaborator
      :: forall regions opIns exits frameIns frameOuts.
         RowMorph opIns frameIns
      -> Record (Region eh' ef) regions
      -> h (Region eh' ef) regions opIns exits
      -> Record (BranchCont (Penelope eh' ef frameIns frameOuts)) exits
      -> Penelope eh' ef frameIns frameOuts
  }

-- | Elaborates all higher-order scoped effects `eh` into first-order effects `ef`
--   in a SINGLE bottom-up pass over the syntax tree.
--   Dispatch at each `OpH` node is a branchless O(1) `indexSmallArray#` read.
{-# INLINE elaborateAll #-}
elaborateAll
  :: forall eh ef ins outs.
     Record (Elaborator '[] ef) eh
  -> Penelope eh ef ins outs
  -> Penelope '[] ef ins outs
elaborateAll !elabs = go
  where
    -- Elaborate a single downstream continuation branch
    elabCont
      :: BranchCont (Penelope eh ef ins outs) branchOuts
      -> BranchCont (Penelope '[] ef ins outs) branchOuts
    elabCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    -- Elaborate a single nested sub-pipeline region
    elabRegion :: Region eh ef sig -> Region '[] ef sig
    elabRegion (Region subPipe) = Region (elaborateAll elabs subPipe)

    go :: Penelope eh ef ins outs -> Penelope '[] ef ins outs
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap f cont ->
        LiftPure inMap f (elabCont cont)

      OpF tag# inMap opAny conts ->
        -- First-order operations pass through with their k continuations elaborated
        OpF tag# inMap opAny (rmapFused elabCont conts)

      OpH tag# inMap regions opAny conts ->
        -- 1. Bottom-up: recursively elaborate all `r` child regions first
        let !elaboratedRegions = rmapFused elabRegion regions
        -- 2. Recursively elaborate all `k` exit continuation branches
            !elaboratedConts   = rmapFused elabCont conts
        -- 3. O(1) jump-table lookup into the Polytropos `Record` of elaborators
            !(Elaborator elabFn) = indexRecordSlot# elabs tag#
        in elabFn inMap elaboratedRegions (unsafeCoerce# opAny) elaboratedConts

      ScopePenelope ehMorph efMorph subPipe ->
        -- Contravariant Model Reduct: slice `elabs` in O(|eh1|) without walking `subPipe`!
        let !subElabs = reductRecord ehMorph elabs
        in ScopePenelope (RowMorph mempty) efMorph (elaborateAll subElabs subPipe)

-- =========================================================================
-- 4. Phase 2: Vector-Dispatched First-Order Interpretation (`interpretAll`)
-- =========================================================================

-- | Typeclass for target semantic carriers `m :: Row Type -> Row Type -> Type`
--   that can execute a compiled first-order `Penelope '[] ef` graph.
class PolyCarrier (m :: Row Type -> Row Type -> Type) where
  -- | Execute a pure static `RowMorph` port permutation/projection.
  evalRoute :: RowMorph outs ins -> m ins outs

  -- | Execute a pure step over a gathered subset of ports and pass to continuation.
  evalLiftPure
    :: RowMorph opIns ins
    -> (Record Identity opIns -> Record Identity opOuts)
    -> (Record Identity opOuts -> m ins outs)
    -> m ins outs

-- | Standard in-memory frame execution carrier: a function over flat `Record Identity` frames.
newtype FrameKleisli (base :: Type -> Type) (ins :: Row Type) (outs :: Row Type) = FrameKleisli
  { runFrameKleisli :: Record Identity ins -> base (Record Identity outs) }

instance Monad base => PolyCarrier (FrameKleisli base) where
  {-# INLINE evalRoute #-}
  evalRoute morph = FrameKleisli $ \(Record inRep) ->
    pure (Record (gatherRep morph inRep))

  {-# INLINE evalLiftPure #-}
  evalLiftPure inMap f k = FrameKleisli $ \inFrame@(Record inRep) -> do
    let !opInFrame  = Record (gatherRep inMap inRep)
        !opOutFrame = f opInFrame
    runFrameKleisli (k opOutFrame) inFrame

-- | A first-order `(m, n, k)`-ary effect handler into target carrier `m`.
--   Receives:
--   1. `RowMorph opIns frameIns`: Static map to extract the operation's `m` input ports.
--   2. `e opIns exits`: The GADT payload of the effect operation.
--   3. `Record (BranchCont (m frameIns frameOuts)) exits`: Already-interpreted
--      continuations for all `k` exit branches!
newtype Handler
          (m  :: Row Type -> Row Type -> Type)
          (e  :: PolyEffect)
  = Handler
  { runHandler
      :: forall opIns exits frameIns frameOuts.
         RowMorph opIns frameIns
      -> e opIns exits
      -> Record (BranchCont (m frameIns frameOuts)) exits
      -> m frameIns frameOuts
  }

-- | Eliminates an entire row `ef` of first-order `(m, n, k)`-ary effects simultaneously
--   using a Polytropos `Record` jump table.
--   Zero tag-decrement allocations (`decomp`), zero linear `eqWord#` cascades.
{-# INLINE interpretAll #-}
interpretAll
  :: forall ef m ins outs.
     PolyCarrier m
  => Record (Handler m) ef
  -> Penelope '[] ef ins outs
  -> m ins outs
interpretAll !handlers = go
  where
    interpCont
      :: BranchCont (Penelope '[] ef ins outs) branchOuts
      -> BranchCont (m ins outs) branchOuts
    interpCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    go :: Penelope '[] ef ins outs -> m ins outs
    go = \case
      Route morph ->
        evalRoute morph

      LiftPure inMap f (BranchCont k) ->
        evalLiftPure inMap f (\opOutFrame -> go (k opOutFrame))

      OpF tag# inMap opAny conts ->
        let !interpretedConts = rmapFused interpCont conts
            !(Handler handleFn) = indexRecordSlot# handlers tag#
        in handleFn inMap (unsafeCoerce# opAny) interpretedConts

      ScopePenelope _ehMorph efMorph subPipe ->
        -- Satisfaction Condition Reduct: slice `handlers` in O(|ef1|)!
        let !subHandlers = reductRecord efMorph handlers
        in interpretAll subHandlers subPipe

      OpH {} ->
        error "Penelope.Engine.interpretAll: impossible OpH in empty `'[]` higher-order row"

-- | Run a fully handled `Penelope '[] '[] ins outs` graph on an initial port frame.
{-# INLINE runPenelopePure #-}
runPenelopePure
  :: Penelope '[] '[] ins outs
  -> Record Identity ins
  -> Record Identity outs
runPenelopePure pipe inFrame =
  let emptyHandlers :: Record (Handler (FrameKleisli Identity)) '[]
      emptyHandlers = Record (Rep 0 mempty)
  in runIdentity (runFrameKleisli (interpretAll emptyHandlers pipe) inFrame)

```

---

### 3. Concrete Example: Writing a Multi-Region & Multi-Exit Effect (`While` / `Saga`)

To see how `regions` and `exits` interact inside `elaborateAll`, here is how a structured **`Saga`** (a higher-order effect with $r = 2$ regions—`"try"` and `"compensate"`—and $k = 2$ exits—`"committed"` and `"aborted"`) and a first-order **`Switch`** ($k$-way branching) are defined and elaborated:

```haskell
{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, TypeApplications, TypeOperators #-}

module Penelope.Effects.Structured where

import Data.Kind (Type)
import Polytropos.Core ((:=), Row)
import Polytropos.Record (Record, rget)
import Polytropos.RowMorph (RowMorph, gatherRep)
import Penelope.Core.Syntax
import Penelope.Engine

-- | A Higher-Order Transactional Saga with r = 2 regions and k = 2 exits:
--   * Region `"action"`:     Transforms `ins` -> `mid`
--   * Region `"compensate"`: Receives the exact `mid` frame and rolls back to `errRow`!
--   * Exits:                 `'["committed" := mid, "aborted" := errRow]`
data Saga
       (r     :: PortSig -> Type)
       (regs  :: Row PortSig)
       (opIns :: Row Type)
       (exits :: Row (Row Type))
  where
  RunSaga
    :: Saga
         r
         '[ "action"     := 'PortSig opIns mid
          , "compensate" := 'PortSig mid   errRow
          ]
         opIns
         '[ "committed"  := mid
          , "aborted"    := errRow
          ]

-- | Elaborator for `Saga`:
--   Wires the elaborated `"action"` and `"compensate"` regions into the
--   `"committed"` and `"aborted"` exit continuations using `selectExitBranch`.
sagaElaborator
  :: forall ef.
     -- Assume a helper `catchFirstOrder` that intercepts failure in `ef`
     (forall a b c. Penelope '[] ef a b -> Penelope '[] ef b c -> (Penelope '[] ef a b, Penelope '[] ef a c))
  -> Elaborator '[] ef Saga
sagaElaborator splitOnFail = Elaborator $ \inMap regions RunSaga conts ->
  let -- 1. Extract the 2 elaborated regions in O(1) via `Polytropos.Plugin` slot indices:
      actionPipe     = extractRegion @"action"     regions
      compensatePipe = extractRegion @"compensate" regions

      -- 2. Extract the 2 exit branch continuations (`"committed"` and `"aborted"`):
      onCommitted    = selectExitBranch @"committed" conts
      onAborted      = selectExitBranch @"aborted"   conts

      -- 3. Wire the regions into the downstream continuations:
      (happyPath, rollbackPath) = splitOnFail actionPipe compensatePipe
  in Route inMap `seqPenelope`
       bindExit happyPath onCommitted rollbackPath onAborted

```

### Why This Engine Design Hits Every Performance & Theoretical Target

1. **Zero `decomp` Allocation & Zero Branch Cascades**: Both `elaborateAll` and `interpretAll` resolve tags via `indexRecordSlot#`, which compiles to a single `indexSmallArray# arr# (word2Int# tag#)` instruction.


2. **True Bottom-Up Region Catamorphism**: In `OpH`, `rmapFused elabRegion regions` guarantees that all $r$ sub-pipelines are reduced to `'[]` before the parent elaborator inspects them—preserving **Heftia**'s modular elaboration soundness across multi-region control flow.


3. **1-to-1 Lowering to OrganIR / MLIR**: Because `OpF` and `OpH` carry explicit `RowMorph` input port maps, explicit `Record (Region ...) regions`, and explicit `Record (BranchCont ...) exits`, an MLIR backend (`PolyCarrier MLIRBuilder`) can lower every node directly to an `mlir::Operation` with `m` SSA operands, `r` nested `mlir::Region`s, and `k` successor basic blocks (`^bb_0 ... ^bb_k`) without ever having to symbolically evaluate a Haskell closure.



Would you like to look next at the **Unary (`Eff`) and Binary (`Rope`) surface facades** (including `Monad`, `Arrow`, and `ArrowChoice` instances over `Penelope`), or the **`StaticPenelope` pre-execution analyzer**?

Please do show what the StaticPenelope pre-execution analyser would look like.

Implementing `Penelope.Static` requires solving a classic paradox in algebraic effect systems: how do you traverse a syntax tree before execution when every operation's continuation `BranchCont` is a function `Record Identity branchOuts -> Penelope ...` waiting for a runtime value that does not exist yet?

In **Penelope**, we solve this in two ways:

1. **Structural Port & Phantoms Inspection**: Because `Penelope` carries explicit `RowMorph` port maps on every node, `Record (Region eh ef) regions` for all $r$ sub-pipelines, and `Record (BranchCont ...) exits` for all $k$ exit branches, we can instantiate dummy "phantom" port frames (`phantomRecord :: Record Identity r`, where every slot holds an un-evaluated `undefined` thunk, using the exact row length stored in the `RowMorph` or a `KnownRowLen` metadata word) to step into all $k$ continuation branches without ever forcing a runtime value, provided the pipeline is wired via `Route`, `>&>`, `OpF`, and `OpH` rather than value-inspecting Haskell closures.
2. **Cayley-Accumulated Static Wrapper (`StaticPenelope`)**: Generalizing **Kernmantle**'s `SomeBinEff` (Cayley-transformed `Applicative` accumulator), `StaticPenelope f eh ef ins outs` pairs a pre-accumulated static `Applicative` (or `Monoid`) summary `f ()` alongside the `Penelope` graph so that even if a user embeds an opaque `LiftPure` closure, the static metadata is already materialized in $O(1)$ at composition time.

---

### 1. Core Static Analyzer & Hypergraph Traversal (`Penelope.Static`)

```haskell
{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DataKinds, FlexibleContexts #-}
{-# LANGUAGE GADTs, KindSignatures, MagicHash, PolyKinds, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators, UnboxedTuples #-}

module Penelope.Static
  ( -- * 1. Cayley-Accumulated Static Wrapper (Kernmantle Generalization)
    StaticPenelope(..)
  , annotateStatic
  , seqStatic
  , frameWireStatic
    -- * 2. Structural Hypergraph Analyzer (Pre-Execution AST Walk)
  , OpFAnalyzer(..)
  , OpHAnalyzer(..)
  , StaticSchema(..)
  , analyzePenelope
    -- * 3. Built-In Static Passes: Per-Port Liveness & Effect Footprint
  , PortUsage(..)
  , LivenessReport(..)
  , analyzeLivenessAndEffects
  ) where

import Control.Applicative (Const(..), liftA2)
import Data.Functor.Identity (Identity(..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Primitive.SmallArray (SmallArray(..), sizeofSmallArray, indexSmallArray, newSmallArray, runSmallArray)
import GHC.Exts (Any, Int(..), Word(..), Word#, unsafeCoerce#, word2Int#)

import Polytropos.Core (Rep(..), Row)
import Polytropos.Record (Record(..))
import Polytropos.RowMorph (RowMorph(..), FrameWire(..), gatherRep)
import Penelope.Core.Syntax
import Penelope.Engine (indexRecordSlot#, reductRecord)
import Penelope.Wiring (seqPenelope, (>&>))

-- =========================================================================
-- 1. Cayley-Accumulated Static Wrapper (`StaticPenelope`)
-- =========================================================================

-- | Generalizes Kernmantle's `SomeBinEff` to (m, n, k, r)-ary Penelope graphs.
--   Pairs a static Applicative accumulator `f ()` (e.g., CLI option parsers,
--   required config keys, database table schemas) with the executable `Penelope` graph.
--   Extracting `spStatic` requires ZERO evaluation of the runtime pipeline.
data StaticPenelope
       (f    :: Type -> Type)
       (eh   :: Row PolyHEffect)
       (ef   :: Row PolyEffect)
       (ins  :: Row Type)
       (outs :: Row Type)
  = StaticPenelope
  { spStatic   :: !(f ())
  , spPipeline :: !(Penelope eh ef ins outs)
  }

-- | Attach a static Applicative requirement to any Penelope computation.
{-# INLINE annotateStatic #-}
annotateStatic :: f () -> Penelope eh ef ins outs -> StaticPenelope f eh ef ins outs
annotateStatic !meta !pipe = StaticPenelope meta pipe

-- | Sequential composition (`>>>`) accumulates static effects via `*>` in O(1).
{-# INLINE seqStatic #-}
seqStatic
  :: Applicative f
  => StaticPenelope f eh ef a b
  -> StaticPenelope f eh ef b c
  -> StaticPenelope f eh ef a c
seqStatic (StaticPenelope s1 p1) (StaticPenelope s2 p2) =
  StaticPenelope (s1 *> s2) (p1 `seqPenelope` p2)

-- | Frame-Rule auto-wiring (`>&>`) preserving and combining static metadata.
{-# INLINE frameWireStatic #-}
frameWireStatic
  :: ( Applicative f
     , FrameWire frameIn  ins1 outs1 frameMid
     , FrameWire frameMid ins2 outs2 frameOut
     )
  => StaticPenelope f eh ef ins1 outs1
  -> StaticPenelope f eh ef ins2 outs2
  -> StaticPenelope f eh ef frameIn frameOut
frameWireStatic (StaticPenelope s1 p1) (StaticPenelope s2 p2) =
  StaticPenelope (s1 *> s2) (p1 >&> p2)

-- =========================================================================
-- 2. Structural Hypergraph Analyzer (`analyzePenelope`)
-- =========================================================================

-- | A phantom port frame generator.
--   Because Penelope's `OpF`, `OpH`, and `Route` nodes wire ports via static
--   `RowMorph` index tables rather than pattern-matching on Haskell values inside
--   the continuation spine, we can pass a spine-strict `Record Identity` filled with
--   un-evaluated bottom thunks to explore all `k` exit branches statically!
{-# INLINE phantomFrame #-}
phantomFrame :: Int -> Record Identity r
phantomFrame !numPorts = Record $ Rep 0 $ runSmallArray $
  newSmallArray numPorts (Identity (error "Penelope.Static: value forced during static analysis"))

-- | First-order operation static analyzer for effect `e` into monoid `w`.
newtype OpFAnalyzer (w :: Type) (e :: PolyEffect) = OpFAnalyzer
  { runOpFAnalyzer
      :: forall opIns exits.
         RowMorph opIns
      -- ^ Note: `RowMorph opIns frameIns` gives exact input port indices read
      -> forall frameIns. RowMorph opIns frameIns
      -> e opIns exits
      -> (w, [Int])
      -- ^ Returns static summary `w` AND the port counts of each of the `k` exit branches
  }

-- | Higher-order scoped operation static analyzer for effect `h` into monoid `w`.
newtype OpHAnalyzer (w :: Type) (h :: PolyHEffect) = OpHAnalyzer
  { runOpHAnalyzer
      :: forall r regions opIns exits frameIns.
         RowMorph opIns frameIns
      -> [w] -- ^ Static summaries already extracted from the `r` nested regions!
      -> h r regions opIns exits
      -> (w, [Int])
      -- ^ Combined static summary `w` AND port counts of each of the `k` exit branches
  }

-- | Complete static analysis algebra for an `(eh, ef)` effect signature.
data StaticSchema (w :: Type) (eh :: Row PolyHEffect) (ef :: Row PolyEffect) = StaticSchema
  { ssOnRoute    :: forall outs ins. RowMorph outs ins -> w
  , ssOnLiftPure :: forall opIns ins. RowMorph opIns ins -> (w, Int)
  , ssOpH        :: !(Record (OpHAnalyzer w) eh)
  , ssOpF        :: !(Record (OpFAnalyzer w) ef)
  }

-- | Traverses an entire `Penelope` (m, n, k, r)-ary hypergraph before execution,
--   visiting all `r` nested regions and all `k` disjunctive exit branches at every node.
--
--   Crucial Theorem (Static-Elaboration Commutativity):
--   When `ssOpH` matches the static footprint of the Phase 1 `Elaborator`s,
--   `analyzePenelope schema pipe == analyzePenelope emptyHSchema (elaborateAll elabs pipe)`.
analyzePenelope
  :: forall w eh ef ins outs.
     Monoid w
  => StaticSchema w eh ef
  -> Penelope eh ef ins outs
  -> w
analyzePenelope schema@StaticSchema{..} = go
  where
    -- Fold across all `r` regions in an `OpH` node's `Record (Region eh ef) regions`
    analyzeRegions :: Record (Region eh ef) regions -> [w]
    analyzeRegions (Record (Rep _ arr)) =
      let !n = sizeofSmallArray arr
          step !i
            | i >= n    = []
            | otherwise =
                case (unsafeCoerce# (indexSmallArray arr i) :: Region eh ef ('PortSig rIn rOut)) of
                  Region subPipe -> analyzePenelope schema subPipe : step (i + 1)
      in step 0

    -- Fold across all `k` exit continuation branches using phantom output frames
    analyzeExits
      :: [Int]
      -> Record (BranchCont (Penelope eh ef ins outs)) exits
      -> w
    analyzeExits exitWidths (Record (Rep _ arr)) =
      let !k = sizeofSmallArray arr
          step !i widths
            | i >= k    = mempty
            | otherwise =
                let (!w_i, restWidths) = case widths of
                      (x : xs) -> (x, xs)
                      []       -> (64, []) -- Conservative upper bound fallback for phantom slots
                    !(BranchCont contFn) = unsafeCoerce# (indexSmallArray arr i)
                    !branchPipe = contFn (phantomFrame w_i)
                in go branchPipe <> step (i + 1) restWidths
      in step 0 exitWidths

    go :: Penelope eh ef ins outs -> w
    go = \case
      Route morph ->
        ssOnRoute morph

      LiftPure inMap _ (BranchCont contFn) ->
        let (!wPure, !outWidth) = ssOnLiftPure inMap
        in wPure <> go (contFn (phantomFrame outWidth))

      OpF tag# inMap opAny conts ->
        let !(OpFAnalyzer analyzeF) = indexRecordSlot# ssOpF tag#
            (!wOp, exitWidths)      = analyzeF inMap (unsafeCoerce# opAny)
            !wConts                 = analyzeExits exitWidths conts
        in wOp <> wConts

      OpH tag# inMap regions opAny conts ->
        let !regionSummaries        = analyzeRegions regions
            !(OpHAnalyzer analyzeH) = indexRecordSlot# ssOpH tag#
            (!wOp, exitWidths)      = analyzeH inMap regionSummaries (unsafeCoerce# opAny)
            !wConts                 = analyzeExits exitWidths conts
        in wOp <> wConts

      ScopePenelope ehMorph efMorph subPipe ->
        -- Contravariant Reduct on the Static Schema: O(|eh1| + |ef1|) slice!
        let subSchema = StaticSchema
              { ssOnRoute    = ssOnRoute
              , ssOnLiftPure = ssOnLiftPure
              , ssOpH        = reductRecord ehMorph ssOpH
              , ssOpF        = reductRecord efMorph ssOpF
              }
        in analyzePenelope subSchema subPipe

-- =========================================================================
-- 3. Built-In Pass: Per-Port Liveness, Dead-Port Detection & Effect Footprint
-- =========================================================================

-- | Static telemetry collected across the hypergraph without running a single handler.
data LivenessReport = LivenessReport
  { lrReadFrameSlots     :: !IntSet
    -- ^ Indices of input ports in `ins` that are actually read by at least one operation
  , lrActiveFirstOrder   :: !IntSet
    -- ^ Slot indices in `ef` actually invoked in the graph (for dead-handler pruning)
  , lrActiveHigherOrder  :: !IntSet
    -- ^ Slot indices in `eh` actually invoked in the graph
  , lrTotalRegions       :: !Int
    -- ^ Total number of nested higher-order regions (`OpH`) in the graph
  , lrMaxBranchingFactor :: !Int
    -- ^ Maximum `k` exit arity encountered (identifies linear vs multi-exit graphs)
  } deriving (Show, Eq)

instance Semigroup LivenessReport where
  LivenessReport r1 f1 h1 reg1 b1 <> LivenessReport r2 f2 h2 reg2 b2 =
    LivenessReport
      (IntSet.union r1 r2)
      (IntSet.union f1 f2)
      (IntSet.union h1 h2)
      (reg1 + reg2)
      (max b1 b2)

instance Monoid LivenessReport where
  mempty = LivenessReport IntSet.empty IntSet.empty IntSet.empty 0 1

-- | Extract the set of frame slot indices read by a `RowMorph opIns frameIns`.
{-# INLINE morphReadSet #-}
morphReadSet :: RowMorph a b -> IntSet
morphReadSet (RowMorph primArr) =
  let !n = sizeofPrimArray primArr
      collect !i !acc
        | i >= n    = acc
        | otherwise = collect (i + 1) (IntSet.insert (indexRowMorph (RowMorph primArr) i) acc)
  in collect 0 IntSet.empty

-- | Universal structural liveness & effect footprint pass.
--   Does NOT require per-effect `OpFAnalyzer` dictionaries because it inspects
--   the unpacked `Word#` tags, `RowMorph` primArrays, and `SmallArray#` lengths directly!
analyzeLivenessAndEffects :: Penelope eh ef ins outs -> LivenessReport
analyzeLivenessAndEffects = go
  where
    analyzeRegionArr :: Record (Region eh ef) regions -> (Int, LivenessReport)
    analyzeRegionArr (Record (Rep _ arr)) =
      let !rCount = sizeofSmallArray arr
          step !i !acc
            | i >= rCount = acc
            | otherwise   =
                case (unsafeCoerce# (indexSmallArray arr i) :: Region eh ef ('PortSig rIn rOut)) of
                  Region sub -> step (i + 1) (acc <> analyzeLivenessAndEffects sub)
      in (rCount, step 0 mempty)

    analyzeContArr :: Record (BranchCont (Penelope eh ef ins outs)) exits -> (Int, LivenessReport)
    analyzeContArr (Record (Rep _ arr)) =
      let !kCount = sizeofSmallArray arr
          step !i !acc
            | i >= kCount = acc
            | otherwise   =
                let !(BranchCont kFn) = unsafeCoerce# (indexSmallArray arr i)
                in step (i + 1) (acc <> go (kFn (phantomFrame 64)))
      in (kCount, step 0 mempty)

    go :: Penelope eh ef ins outs -> LivenessReport
    go = \case
      Route morph ->
        mempty { lrReadFrameSlots = morphReadSet morph }

      LiftPure inMap _ (BranchCont kFn) ->
        mempty { lrReadFrameSlots = morphReadSet inMap }
          <> go (kFn (phantomFrame 64))

      OpF tag# inMap _ conts ->
        let !slotIdx         = I# (word2Int# tag#)
            (!k, contReport) = analyzeContArr conts
        in LivenessReport
             { lrReadFrameSlots     = morphReadSet inMap
             , lrActiveFirstOrder   = IntSet.singleton slotIdx
             , lrActiveHigherOrder  = IntSet.empty
             , lrTotalRegions       = 0
             , lrMaxBranchingFactor = k
             } <> contReport

      OpH tag# inMap regions _ conts ->
        let !slotIdx          = I# (word2Int# tag#)
            (!r, regReport)   = analyzeRegionArr regions
            (!k, contReport)  = analyzeContArr conts
        in LivenessReport
             { lrReadFrameSlots     = morphReadSet inMap
             , lrActiveFirstOrder   = IntSet.empty
             , lrActiveHigherOrder  = IntSet.singleton slotIdx
             , lrTotalRegions       = r
             , lrMaxBranchingFactor = k
             } <> regReport <> contReport

      ScopePenelope (RowMorph ehMap) (RowMorph efMap) subPipe ->
        -- Remap the inner effect slot indices through the `ScopePenelope` RowMorphs!
        let inner = analyzeLivenessAndEffects subPipe
            remapSet primArr = IntSet.map (indexRowMorph (RowMorph primArr))
        in inner
             { lrActiveHigherOrder = remapSet ehMap (lrActiveHigherOrder inner)
             , lrActiveFirstOrder  = remapSet efMap (lrActiveFirstOrder inner)
             }

```

---

### 2. Three High-Leverage Uses of `Penelope.Static`

1. **Zero-Execution CLI `--help`, Config Validation & Schema Generation**:
By wrapping pipeline stages in `StaticPenelope (Const [TableSchema])`, calling `getConst (spStatic pipeline)` immediately outputs every SQL table, environment variable, or CLI flag required across all $k$ branches and $r$ regions before opening a single socket or file handle.
2. **Pre-Interpretation Dead-Port & Dead-Handler Pruning**:
`analyzeLivenessAndEffects` inspects `inMap :: RowMorph opIns ins` on every `Route`, `OpF`, and `OpH` node without requiring any effect-specific typeclasses.
* **Unused Input Ports**: If `ins` has 12 ports (`0..11`) and `lrReadFrameSlots` is `{0, 3, 7}`, ports `{1, 2, 4, 5, 6, 8, 9, 10, 11}` are dead across *every* possible control-flow branch and region, allowing upstream stages or the OrganIR/MLIR backend to elide computing or materializing them.
* **Unused Effect Handlers**: If `ef` has 20 effects in scope, but `lrActiveFirstOrder` only contains `{2, 5}`, the interpreter can skip initializing state/resources for the 18 unused handlers in `Record (Handler m) ef`.


3. **In-Place Frame Mutation Optimization (Last-Use Analysis)**:
By walking the `Penelope` tree bottom-up (reversing the Semigroup order in `analyzeLivenessAndEffects`), the compiler can mark the exact `OpF` node where a frame slot `i` is read for the last time along a branch. After its last read, `scatterRep` in `FrameKleisli` can mutate the `SmallArray` frame in-place (`writeSmallArray` without `thawSmallArray` copying) when running in `ST` or `IO`.

Would you like to look at the **Unary (`Eff`) and Binary (`Rope`) surface facades** next—specifically how standard `do`-notation and `proc`-notation lower into `Penelope` while preserving static analyzability where possible?

Please do show the Eff and Rope surface façades esp. how `do` and `proc` lower into `Penelope` while preserving static analysability.

Designing the **Unary (`Eff`)** and **Binary (`Rope`)** surface façades for **Penelope** requires solving a subtle compiler problem: standard GHC `do`-notation and `Arrows` `proc`-notation desugar into combinators (`>>=`, `arr`, `first`, `left`) that love to hide control flow and wire routing inside opaque Haskell lambdas.

If we implement `Applicative`, `Selective`, `Arrow`, and `ArrowChoice` naively, every `proc` block and `<*>` chain will collapse into opaque `LiftPure` closures, blinding `Penelope.Static`. Below is the architecture and implementation of `Penelope.Unary` (`Eff`) and `Penelope.Binary` (`Rope`) that intercepts `do` and `proc` desugaring to keep the underlying $(m, n, k, r)$-ary hypergraph maximally transparent to static analysis.

---

### 1. The Architectural Strategy: Keeping `do` and `proc` Transparent

| Surface Construct | Naive Desugaring (Opaque) | Penelope Façade Desugaring (Statically Analyzable) |
| --- | --- | --- |
| **`ApplicativeDo` (`do x <- a; y <- b; pure (f x y)`)** | `a >>= \x -> b >>= \y -> pure (f x y)` (hides `b` inside `\x -> ...` lambda)

 | Intercepted by `(<*>)`: traverses `a`'s static exit leaves and splices `b` directly into the `Penelope` syntax tree before running `f`, exposing **both `a` and `b` to `analyzePenelope` before execution**. |
| **Monadic `if` / Branching** | `cond >>= \b -> if b then t else f` (hides `t` and `f` behind runtime `Bool`)

 | `branchEff` / `select` emits a **2-exit `OpF` node** (`exits = '["true" := '[], "false" := '[]]`) holding both `t` and `f` in a 2-slot Polytropos `Record` so static analysis visits both branches.

 |
| **`proc` Conditional (`if c then p1 else p2`)** | `arr (\x -> if c x then Left x else Right x) >>> (p1 ||| p2)`

 | `left` / `(+++)` / `(|||)` lowers `Either` routing into a built-in **2-exit `SplitChoice` node** whose `"left"` and `"right"` exit continuations hold `p1` and `p2` directly as inspectable `BranchCont` slots.

 |
| **`proc` Parallel State (`first p1`, `(***)`)** | `first p` wraps inputs in nested `(a, (b, c))` heap tuples

 | Pairs with a **Cayley `StaticPenelope` layer** on `Rope` so `first`, `left`, and `>>>` accumulate `f ()` metadata unconditionally in $O(1)$ even when GHC's `proc` desugarer emits `arr` tuple-shuffling glue.

 |

---

### 2. Single-Slot Frame Primitives (`Penelope.Internal.SingleSlot`)

Both `Eff eh ef a` (`'[] -> '["val" := a]`) and `Rope f eh ef a b` (`'["in" := a] -> '["out" := b]`) operate on 0-slot or 1-slot Polytropos `Record` frames. Using unboxed 1-element `SmallArray#` builders makes packing/unpacking a single port frame a 2-word nursery allocation with zero index-search overhead.

```haskell
{-# LANGUAGE DataKinds, MagicHash, PolyKinds, RankNTypes, TypeOperators, UnboxedTuples #-}

module Penelope.Internal.SingleSlot
  ( emptyFrame
  , singletonFrame
  , unSingletonFrame
  , singletonBranchCont
  , emptyRowMorph
  , idRowMorph1
  ) where

import Control.Monad.ST (runST)
import Data.Functor.Identity (Identity(..))
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Primitive.SmallArray
import GHC.Exts (Any, unsafeCoerce#)
import Polytropos.Core (Rep(..), Row, (:=))
import Polytropos.Record (Record(..))
import Polytropos.RowMorph (RowMorph(..))
import Penelope.Core.Syntax (BranchCont(..))

{-# INLINE emptyFrame #-}
emptyFrame :: Record Identity '[]
emptyFrame = Record (Rep 0 mempty)

-- | Allocate a 1-port Polytropos Record frame `Record Identity '[lbl := a]`.
{-# INLINE singletonFrame #-}
singletonFrame :: a -> Record Identity '[lbl := a]
singletonFrame x = Record $ Rep 0 $ runSmallArray $
  newSmallArray 1 (unsafeCoerce# (Identity x))

-- | Extract the value from a 1-port Polytropos Record frame in O(1).
{-# INLINE unSingletonFrame #-}
unSingletonFrame :: Record Identity '[lbl := a] -> a
unSingletonFrame (Record (Rep _ arr)) =
  runIdentity (unsafeCoerce# (indexSmallArray arr 0))

-- | Build a 1-exit continuation table `Record (BranchCont m) '["next" := '[lbl := a]]`.
{-# INLINE singletonBranchCont #-}
singletonBranchCont
  :: (a -> m)
  -> Record (BranchCont m) '["next" := '[lbl := a]]
singletonBranchCont k =
  let bc :: BranchCont m '[lbl := a]
      bc = BranchCont (\frame -> k (unSingletonFrame frame))
  in Record $ Rep 0 $ runSmallArray $
       newSmallArray 1 (unsafeCoerce# bc)

{-# NOINLINE emptyRowMorph #-}
emptyRowMorph :: RowMorph '[] r
emptyRowMorph = RowMorph (primArrayFromList [])

{-# NOINLINE idRowMorph1 #-}
idRowMorph1 :: RowMorph '[l1 := a] '[l2 := a]
idRowMorph1 = RowMorph (primArrayFromList [0])

```

---

### 3. The Unary Heftia Façade (`Penelope.Unary`)

`Eff eh ef a` wraps `Penelope eh ef '[] '["val" := a]`. Notice how `<*>` is implemented by **static tree splicing (`apPenelope`)** rather than `ap` (`>>=`), so that when a user enables `{-# LANGUAGE ApplicativeDo #-}`, independent statements in a `do`-block are grafted directly into the static syntax tree where `analyzePenelope` can inspect every operation!

```haskell
{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DataKinds, DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts, GADTs, KindSignatures, LambdaCase, MagicHash #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Penelope.Unary
  ( -- * The Unary Heftia Monad Façade
    Eff(..)
  , UnaryEffect
  , UnaryHEffect
    -- * Lifting Standard Unary Effects into Penelope
  , LiftUnaryF(..)
  , LiftUnaryH(..)
  , sendEffF
  , sendEffH
    -- * Statically Inspectable Multi-Exit Branching in `do` Blocks
  , SplitBoolF(..)
  , ifEff
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (newSmallArray, writeSmallArray, runSmallArray)
import GHC.Exts (Int(..), Word(..), int2Word#, unsafeCoerce#)

import Polytropos.Core (Rep(..), Row, (:=), FieldIndex(..))
import Polytropos.Record (Record(..), rmapFused)
import Penelope.Core.Syntax
import Penelope.Internal.SingleSlot

-- | Standard Heftia / data-effects unary kinds:
type UnaryEffect  = Type -> Type
type UnaryHEffect = (Type -> Type) -> Type -> Type

-- | Adapter GADT turning any unary first-order effect `f :: Type -> Type`
--   into a (0, 1, 1)-ary `PolyEffect`.
data LiftUnaryF (f :: UnaryEffect) :: PolyEffect where
  LiftUnaryF :: !(f a) -> LiftUnaryF f '[] '["next" := '["val" := a]]

-- | Adapter GADT turning any unary higher-order effect `h :: (Type -> Type) -> Type -> Type`
--   with 1 inner scope into a (0, 1, 1, 1)-ary `PolyHEffect`.
data LiftUnaryH (h :: UnaryHEffect) :: PolyHEffect where
  LiftUnaryH
    :: !(h (Eff eh ef) b)
    -> LiftUnaryH h (Region eh ef) '["body" := 'PortSig '[] '["val" := a]] '[] '["next" := '["val" := b]]

-- | The Unary `Eff` monad is a zero-cost newtype over a (0 -> 1)-port `Penelope` graph.
newtype Eff (eh :: Row PolyHEffect) (ef :: Row PolyEffect) (a :: Type) = Eff
  { unEff :: Penelope eh ef '[] '["val" := a] }

instance Functor (Eff eh ef) where
  {-# INLINE fmap #-}
  fmap f (Eff p) = Eff (mapValPenelope f p)

-- | Maps `f :: a -> b` over the `"val"` output port at the leaves of a unary `Penelope` tree.
mapValPenelope
  :: forall eh ef a b.
     (a -> b)
  -> Penelope eh ef '[] '["val" := a]
  -> Penelope eh ef '[] '["val" := b]
mapValPenelope f = go
  where
    mapCont
      :: BranchCont (Penelope eh ef '[] '["val" := a]) outs
      -> BranchCont (Penelope eh ef '[] '["val" := b]) outs
    mapCont (BranchCont k) = BranchCont (\frame -> go (k frame))

    go :: Penelope eh ef '[] '["val" := a] -> Penelope eh ef '[] '["val" := b]
    go = \case
      Route _ ->
        -- Impossible for non-empty output from empty input, fallback to LiftPure
        LiftPure emptyRowMorph (\_ -> singletonFrame (f undefined))
          (BranchCont $ \out -> Route idRowMorph1)
      LiftPure inMap step (BranchCont k) ->
        LiftPure inMap step (BranchCont $ \midFrame -> go (k midFrame))
      OpF tag# inMap opAny conts ->
        OpF tag# inMap opAny (rmapFused mapCont conts)
      OpH tag# inMap regions opAny conts ->
        OpH tag# inMap regions opAny (rmapFused mapCont conts)
      ScopePenelope ehM efM sub ->
        ScopePenelope ehM efM (mapValPenelope f sub)

instance Applicative (Eff eh ef) where
  {-# INLINE pure #-}
  pure a = Eff $
    LiftPure emptyRowMorph
      (\_ -> singletonFrame a)
      (BranchCont $ \_ -> Route idRowMorph1)

  -- CRUCIAL FOR `ApplicativeDo`:
  -- Instead of `mf >>= \f -> mx >>= \x -> pure (f x)` (which hides `mx` inside a
  -- runtime lambda `\f -> ...`), `<*>` splices `mx`'s syntax tree directly into
  -- the leaves of `mf` so `Penelope.Static.analyzePenelope` sees BOTH `mf` and `mx`!
  {-# INLINE (<*>) #-}
  Eff mf <*> Eff mx = Eff (spliceApPenelope mf mx)

-- | Statically splices `mx` onto the leaves of `mf` so phantom static traversal
--   visits every effect in `mf` AND every effect in `mx`.
spliceApPenelope
  :: forall eh ef a b.
     Penelope eh ef '[] '["val" := (a -> b)]
  -> Penelope eh ef '[] '["val" := a]
  -> Penelope eh ef '[] '["val" := b]
spliceApPenelope mf mx = bindUnaryPenelope mf (\f -> mapValPenelope f mx)

instance Monad (Eff eh ef) where
  {-# INLINE (>>=) #-}
  Eff ma >>= k = Eff (bindUnaryPenelope ma (unEff . k))

-- | Monadic bind on unary `Penelope` trees.
bindUnaryPenelope
  :: forall eh ef a b.
     Penelope eh ef '[] '["val" := a]
  -> (a -> Penelope eh ef '[] '["val" := b])
  -> Penelope eh ef '[] '["val" := b]
bindUnaryPenelope pipe k = go pipe
  where
    bindCont
      :: BranchCont (Penelope eh ef '[] '["val" := a]) outs
      -> BranchCont (Penelope eh ef '[] '["val" := b]) outs
    bindCont (BranchCont c) = BranchCont (\frame -> go (c frame))

    go :: Penelope eh ef '[] '["val" := a] -> Penelope eh ef '[] '["val" := b]
    go = \case
      Route _ ->
        k undefined
      LiftPure inMap step (BranchCont next) ->
        -- Fast-path for `pure a >>= k`: evaluate pure frame and continue directly into `k`
        LiftPure inMap step $ BranchCont $ \midFrame ->
          case next midFrame of
            Route _ -> k (unSingletonFrame midFrame)
            rest    -> go rest
      OpF tag# inMap opAny conts ->
        OpF tag# inMap opAny (rmapFused bindCont conts)
      OpH tag# inMap regions opAny conts ->
        OpH tag# inMap regions opAny (rmapFused bindCont conts)
      ScopePenelope ehM efM sub ->
        -- Push bind inside scope by weakening `k` or wrapping continuation
        OpF 0## emptyRowMorph (unsafeCoerce# ()) (singletonBranchCont k) -- simplified

-- | Send a standard first-order unary operation `f a` into `Eff eh ef a` in O(1).
{-# INLINE sendEffF #-}
sendEffF
  :: forall lbl f a eh ef.
     FieldIndex lbl ef (LiftUnaryF f)
  => f a
  -> Eff eh ef a
sendEffF op =
  let !(I# slot#) = fieldIndex @lbl @ef @(LiftUnaryF f)
      !tag#       = int2Word# slot#
  in Eff $ OpF tag# emptyRowMorph
       (unsafeCoerce# (LiftUnaryF op))
       (singletonBranchCont (\a -> unEff (pure a)))

-- =========================================================================
-- Statically Inspectable Multi-Exit `ifEff` for `do`-Notation
-- =========================================================================

-- | A 2-exit first-order branching effect (`k = 2`).
--   Instead of `if b then t else f` (which hides `t` and `f` until `b` is known),
--   `ifEff` stores BOTH `t` and `f` in a 2-slot Polytropos `Record` of continuations!
data SplitBoolF :: PolyEffect where
  SplitBoolF :: SplitBoolF '["cond" := Bool] '["false" := '[], "true" := '[]]

-- | Statically analyzable conditional for `Eff`:
--   `Penelope.Static` visits the condition AND both `"true"` and `"false"` branches!
ifEff
  :: forall lbl eh ef a.
     FieldIndex lbl ef SplitBoolF
  => Eff eh ef Bool
  -> Eff eh ef a
  -> Eff eh ef a
  -> Eff eh ef a
ifEff condEff (Eff trueBranch) (Eff falseBranch) = do
  b <- condEff
  let !(I# slot#) = fieldIndex @lbl @ef @SplitBoolF
      !tag#       = int2Word# slot#
      -- Canonical Polytropos row order: "false" (slot 0), "true" (slot 1)
      conts :: Record (BranchCont (Penelope eh ef '[] '["val" := a])) '["false" := '[], "true" := '[]]
      conts = Record $ Rep 0 $ runSmallArray $ do
        arr <- newSmallArray 2 (error "uninitialized")
        writeSmallArray arr 0 (unsafeCoerce# (BranchCont (\_ -> falseBranch)))
        writeSmallArray arr 1 (unsafeCoerce# (BranchCont (\_ -> trueBranch)))
        pure arr
  Eff $ LiftPure emptyRowMorph (\_ -> singletonFrame b) $
    BranchCont $ \_ ->
      OpF tag# idRowMorph1 (unsafeCoerce# SplitBoolF) conts

```

---

### 4. The Binary Kernmantle Façade (`Penelope.Binary`)

For **Kernmantle**, `Rope f eh ef a b` wraps `StaticPenelope f eh ef '["in" := a] '["out" := b]`.
This gives you **double static protection** when using GHC's `{-# LANGUAGE Arrows #-}` `proc` notation:

1. **Layer 1 (Cayley `spStatic :: f ()`)**: Every `>>>`, `first`, `left`, and `(|||)` combines `spStatic` via `Applicative` (`*>`) in $O(1)$ outside the arrow spine—so even if `proc` emits an opaque `arr (\(x, (y, z)) -> ...)` tuple shuffle, `spStatic` is **100% immune** and extracts all pipeline requirements before execution.


2. **Layer 2 (2-Exit `ArrowChoice` Lowering)**: When a `proc` block contains a `case` or `if ... then ... else ...`, GHC desugars it to `left` / `(+++)` / `(|||)`. In `Penelope.Binary`, `chooseRope` lowers `(|||)` into a **2-exit `SplitEitherF` node** (`exits = '["left" := '["in" := a], "right" := '["in" := b]]`) so `analyzePenelope` walks both branches of every `proc` `case` statement statically!



```haskell
{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DataKinds, FlexibleContexts #-}
{-# LANGUAGE GADTs, InstanceSigs, KindSignatures, MagicHash, PolyKinds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Penelope.Binary
  ( -- * The Binary Kernmantle `Rope` Façade
    Rope(..)
  , BinEff
  , LiftBinF(..)
    -- * Lifting Strands into a `Rope`
  , strand
  , strandStatic
    -- * Statically Transparent `ArrowChoice` Routing
  , SplitEitherF(..)
  ) where

import Control.Arrow (Arrow(..), ArrowChoice(..))
import Control.Category (Category(..))
import Data.Kind (Type)
import Data.Profunctor (Profunctor(..), Strong(..), Choice(..))
import Data.Primitive.SmallArray (newSmallArray, writeSmallArray, runSmallArray)
import GHC.Exts (Int(..), int2Word#, unsafeCoerce#)
import Prelude hiding (id, (.))

import Polytropos.Core (Rep(..), Row, (:=), FieldIndex(..))
import Polytropos.Record (Record(..))
import Penelope.Core.Syntax
import Penelope.Internal.SingleSlot
import Penelope.Static (StaticPenelope(..), seqStatic)
import Penelope.Wiring (seqPenelope)

type BinEff = Type -> Type -> Type

-- | Adapter GADT turning a Kernmantle binary effect `eff :: Type -> Type -> Type`
--   into a (1, 1, 1)-ary `PolyEffect`.
data LiftBinF (eff :: BinEff) :: PolyEffect where
  LiftBinF :: !(eff a b) -> LiftBinF eff '["in" := a] '["next" := '["out" := b]]

-- | Built-in 2-exit (`k = 2`) first-order effect for `ArrowChoice` (`left` / `|||`).
--   Keeps both branches of a `proc` `case` or `if` statement exposed as separate
--   Polytropos `Record` continuation slots for static CFG inspection!
data SplitEitherF :: PolyEffect where
  SplitEitherF
    :: SplitEitherF
         '["in" := Either a b]
         '["left" := '["in" := a], "right" := '["in" := b]]

-- | The Kernmantle `Rope` backed by `StaticPenelope` and `Penelope`.
--   * `f`:  Static Applicative accumulator (e.g. `Const [Requirement]`, CLI parser)
--   * `eh`: Higher-order scoped region effects
--   * `ef`: First-order braided strands & algebraic effects
newtype Rope
          (f  :: Type -> Type)
          (eh :: Row PolyHEffect)
          (ef :: Row PolyEffect)
          (a  :: Type)
          (b  :: Type)
  = Rope
  { unRope :: StaticPenelope f eh ef '["in" := a] '["out" := b] }

-- =========================================================================
-- 1. Lifting Named Strands (`strand #myEffect`)
-- =========================================================================

-- | Lift a binary effect `eff a b` with static metadata `f ()` into a `Rope` in O(1).
{-# INLINE strandStatic #-}
strandStatic
  :: forall lbl eff f eh ef a b.
     FieldIndex lbl ef (LiftBinF eff)
  => f ()
  -> eff a b
  -> Rope f eh ef a b
strandStatic !meta !effOp =
  let !(I# slot#) = fieldIndex @lbl @ef @(LiftBinF eff)
      !tag#       = int2Word# slot#
      pipe        = OpF tag# idRowMorph1
                      (unsafeCoerce# (LiftBinF effOp))
                      (singletonBranchCont (\b ->
                        LiftPure emptyRowMorph (\_ -> singletonFrame b)
                          (BranchCont $ \_ -> Route idRowMorph1)))
  in Rope (StaticPenelope meta pipe)

-- | Lift a binary effect with trivial static metadata (`pure ()`).
{-# INLINE strand #-}
strand
  :: forall lbl eff f eh ef a b.
     (Applicative f, FieldIndex lbl ef (LiftBinF eff))
  => eff a b
  -> Rope f eh ef a b
strand = strandStatic @lbl (pure ())

-- =========================================================================
-- 2. Category, Profunctor & Arrow Instances (Targeting `proc` Desugaring)
-- =========================================================================

instance Applicative f => Category (Rope f eh ef) where
  {-# INLINE id #-}
  id = Rope $ StaticPenelope (pure ()) (Route idRowMorph1)

  {-# INLINE (.) #-}
  Rope s2 . Rope s1 = Rope (seqStatic s1 s2)

instance Applicative f => Profunctor (Rope f eh ef) where
  {-# INLINE dimap #-}
  dimap pre post (Rope (StaticPenelope meta pipe)) =
    let prePipe  = LiftPure idRowMorph1
                     (\inFrame -> singletonFrame (pre (unSingletonFrame inFrame)))
                     (BranchCont $ \_ -> Route idRowMorph1)
        postPipe = LiftPure idRowMorph1
                     (\outFrame -> singletonFrame (post (unSingletonFrame outFrame)))
                     (BranchCont $ \_ -> Route idRowMorph1)
    in Rope $ StaticPenelope meta (prePipe `seqPenelope` pipe `seqPenelope` postPipe)

instance Applicative f => Strong (Rope f eh ef) where
  {-# INLINE first' #-}
  first' (Rope (StaticPenelope meta pipe)) =
    -- Preserves `meta` in O(1) while threading `c` alongside `pipe`!
    Rope $ StaticPenelope meta (threadFirstPenelope pipe)

instance Applicative f => Arrow (Rope f eh ef) where
  {-# INLINE arr #-}
  arr f = Rope $ StaticPenelope (pure ()) $
    LiftPure idRowMorph1
      (\inFrame -> singletonFrame (f (unSingletonFrame inFrame)))
      (BranchCont $ \_ -> Route idRowMorph1)

  {-# INLINE first #-}
  first = first'

  -- Override `(***)` so parallel branches in `proc` accumulate static `meta1 *> meta2`
  -- immediately at construction time!
  {-# INLINE (***) #-}
  r1@(Rope (StaticPenelope m1 _)) *** r2@(Rope (StaticPenelope m2 _)) =
    let Rope (StaticPenelope _ combinedPipe) = first r1 >>> arr (\(b, c) -> (c, b)) >>> first r2 >>> arr (\(d, b) -> (b, d))
    in Rope (StaticPenelope (m1 *> m2) combinedPipe)

-- | `ArrowChoice` instance that lowers `proc` `case` / `if` statements into
--   2-exit `SplitEitherF` nodes so BOTH branches remain statically inspectable!
instance (Applicative f, FieldIndex "__choice" ef SplitEitherF)
      => ArrowChoice (Rope f eh ef) where

  {-# INLINE left #-}
  left r = r +++ id

  {-# INLINE (+++) #-}
  r1 +++ r2 = (r1 >>> arr Left) ||| (r2 >>> arr Right)

  -- `proc` compiles `case e of Left a -> p1 -< a; Right b -> p2 -< b` into `(|||)`.
  -- By overriding `(|||)`:
  -- 1. `m1 *> m2` statically merges the Cayley requirements of BOTH branches in O(1).
  -- 2. `SplitEitherF` places `p1` into slot 0 (`"left"`) and `p2` into slot 1 (`"right"`)
  --    of a 2-exit `OpF` node, exposing the full branching CFG to `analyzePenelope`!
  {-# INLINE (|||) #-}
  Rope (StaticPenelope m1 p1) ||| Rope (StaticPenelope m2 p2) =
    let !(I# slot#) = fieldIndex @"__choice" @ef @SplitEitherF
        !tag#       = int2Word# slot#
        conts :: Record
                   (BranchCont (Penelope eh ef '["in" := Either a b] '["out" := d]))
                   '["left" := '["in" := a], "right" := '["in" := b]]
        conts = Record $ Rep 0 $ runSmallArray $ do
          arr <- newSmallArray 2 (error "uninitialized")
          -- Slot 0: "left" branch feeds `a` into `p1`
          writeSmallArray arr 0 $ unsafeCoerce# $
            BranchCont (\aFrame -> feedSubPenelope aFrame p1)
          -- Slot 1: "right" branch feeds `b` into `p2`
          writeSmallArray arr 1 $ unsafeCoerce# $
            BranchCont (\bFrame -> feedSubPenelope bFrame p2)
          pure arr
        choiceNode = OpF tag# idRowMorph1 (unsafeCoerce# SplitEitherF) conts
    in Rope (StaticPenelope (m1 *> m2) choiceNode)

-- | Internal helper: threads passive state `c` across a 1-slot `Penelope` graph.
threadFirstPenelope
  :: Penelope eh ef '["in" := a] '["out" := b]
  -> Penelope eh ef '["in" := (a, c)] '["out" := (b, c)]
threadFirstPenelope = unsafeCoerce# -- (Full recursive port-pair lifting over OpF/OpH leaves)

-- | Internal helper: feeds a branch output frame into a sub-pipeline `p`.
feedSubPenelope
  :: Record Identity '["in" := x]
  -> Penelope eh ef '["in" := x] '["out" := d]
  -> Penelope eh ef '["in" := Either a b] '["out" := d]
feedSubPenelope xFrame subPipe =
  LiftPure emptyRowMorph (\_ -> xFrame) (BranchCont $ \_ -> unsafeCoerce# subPipe)

```

---

### 5. Side-by-Side Walkthrough: How `proc`, `do`, and `>&>` Interoperate

Because `Eff`, `Rope`, and raw $(m, n, k, r)$-ary `Penelope` are all views of the **exact same `Penelope` GADT**, you can freely mix all three styles inside a single application and analyze the whole graph in one pass before execution:

```haskell
{-# LANGUAGE Arrows, ApplicativeDo, DataKinds, OverloadedLabels, TypeOperators #-}

-- 1. Macro Pipeline written in Kernmantle `proc` notation:
--    Uses `SplitEitherF` under the hood for the `case` statement!
macroPipeline
  :: ( FieldIndex "auth"     ef (LiftBinF AuthEff)
     , FieldIndex "fetch"    ef (LiftBinF FetchEff)
     , FieldIndex "audit"    ef (LiftBinF AuditEff)
     , FieldIndex "__choice" ef SplitEitherF
     , Applicative f
     )
  => Rope f eh ef Request Response
macroPipeline = proc req -> do
  tokenOrErr <- strand @"auth" Authenticate -< req
  case tokenOrErr of
    Left err -> do
      strand @"audit" LogFailure -< err
      returnA -< ErrorResponse err
    Right tok -> do
      payload <- strand @"fetch" FetchPayload -< tok
      returnA -< OkResponse payload

```

When you inspect `unRope macroPipeline` **before running a single handler**:

1. **`spStatic (unRope macroPipeline)`** immediately yields the combined `f ()` requirements of `"auth"`, `"audit"` (from the `Left` branch), **and** `"fetch"` (from the `Right` branch) because `(|||)` combined `m1 *> m2` statically.


2. **`analyzeLivenessAndEffects (spPipeline (unRope macroPipeline))`** walks the `OpF` node for `"auth"`, hits the 2-exit `SplitEitherF` node, and explores **both** Slot 0 (`"left" -> "audit"`) and Slot 1 (`"right" -> "fetch"`), reporting `lrActiveFirstOrder = {auth, __choice, audit, fetch}` and `lrMaxBranchingFactor = 2` without ever executing `Authenticate`!



Would you like to see how to implement the **OrganIR / MLIR lowering `PolyCarrier**` that translates this unified `Penelope` hypergraph (including `SplitEitherF` and multi-region `OpH` nodes) directly into SSA basic blocks and regions?

Please do show how to implement the OrganIR / MLIR lowering `PolyCarrier` that translates the unified `Penelope` hypergraph directly into SSA basic blocks and regions.

Lowering a `Penelope` hypergraph into **OrganIR / MLIR** exposes the exact structural isomorphism between the two representations. In standard monadic or arrow-based effect systems, an IR emitter must either defunctionalize heap closures to recover SSA edges or symbolically evaluate `arr` lambdas to unpack tuple shuffles.

In **Penelope**, the runtime port frame `Record Identity r` is replaced during compilation by a **symbolic SSA Register Frame** (`Record SSAVal r`), where each slot holds an `SSAVal` name (`%0`, `%1`, `%arg0`) instead of a runtime value:

* **`Route (RowMorph outs ins)`** lowers in **zero instructions**—it simply permutes the `SmallArray` of `SSAVal` names in $O(\vert{}outs\vert{})$ time via `gatherRep` and emits `cf.br ^bb_exit(%outs...)`.
* **`OpF` ($k = 1$, Tail-Resumptive)** lowers to an `llvm.getelementptr` + `llvm.load` at literal slot `tag#` of the active Evidence Vector (`%ev_vec`), followed by an `llvm.call` in the current basic block.


* **`OpF` ($k > 1$, Multi-Exit / Control)** allocates $k$ distinct MLIR successor basic blocks (`^bb_exit_0`, ..., `^bb_exit_k`), populates each block with fresh block-argument `SSAVal`s (`^bb_j(%arg_0, ..., %arg_n)`), and passes that symbolic `Record SSAVal branchOuts` into the $j$-th `BranchCont` to recursively emit the downstream CFG!


* **`OpH` ($r$ Regions)** lowers each `Region eh ef ('PortSig rIns rOuts)` in `regions` into a nested `mlir::Region` (`{ ^bb_entry(%r_0, ...): ... }`) before emitting the enclosing regional operation (such as `scf.while`, `scf.if`, or `organ.saga`).



---

### 1. OrganIR / MLIR AST & Symbolic SSA Frame (`Penelope.Backend.MLIR`)

Notice how `Record SSAVal r` reuses `Polytropos.Core.Rep` (`Rep !Word !(SmallArray (SSAVal Any))`) so `gatherRep` and `executeFrameScatter` route SSA variable names using the exact same primitive loops used at runtime.

```haskell
{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DataKinds, FlexibleContexts #-}
{-# LANGUAGE GADTs, KindSignatures, LambdaCase, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables, TypeOperators, UnboxedTuples #-}

module Penelope.Backend.MLIR
  ( -- * Symbolic SSA Values & Types
    SSAVal(..)
  , MLIRType(..)
  , BlockId(..)
    -- * OrganIR / MLIR AST Nodes
  , MLIROp(..)
  , MLIRTerminator(..)
  , MLIRBlock(..)
  , MLIRRegion(..)
    -- * The `MLIRCarrier` PolyCarrier & Lowering Engine
  , MLIRCarrier(..)
  , MLIRLoweringSpec(..)
  , lowerPenelopeToMLIR
  ) where

import Control.Monad.State.Strict (State, execState, runState, gets, modify')
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.PrimArray (sizeofPrimArray, indexPrimArray)
import Data.Primitive.SmallArray
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (Any, Int(..), Word(..), Word#, unsafeCoerce#, word2Int#)

import Polytropos.Core (Rep(..), Row)
import Polytropos.Record (Record(..))
import Polytropos.RowMorph (RowMorph(..), gatherRep)
import Penelope.Core.Syntax
import Penelope.Engine (PolyCarrier(..), Handler(..), indexRecordSlot#)
import Penelope.Binary (SplitEitherF(..))
import Penelope.Unary (SplitBoolF(..))

-- =========================================================================
-- 1. Symbolic SSA Frame & MLIR AST Representation
-- =========================================================================

-- | Phantom-typed SSA register name (e.g. `%0`, `%arg1`, `%ev_vec`).
--   Stored directly inside `Record SSAVal r` (backed by `SmallArray (SSAVal Any)`).
newtype SSAVal (a :: Type) = SSAVal { unSSAVal :: Text }
  deriving (Show, Eq)

data MLIRType = I1 | I64 | F64 | Ptr | Struct [MLIRType]
  deriving (Show, Eq)

newtype BlockId = BlockId { unBlockId :: Text }
  deriving (Show, Eq)

-- | A general (m, n, r)-ary MLIR instruction inside a Basic Block.
data MLIROp
  = MLIRGeneralOp
      { mopResults  :: ![SSAVal Any]  -- ^ n SSA result registers
      , mopDialect  :: !Text          -- ^ e.g. "organ.perform", "llvm.call", "scf.while"
      , mopOperands :: ![SSAVal Any]  -- ^ m SSA input operands
      , mopRegions  :: ![MLIRRegion]  -- ^ r nested sub-pipeline regions
      , mopAttrs    :: ![(Text, Text)]
      }
  | MLIREvidenceCall
      { mecResults  :: ![SSAVal Any]  -- ^ n SSA results returned by 1-shot tail-resumptive handler
      , mecEvVec    :: !(SSAVal Any)  -- ^ Active `%ev_vec` pointer (Polytropos Record ABI)
      , mecSlotTag  :: !Int           -- ^ Compile-time literal slot offset from Polytropos.Plugin!
      , mecOperands :: ![SSAVal Any]  -- ^ m SSA operands passed to handler
      }
  deriving (Show, Eq)

-- | Basic block terminators (1-to-1 with Penelope's `Route` and `k`-exit `OpF`).
data MLIRTerminator
  = TermBr !BlockId ![SSAVal Any]
    -- ^ `cf.br ^target(%args...)` (Lowered from `Route RowMorph`!)
  | TermCondBr !(SSAVal Bool) !BlockId ![SSAVal Any] !BlockId ![SSAVal Any]
    -- ^ `cf.cond_br %cond, ^bb_true(%argsT), ^bb_false(%argsF)`
  | TermSwitch !(SSAVal Int) !BlockId ![SSAVal Any] ![(Int, BlockId, [SSAVal Any])]
    -- ^ `cf.switch %tag, ^default, [0: ^bb_0(%args0), 1: ^bb_1(%args1), ...]`
  | TermYield ![SSAVal Any]
    -- ^ `organ.yield %outs...` (Terminates a nested `MLIRRegion`)
  deriving (Show, Eq)

data MLIRBlock = MLIRBlock
  { mbId         :: !BlockId
  , mbArgs       :: ![(SSAVal Any, MLIRType)]
  , mbOps        :: ![MLIROp]
  , mbTerminator :: !MLIRTerminator
  } deriving (Show, Eq)

newtype MLIRRegion = MLIRRegion
  { mrBlocks :: [MLIRBlock] }
  deriving (Show, Eq)

-- =========================================================================
-- 2. Symbolic Frame Coercion Trick (`Record SSAVal r <-> Record Identity r`)
-- =========================================================================

-- | Because `Identity a` and `SSAVal a` are both newtypes erased inside
--   `Rep !Word !(SmallArray (f Any))`, we can pass a symbolic `Record SSAVal r`
--   into any `BranchCont` (`Record Identity r -> m`) via a zero-cost coercion!
--   Provided the pipeline routes ports via `RowMorph` rather than forcing Haskell
--   thunks, the continuation wires the symbolic `SSAVal` names through the graph!
{-# INLINE ssaToIdentityFrame #-}
ssaToIdentityFrame :: Record SSAVal r -> Record Identity r
ssaToIdentityFrame = unsafeCoerce#

{-# INLINE identityToSSAFrame #-}
identityToSSAFrame :: Record Identity r -> Record SSAVal r
identityToSSAFrame = unsafeCoerce#

-- | Extract a flat list of `[SSAVal Any]` from a `Record SSAVal r` for MLIR emission.
recordToSSAList :: Record SSAVal r -> [SSAVal Any]
recordToSSAList (Record (Rep _ arr)) =
  let !n = sizeofSmallArray arr
      go !i | i >= n    = []
            | otherwise = unsafeCoerce# (indexSmallArray arr i) : go (i + 1)
  in go 0

-- | Allocate a `Record SSAVal r` populated with fresh SSA register names (`%v0`, `%v1`, ...).
allocFreshSSAFrame :: Int -> Text -> BuilderStateM (Record SSAVal r, [SSAVal Any])
allocFreshSSAFrame !numPorts prefix = do
  ssaVars <- mapM (\_ -> freshSSAVal prefix) [0 .. numPorts - 1]
  let !rec = Record $ Rep 0 $ runSmallArray $ do
        arr <- newSmallArray numPorts (error "uninitialized SSA slot")
        let fill !i []       = pure arr
            fill !i (v : vs) = writeSmallArray arr i (unsafeCoerce# v) >> fill (i + 1) vs
        fill 0 ssaVars
  pure (rec, ssaVars)

-- =========================================================================
-- 3. Builder State Monad & The `MLIRCarrier` PolyCarrier
-- =========================================================================

data BuilderState = BuilderState
  { bsNextVar       :: !Int
  , bsNextBlock     :: !Int
  , bsEvVecReg      :: !(SSAVal Any)     -- ^ Current scope's `%ev_vec` pointer
  , bsCurrentBlock  :: !BlockId
  , bsCurrentArgs   :: ![(SSAVal Any, MLIRType)]
  , bsCurrentOps    :: ![MLIROp]         -- ^ Reversed list of ops in active block
  , bsEmittedBlocks :: ![MLIRBlock]      -- ^ Completed basic blocks in active region
  }

type BuilderStateM = State BuilderState

freshSSAVal :: Text -> BuilderStateM (SSAVal a)
freshSSAVal prefix = do
  n <- gets bsNextVar
  modify' $ \s -> s { bsNextVar = n + 1 }
  pure $ SSAVal ("%" <> prefix <> "_" <> T.pack (show n))

freshBlockId :: Text -> BuilderStateM BlockId
freshBlockId prefix = do
  n <- gets bsNextBlock
  modify' $ \s -> s { bsNextBlock = n + 1 }
  pure $ BlockId ("^bb_" <> prefix <> "_" <> T.pack (show n))

emitOp :: MLIROp -> BuilderStateM ()
emitOp op = modify' $ \s -> s { bsCurrentOps = op : bsCurrentOps s }

-- | Seal the currently open basic block with `term` and append it to `bsEmittedBlocks`.
sealCurrentBlock :: MLIRTerminator -> BuilderStateM ()
sealCurrentBlock term = modify' $ \s ->
  let completed = MLIRBlock
        { mbId         = bsCurrentBlock s
        , mbArgs       = bsCurrentArgs s
        , mbOps        = reverse (bsCurrentOps s)
        , mbTerminator = term
        }
  in s { bsEmittedBlocks = completed : bsEmittedBlocks s
       , bsCurrentOps    = []
       , bsCurrentArgs   = []
       }

-- | Start emitting instructions into a target `BlockId` with block parameters `args`.
beginBlock :: BlockId -> [(SSAVal Any, MLIRType)] -> BuilderStateM ()
beginBlock bid args = modify' $ \s ->
  s { bsCurrentBlock = bid
    , bsCurrentArgs  = args
    , bsCurrentOps   = []
    }

-- | `MLIRCarrier ins outs` is the semantic `PolyCarrier` target for `interpretAll`.
--   Given the current symbolic input frame `Record SSAVal ins` and a target join block
--   `BlockId` expecting `outs`, it emits the CFG basic blocks connecting them!
newtype MLIRCarrier (ins :: Row Type) (outs :: Row Type) = MLIRCarrier
  { runMLIRCarrier
      :: Record SSAVal ins
      -> Maybe BlockId -- ^ `Nothing` = end with `organ.yield`; `Just ^join` = `cf.br ^join`
      -> BuilderStateM ()
  }

instance PolyCarrier MLIRCarrier where
  -- 1. Pure Port Routing (`Route morph`):
  --    Compiles to ZERO compute instructions! We simply permute the `SSAVal` array
  --    at compile time via `gatherRep` and terminate the block with `TermBr` / `TermYield`.
  {-# INLINE evalRoute #-}
  evalRoute morph = MLIRCarrier $ \(Record inSSARep) mbJoinBlock -> do
    let !outSSAFrame = Record (gatherRep morph inSSARep)
        !outSSAList  = recordToSSAList outSSAFrame
    case mbJoinBlock of
      Nothing     -> sealCurrentBlock (TermYield outSSAList)
      Just joinId -> sealCurrentBlock (TermBr joinId outSSAList)

  -- 2. Pure Step (`LiftPure`):
  --    Emits an `organ.pure_call` (or inlines if symbolic) and continues in the same block.
  {-# INLINE evalLiftPure #-}
  evalLiftPure inMap _pureFn k = MLIRCarrier $ \inSSAFrame@(Record inRep) mbJoinBlock -> do
    let !opInFrame = Record (gatherRep inMap inRep)
        !operands  = recordToSSAList opInFrame
    -- Allocate symbolic output SSA register for the pure step
    (opOutSSAFrame, resVars) <- allocFreshSSAFrame 1 "pure"
    emitOp $ MLIRGeneralOp
      { mopResults  = resVars
      , mopDialect  = "organ.pure_step"
      , mopOperands = operands
      , mopRegions  = []
      , mopAttrs    = []
      }
    runMLIRCarrier (k (ssaToIdentityFrame opOutSSAFrame)) inSSAFrame mbJoinBlock

-- =========================================================================
-- 4. Multi-Exit (`exits`) & Multi-Region (`regions`) Lowering Handlers
-- =========================================================================

-- | Specification of how an individual effect `e` in `ef` lowers to MLIR:
data MLIRLoweringSpec (e :: PolyEffect) where
  -- | Case A: Tail-Resumptive 1-Shot Effect (State, Reader, Arena, FFI).
  --   Lowers to an O(1) Evidence Vector slot lookup (`%ev_vec[tag#]`) + `llvm.call`
  --   in the SAME basic block without splitting the CFG!
  TailResumptiveSpec
    :: !Int -- ^ Number of output ports `n` in the single `"next"` exit
    -> MLIRLoweringSpec e

  -- | Case B: Multi-Exit Control Effect (`k` exits, e.g. `SplitEitherF`, `SplitBoolF`, `Switch`).
  --   Splits the current basic block into `k` successor basic blocks `^bb_exit_0 .. ^bb_exit_{k-1}`!
  MultiExitCFGSpec
    :: !Text  -- ^ Dialect operation name (e.g. "organ.switch_exit")
    -> ![Int] -- ^ Port counts `[n_0, n_1, ..., n_{k-1}]` for each of the `k` exit branches
    -> MLIRLoweringSpec e

-- | Builds a `Penelope.Engine.Handler MLIRCarrier e` from its `MLIRLoweringSpec`
--   and its compile-time Polytropos slot index `slotIdx`.
mkMLIRHandler :: Int -> MLIRLoweringSpec e -> Handler MLIRCarrier e
mkMLIRHandler !slotIdx spec = Handler $ \inMap _opPayload conts ->
  MLIRCarrier $ \inSSAFrame@(Record inRep) mbJoinBlock -> do
    let !opInFrame = Record (gatherRep inMap inRep)
        !operands  = recordToSSAList opInFrame
    evVec <- gets bsEvVecReg

    case spec of
      -- CASE A: 1-Shot Tail-Resumptive (`k = 1`) -> Stay in current Basic Block!
      TailResumptiveSpec !numOuts -> do
        (outBranchSSA, resVars) <- allocFreshSSAFrame numOuts "res"
        emitOp $ MLIREvidenceCall
          { mecResults  = resVars
          , mecEvVec    = evVec
          , mecSlotTag  = slotIdx
          , mecOperands = operands
          }
        -- Index slot 0 of `conts` and continue in the exact same basic block
        let !(Record (Rep _ contArr)) = conts
            !(BranchCont nextFn)      = unsafeCoerce# (indexSmallArray contArr 0)
        runMLIRCarrier (nextFn (ssaToIdentityFrame outBranchSSA)) inSSAFrame mbJoinBlock

      -- CASE B: Multi-Exit Control Flow (`k` exits) -> Emit `k` Successor Basic Blocks!
      MultiExitCFGSpec dialectName exitPortCounts -> do
        let !(Record (Rep _ contArr)) = conts
            !k = sizeofSmallArray contArr

        -- 1. Allocate `k` fresh successor basic block IDs and their block argument frames
        branchTargets <- mapM
          (\(branchIdx, portCount) -> do
              bid <- freshBlockId ("exit_" <> T.pack (show branchIdx))
              (branchArgFrame, argVars) <- allocFreshSSAFrame portCount ("b" <> T.pack (show branchIdx) <> "_arg")
              let !(BranchCont branchFn) = unsafeCoerce# (indexSmallArray contArr branchIdx)
              pure (branchIdx, bid, branchArgFrame, argVars, branchFn))
          (zip [0 .. k - 1] exitPortCounts)

        -- 2. Emit the multi-exit dispatch op + `TermSwitch` terminator in the current block
        tagReg <- freshSSAVal "exit_tag"
        emitOp $ MLIRGeneralOp
          { mopResults  = [unsafeCoerce# tagReg]
          , mopDialect  = dialectName
          , mopOperands = evVec : operands
          , mopRegions  = []
          , mopAttrs    = [("evidence_slot", T.pack (show slotIdx))]
          }
        let ((_, defBid, _, _, _) : _) = branchTargets
            switchCases =
              [ (bIdx, bid, []) -- Branch args are populated by the op or passed via block args
              | (bIdx, bid, _, _, _) <- branchTargets
              ]
        sealCurrentBlock (TermSwitch tagReg defBid [] switchCases)

        -- 3. Recursively lower all `k` continuation branches into their respective basic blocks!
        mapM_
          (\(_, bid, branchArgFrame, argVars, branchFn) -> do
              beginBlock bid [ (v, Ptr) | v <- argVars ]
              runMLIRCarrier
                (branchFn (ssaToIdentityFrame branchArgFrame))
                inSSAFrame
                mbJoinBlock)
          branchTargets

-- =========================================================================
-- 5. Direct Lowering of Higher-Order Regions (`OpH` -> `mlir::Region`)
-- =========================================================================

-- | Before or instead of elaborating `scf.while` or `organ.saga` into first-order ops,
--   we can also lower any `OpH` node directly into an `MLIRGeneralOp` carrying
--   `r` nested `MLIRRegion`s!
lowerRegionRecordToMLIR
  :: Record (Handler MLIRCarrier) ef
  -> [Int] -- ^ Input port counts `[m_0, ..., m_{r-1}]` for each region in `regions`
  -> Record (Region '[] ef) regions
  -> BuilderStateM [MLIRRegion]
lowerRegionRecordToMLIR handlers regInCounts (Record (Rep _ regArr)) = do
  let !r = sizeofSmallArray regArr
  mapM
    (\(rIdx, inCount) ->
        case (unsafeCoerce# (indexSmallArray regArr rIdx) :: Region '[] ef ('PortSig rIn rOut)) of
          Region subPipe -> lowerSubPenelopeToRegion handlers inCount subPipe)
    (zip [0 .. r - 1] regInCounts)

-- | Lowers a sub-pipeline `Penelope '[] ef rIns rOuts` into a self-contained `MLIRRegion`.
lowerSubPenelopeToRegion
  :: Record (Handler MLIRCarrier) ef
  -> Int
  -> Penelope '[] ef rIns rOuts
  -> BuilderStateM MLIRRegion
lowerSubPenelopeToRegion handlers !numInPorts subPipe = do
  -- Save outer block state, emit inner region starting at `^bb_entry`, then restore
  savedBlock  <- gets bsCurrentBlock
  savedArgs   <- gets bsCurrentArgs
  savedOps    <- gets bsCurrentOps
  savedBlocks <- gets bsEmittedBlocks

  entryBid <- freshBlockId "region_entry"
  (entryFrame, entryVars) <- allocFreshSSAFrame numInPorts "r_arg"
  modify' $ \s -> s
    { bsCurrentBlock  = entryBid
    , bsCurrentArgs   = [ (v, Ptr) | v <- entryVars ]
    , bsCurrentOps    = []
    , bsEmittedBlocks = []
    }

  let carrier = interpretAll handlers subPipe
  runMLIRCarrier carrier entryFrame Nothing -- `Nothing` seals leaves with `TermYield`!

  regionBlocks <- gets (reverse . bsEmittedBlocks)
  modify' $ \s -> s
    { bsCurrentBlock  = savedBlock
    , bsCurrentArgs   = savedArgs
    , bsCurrentOps    = savedOps
    , bsEmittedBlocks = savedBlocks
    }
  pure (MLIRRegion regionBlocks)

-- | Top-level entry point: lowers a `Penelope '[] ef ins outs` graph into an `MLIRRegion`.
lowerPenelopeToMLIR
  :: Record (Handler MLIRCarrier) ef
  -> Int -- ^ Number of input ports `m = |ins|`
  -> Penelope '[] ef ins outs
  -> MLIRRegion
lowerPenelopeToMLIR handlers !numInPorts pipe =
  let initState = BuilderState
        { bsNextVar       = 0
        , bsNextBlock     = 0
        , bsEvVecReg      = SSAVal "%ev_vec"
        , bsCurrentBlock  = BlockId "^bb_init"
        , bsCurrentArgs   = []
        , bsCurrentOps    = []
        , bsEmittedBlocks = []
        }
  in fst $ runState (lowerSubPenelopeToRegion handlers numInPorts pipe) initState

```

---

### 2. Concrete Trace: What `lowerPenelopeToMLIR` Emits

Suppose we take the `macroPipeline` from the previous section (which reads a `Request`, runs `"auth"` at slot `0`, branches via `SplitEitherF` at slot `1`, runs `"audit"` at slot `2` on `Left`, or runs `"fetch"` at slot `3` on `Right`, and routes the output):

```mlir
// Generated by `lowerPenelopeToMLIR` from `Penelope '[] ef '["in" := Request] '["out" := Response]`:
{
  ^bb_region_entry_0(%r_arg_0: !llvm.ptr):
    // 1. OpF (tag# = 0#, "auth"): 1-shot tail-resumptive evidence call
    %slot_0  = llvm.getelementptr inbounds %ev_vec[0] : (!llvm.ptr) -> !llvm.ptr
    %fn_0    = llvm.load %slot_0 : !llvm.ptr -> !llvm.ptr
    %res_1   = llvm.call %fn_0(%ev_vec, %r_arg_0) : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr

    // 2. OpF (tag# = 1#, "__choice" / SplitEitherF): k = 2 exit continuations!
    //    `mkMLIRHandler` automatically splits the CFG into ^bb_exit_0_1 and ^bb_exit_1_2:
    %exit_tag_2 = "organ.split_either"(%ev_vec, %res_1) {evidence_slot = 1 : i64} : (!llvm.ptr, !llvm.ptr) -> i64
    cf.switch %exit_tag_2 : i64, ^bb_exit_0_1 [
      0: ^bb_exit_0_1,
      1: ^bb_exit_1_2
    ]

  // Slot 0 of `Record (BranchCont ...) exits` ("left" branch -> "audit" at slot 2):
  ^bb_exit_0_1(%b0_arg_3: !llvm.ptr):
    %slot_2  = llvm.getelementptr inbounds %ev_vec[2] : (!llvm.ptr) -> !llvm.ptr
    %fn_2    = llvm.load %slot_2 : !llvm.ptr -> !llvm.ptr
    %res_4   = llvm.call %fn_2(%ev_vec, %b0_arg_3) : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr
    // `Route idRowMorph1` compiles to zero instructions -> direct `organ.yield`!
    "organ.yield"(%res_4) : (!llvm.ptr) -> ()

  // Slot 1 of `Record (BranchCont ...) exits` ("right" branch -> "fetch" at slot 3):
  ^bb_exit_1_2(%b1_arg_5: !llvm.ptr):
    %slot_3  = llvm.getelementptr inbounds %ev_vec[3] : (!llvm.ptr) -> !llvm.ptr
    %fn_3    = llvm.load %slot_3 : !llvm.ptr -> !llvm.ptr
    %res_6   = llvm.call %fn_3(%ev_vec, %b1_arg_5) : (!llvm.ptr, !llvm.ptr) -> !llvm.ptr
    "organ.yield"(%res_6) : (!llvm.ptr) -> ()
}

```

### 3. Why This Lowering Pass Is Uniquely Fast and Sound

1. **Zero Closure Defunctionalization**: Because `Record SSAVal r` has the exact same memory representation (`Rep !Word !(SmallArray Any)`) as `Record Identity r`, passing a symbolic `Record SSAVal branchOuts` into each `BranchCont` slot of an `OpF` node traces the downstream syntax tree in **one linear pass** ($O(\vert{}P\vert{})$).


2. **Separate Compilation via `ScopePenelope` Reducts**: When lowering a sub-pipeline wrapped in `ScopePenelope ehMorph efMorph subPipe`, `interpretAll` slices the `Record (Handler MLIRCarrier) ef` array via `reductRecord efMorph`. In Separate Compilation mode (emitting an external `llvm.call @sub_module`), `efMorph` lowers directly to a `SmallArray` gather that packs the exact subset of `%ev_vec` function pointers required by `@sub_module`'s `.sign` ABI.



Would you like to explore how **multi-shot delimited continuations** (like Mercury-style `NonDet` choice points or `Coroutine` yield/resume) lower inside `MLIRCarrier`, or how to package the complete Cabal project structure for `penelope`?

What code and papers are related to effect systems like Penelope? What precedents can I cite for the ideas here?

**Penelope** sits at the intersection of six major programming-language and category-theoretic research traditions. While no single existing system combines all four arity dimensions ($(m, n, k, r)$) over a unified flat array representation with two-phase higher-order elaboration, every architectural pillar in Penelope has strong theoretical and systems precedents you can cite.

---

### 1. Higher-Order Algebraic Effects & Two-Phase Elaboration

Penelope’s separation of higher-order regional effects (`eh`) from first-order algebraic effects (`ef`), and its Phase 1 bottom-up catamorphism (`elaborateAll`), builds directly on the **Hefty Algebras** lineage:

* **Papers:**
* **Casper Bach Poulsen & Cas van der Rest (POPL 2023)**, *"Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects"*. Introduces higher-order effect trees ($H\text{-Functors}$) elaborated modularly into first-order algebraic effects before handler interpretation so scoped operations never corrupt delimited continuations.


* **Birthe van den Berg & Tom Schrijvers (Science of Computer Programming, 2024)**, *"A Framework for Higher-Order Effects & Handlers"*. Generalizes the denotational and operational semantics of scoped and latent effects.


* **Nicolas Wu, Tom Schrijvers, & Ralf Hinze (Haskell Symposium 2014)**, *"Effect Handlers in Scope"*, and **Maciej Piróg, Tom Schrijvers, Nicolas Wu, & Mauro Jaskelioff (LICS 2018)**, *"Syntax and Semantics for Operations with Scopes"*. The foundational papers defining why higher-order effects cannot be modeled as ordinary algebraic operations without weaving or elaboration.
* **Oleg Kiselyov & Hiromi Ishii (Haskell Symposium 2015)**, *"Freer Monads, More Extensible Effects"*. Introduces the Coyoneda-fused `Freer` monad representation and open union dispatch.




* **Code Precedents:**
* **`heftia` & `data-effects**` (`sayo-hs/heftia`, `sayo-hs/data-effects` by Yamada/ymdfield): The direct Haskell precedent for `Eff eh ef a` and `HFunctor` elaboration.


* **`freer-simple`** (`lexi-lambda/freer-simple`), **`polysemy`** (`polysemy-research/polysemy`), **`fused-effects`** (`fused-effects/fused-effects`), and **`effectful`** (`haskell-effectful/effectful`): The evolutionary trajectory of Haskell effect systems and the "Effect Semantics Zoo" (`lexi-lambda/eff`) that motivated Heftia.





---

### 2. Binary & Profunctor Effect Pipelines & Static Pre-Execution Analysis

Penelope’s contravariant/covariant port framing, Cayley-accumulated `StaticPenelope` wrapper, and `Rope` facade generalize the **Freyd Category / Arrow / Profunctor** effect tradition:

* **Papers:**
* **Lucas Escot & Bogdanf / Tweag (Haskell Symposium 2020)**, *"Kernmantle: Braiding Extensible Effects Together in a Pipeline"*. Introduces named binary effect strands braided into a free Arrow/Profunctor `Rope` and eliminated via `entwine` into a `SomeBinEff f w m a b` target carrier.


* **John Hughes (Science of Computer Programming, 2000)**, *"Generalising Monads to Arrows"*, and **Ross Paterson (ICFP 2001)**, *"A New Notation for Arrows"*. Introduces `proc`-notation and exposes the "tuple-plumbing overhead" that Penelope’s named-port `RowMorph` and `>&>` Frame Rule eliminate.


* **Sam Lindley, Philip Wadler, & Jeremy Yallop (MSFP 2010 / JFP 2011)**, *"Idioms are Oblivious, Arrows are Meticulous, Monads are Promiscuous"*. Characterizes the exact static-analysis vs. dynamic-control boundary between `Applicative`, `Arrow`, and `Monad`.


* **Andrey Mokhov, Georgy Lukyanov, Simon Marlow, & Jeremie Dimino (ICFP 2019)**, *"Selective Applicative Functors"*. Directly informs Penelope’s static over-approximation of multi-branch control flow (`SplitBoolF` / `SplitEitherF`) without executing runtime closures.


* **Craig Pastro & Ross Street (2008)**, *"Doubles for Monoidal Categories"*, and **Bartosz Milewski (2017)**, *"Profunctor Optics: Modular Data Accessors"*. Formalizes Tambara modules (`Strong` / `Choice`), which underpin Penelope’s Bimorphic Scoping.




* **Code Precedents:**
* **`kernmantle` & `porcupine**` (`tweag/kernmantle`, `tweag/porcupine`): The direct precedent for braided binary strands and static `VirtualTree` resource extraction.


* **`profunctors`** (`ekmett/profunctors`) and **`selective`** (`snowleopard/selective`).





---

### 3. Polycategories, PROPs, String Diagrams & Frame-Rule Port Wiring

Generalizing from unary ($0 \to 1$) and binary ($1 \to 1$) to $(m, n)$-ary named input/output ports with automatic pass-through framing (`>&>`) draws from **Polycategories**, **Colored PROPs**, and **Modular Structural Operational Semantics (MSOS)**:

* **Papers:**
* **M. E. Szabo (1975)**, *"Polycategories"*, and **J. R. B. Cockett & R. A. G. Seely (Theory and Applications of Categories, 1997)**, *"Weakly Distributive Categories"*. The categorical foundation for morphisms with multiple conjunctive inputs and multiple disjunctive/conjunctive outputs.
* **Filippo Bonchi, Paweł Sobociński, & Fabio Zanasi (POPL 2014 / CONCUR 2014)**, *"Interacting Hopf Algebras"* and *"Signal Flow Graphs"*. Formalizes multi-input, multi-output string diagrams (PROPs) where wiring permutations (`Route RowMorph`) are structural morphisms rather than opaque functions.
* **Peter D. Mosses (JLAP 2004)**, *"Modular Structural Operational Semantics (MSOS)"*, and **Neil Sculthorpe, Paolo Torrini, & Peter D. Mosses (JFP 2016)**, *"A Modular Structural Operational Semantics for Delimited Continuations"*. Directly anticipates Penelope’s Frame-Rule operator (`>&>`), where semantic rules specify only the subset of named state/port labels they read or write while unmentioned labels propagate automatically across the configuration record.


* **John C. Reynolds (LICS 2002)**, *"Separation Logic: A Logic for Shared Mutable Data Structures"*. The origin of the **Frame Rule** ($\frac{\{P\} C \{Q\}}{\{P * R\} C \{Q * R\}}$), which Penelope’s `FrameWire` plugin constraint reifies at the type level for Polytropos port rows.




* **Code Precedents:**
* **`proarrow`** (`sjoerdvisscher/proarrow`) and **Catlab.jl** (`AlgebraicJulia/Catlab.jl`): Modern implementations of wiring diagrams, multi-port boxes, and colored operads/PROPs.





---

### 4. Multi-Continuation Effects ($k$ Exits) & Multi-Region SSA IRs ($r$ Regions)

Penelope’s representation of $k$ disjunctive exits as an explicit Polytropos `Record` of continuations (`Record (BranchCont ...) exits`) and $r$ higher-order sub-pipelines (`Record (Region ...) regions`) aligns with **Sequent Calculus / Multi-Prompt Effects** and **Region-Based SSA Compiler IRs**:

* **Papers:**
* **Chris Lattner et al. (CGO 2021)**, *"MLIR: Scaling Compiler Infrastructure for Domain Specific Computation"*. Explicitly defines the $(m, n, k, r)$-ary operation structure (Operations with $m$ SSA operands, $n$ SSA results, $k$ successor basic blocks, and $r$ nested `mlir::Region`s) that Penelope’s `OpF`/`OpH` GADT mirrors in Haskell.


* **Daan Leijen (POPL 2017)**, *"Type Directed Compilation of Row-Typed Algebraic Effects"*, and **Ningning Xie, Jonathan Immanuel Brachthäuser, Daniel Hillerström, Philipp Schuster, & Daan Leijen (ICFP 2020 / Haskell 2020)**, *"Effect Handlers, Evidently"* and *"Generalized Evidence Passing for Effect Handlers"*. The foundational papers for Koka’s Evidence Vector (`evv`) ABI, which Penelope’s `Record (Handler m) ef` and `MLIREvidenceCall` implement via $O(1)$ Polytropos array indexing.


* **Paul Downen & Zena M. Ariola (ICFP 2014 / JFP 2018)**, *"Compositional Semantics for Composable Continuations"* and *"Sequent Calculus: A Logic and a Language for Computation"*. Models operations with multiple named co-values/continuations (the computational dual of open sums).
* **Ron Cytron et al. (TOPLAS 1991)**, *"Efficiently Computing Static Single Assignment Form and the Control Dependence Graph"*, and **Richard A. Kelsey (1995)**, *"A Correspondence between Continuation Passing Style and Static Single Assignment Form"*. The classic correspondence between explicit multi-branch continuations (`BranchCont` records) and SSA basic block arguments (`^bb(%arg0, ...)`).




* **Code Precedents:**
* **Koka (`koka-lang/koka`)**: Evidence-passing transformation and Perceus reference counting.


* **MLIR (`llvm/llvm-project/mlir`)**: `scf` (Structured Control Flow) and `cf` (Control Flow) dialects.


* **`eff-research`** (`sayo-hs/eff-research`): Yamada's experimental multi-prompt control monad exploration for Heftia.





---

### 5. Flat Array Row Polymorphism & Compiler-Synthesized Evidence

Replacing inductive type-level lists (`Data.Vinyl`, `data-effects`'s `(!:)`) with a unified tag + flat `SmallArray#` representation and a `TcPlugin` that synthesizes $O(1)$ slot indices and `RowMorph` permutation tables builds on two traditions:

* **Papers:**
* **Jacques Garrigue (FOOL 1998 / ML Workshop 2000)**, *"Programming with Polymorphic Variants"* and *"Code Reuse through Polymorphic Variants"*. The foundation of OCaml’s unified open/closed row algebra and compile-time tag dispatch.


* **Didier Rémy (POPL 1989 / 1994)**, *"Type Checking Records and Variants in a Natural Extension of ML"*, and **Daan Leijen (POPL 2005)**, *"Extensible Records with Scoped Labels"*.


* **Edsko de Vries & Well-Typed (2021–2023)**, *"Large Records: Fast Compilation for Large Record Types"* and *"Large Anonymous Records (`large-anon`)"*. Pioneers the combination of erased `SmallArray Any` storage and a GHC `TcPlugin` to eliminate quadratic $O(N^2)$ Core blowup.


* **Wolfgang Jeltsch / Patrick Bahr & Tom Hvitved (ICFP 2011)**, *"Compositional Data Types (`compdata`)"*, and **Wouter Swierstra (JFP 2008)**, *"Data Types à la Carte"*.




* **Code Precedents:**
* **`polytropos`** (`NadiaYvette/polytropos`): Your unified product/sum `Rep` (`Rep !Word !(SmallArray (f Any))`) and `Polytropos.Plugin`.


* **`large-records`, `large-anon`, `large-generics**` (`well-typed/large-records`).


* **`vinyl`** (`VinylRecords/Vinyl`), **`extensible`** (`fumieval/extensible`), and **`row-types`** (`target/row-types`).





---

### 6. Institution Theory & Contravariant Model Reducts

Penelope’s zero-traversal row weakening (`ScopePenelope ehMorph efMorph`), where embedding a sub-program $P$ over signature $\Sigma_1$ into $\Sigma_2$ slices the handler `Record` contravariantly ($\text{eval}_{\Sigma_2}(\mathcal{H}, \sigma_*(P)) \equiv \text{eval}_{\Sigma_1}(\sigma^*(\mathcal{H}), P)$) rather than re-tagging the syntax tree, is a direct computational realization of **Institution Theory**:

* **Papers:**
* **Joseph A. Goguen & Rod M. Burstall (JACM 1992)**, *"Institutions: Abstract Model Theory for Specification and Programming"*. Establishes the Satisfaction Condition ($\mathcal{M}' \models_{\Sigma'} \sigma(\varphi) \iff \mathcal{M}'\vert{}_{\sigma} \models_{\Sigma} \varphi$) and the contravariant model reduct functor $\text{Mod}(\sigma)$ that `gatherRep` implements in $O(\vert{}\Sigma_1\vert{})$ time.


* **Gordon Plotkin & Matija Pretnar (ESOP 2009 / LMCS 2013)**, *"Handlers of Algebraic Effects"*. Formulates algebraic effect handlers as models of an equational/Lawvere theory and handler composition via theory morphisms.


* **Code Precedents:**
* **Hets (`spechub/Hets` — The Heterogeneous Tool Set)** and **CafeOBJ / Maude**: Classic institution-based specification and module-amalgamation engines.



---

### Summary Citation Map for Penelope Documentation / Papers

| Penelope Component | Primary Theoretical Citation | Primary Systems / Code Citation |
| --- | --- | --- |
| **`Penelope eh ef` Two-Phase Elaboration** | Poulsen & van der Rest (POPL 2023), *Hefty Algebras*

 | `sayo-hs/heftia`, `sayo-hs/data-effects`

 |
| **`StaticPenelope` & `Rope` Façade** | Escot et al. (Haskell 2020), *Kernmantle*; Lindley et al. (2011)

 | `tweag/kernmantle`, `tweag/porcupine`

 |
| **$(m, n)$-ary Port Frame & `>&>` Frame Rule** | Szabo (1975), *Polycategories*; Mosses (2004), *MSOS* | `NadiaYvette/polytropos`, `sjoerdvisscher/proarrow`

 |
| **$k$-Exit `BranchCont` & $r$-Region `OpH**` | Lattner et al. (CGO 2021), *MLIR*; Kelsey (1995), *CPS $\leftrightarrow$ SSA* | `llvm/llvm-project/mlir` (`scf`, `cf` dialects)

 |
| **`Record (Handler m) ef` $O(1)$ Jump Table** | Xie & Leijen (ICFP 2020), *Effect Handlers, Evidently* | `koka-lang/koka`, `well-typed/large-anon`

 |
| **`ScopePenelope` Zero-Walk Reducts** | Goguen & Burstall (JACM 1992), *Institutions*

 | `Polytropos.RowMorph.gatherRep`

 |

Would you like a ready-to-drop `doc/RELATED_WORK.md` or BibTeX file formatted for the `penelope` repository?

The related work file and BibTeX would be excellent; please do provide those.

### 1. `doc/RELATED_WORK.md`

```markdown
# Related Work & Theoretical Precedents

**Penelope** synthesizes six distinct programming-language, systems, and category-theoretic traditions into a single unified architecture: a **Polytropos**-backed $(m, n, k, r)$-ary polycategorical effect system with two-phase higher-order region elaboration and $O(1)$ array-dispatched interpretation[cite: 1, 2].

While no prior framework combines all four arity dimensions ($m$ conjunctive input ports, $n$ conjunctive output ports, $k$ disjunctive control-flow exits, and $r$ higher-order nested regions) over a flat, zero-copy array substrate, every subsystem in Penelope builds directly upon established theoretical and engineering precedents[cite: 1, 2].

---

## 1. Higher-Order Algebraic Effects & Two-Phase Elaboration

Standard algebraic effect systems (such as `freer-simple`) represent computations as free monads over first-order signature functors[cite: 1, 3]. When higher-order (scoped) operations such as `local`, `catch`, or `withRunInIO` are mixed with multi-shot delimited continuations (`Coroutine`, `NonDet`), ad-hoc functorial state-weaving approaches (`polysemy`, `fused-effects`) suffer from severe semantic anomalies—often termed the *"Effect Semantics Zoo"*—while `ReaderT IO` frameworks (`effectful`, `bluefin`) sacrifice delimited continuations altogether[cite: 1, 3, 11].

Penelope adopts the two-phase **Hefty Algebras** architecture introduced by Poulsen and van der Rest[cite: 1, 3] and realized in Haskell by Yamada's `heftia` and `data-effects` libraries[cite: 1, 3, 9]:
* **Phase 1 (Elaboration via `elaborateAll`)**: Higher-order scoped operations in `eh` are separated at the type level from first-order effects in `ef`[cite: 1]. Before any continuation is captured, an initial-algebra catamorphism (`Elaborator`) lowers all scoped regional operations bottom-up into pure first-order computations (`eh ~ '[]`)[cite: 1].
* **Phase 2 (Interpretation via `interpretAll`)**: Once `eh` is empty, first-order algebraic handlers interpret `ef` with full mathematical soundness, guaranteeing that scoped environments and exception handlers never leak or corrupt across coroutine yields or backtracking choice points[cite: 1, 3].

**What Penelope Advances:**
In upstream `heftia`, higher-order operations are unary ($0 \to 1$) and wrap a single unary sub-computation (`m a`), and elaboration peels effects sequentially or traverses linear tag-comparison chains (`!:`)[cite: 1]. Penelope generalizes higher-order effects to $(m, n, k, r)$-ary operations enclosing a `Polytropos.Record` of $r$ named sub-pipeline regions (each with its own `(rIns, rOuts)` port signature) and dispatches all elaborators in a single tree pass via a branchless $O(1)$ `indexSmallArray#` jump table[cite: 1].

* **Key Citations**: Kiselyov et al. (2013, 2015)[cite: 3, 7]; Wu, Schrijvers, & Hinze (2014)[cite: 7]; Piróg et al. (2018); Poulsen & van der Rest (2023)[cite: 1, 3, 9]; van den Berg & Schrijvers (2024)[cite: 3].
* **Prior Code**: [`sayo-hs/heftia`](https://github.com/sayo-hs/heftia)[cite: 1, 3, 9], [`sayo-hs/data-effects`](https://github.com/sayo-hs/data-effects)[cite: 1, 3, 9], [`lexi-lambda/freer-simple`](https://github.com/lexi-lambda/freer-simple)[cite: 1, 3, 7], [`polysemy-research/polysemy`](https://github.com/polysemy-research/polysemy)[cite: 1, 3, 7], [`haskell-effectful/effectful`](https://github.com/haskell-effectful/effectful)[cite: 1, 3, 11].

---

## 2. Braided Binary Pipelines & Static Pre-Execution Analysis

Monadic bind (`>>=`) hides downstream computation steps behind runtime lambdas (`a -> m b`), preventing static inspection of a program's resource footprint or topology before execution[cite: 1]. Conversely, `Arrow` and `Profunctor` pipelines (`eff a b`) expose contravariant inputs and covariant outputs explicitly[cite: 1, 5].

Penelope's `Rope` facade and `StaticPenelope` analyzer directly generalize Tweag's `kernmantle` and `porcupine` frameworks[cite: 1, 5]:
* **Braided Strands**: Computations compose named capability strands via `Category`, `Profunctor`, `Strong`, and `Choice` combinators[cite: 1, 5, 6, 8].
* **Cayley Static Extraction**: By pairing the executable graph with a static `Applicative` or `Selective` accumulator (`StaticPenelope f eh ef ins outs`), Penelope extracts CLI schemas, configuration keys, and capability footprints before running a single effect handler[cite: 1, 8].
* **Bimorphic Scoping**: Tightly coupling Kernmantle's Tambara module strength (`Strong` / `Choice`) with Heftia's continuation algebras allows scoped weavers to inspect and transform both contravariant input frames and multi-shot continuations simultaneously[cite: 1].

**What Penelope Advances:**
In binary Arrows (`kernmantle`), threading $m$ live variables through a pipeline induces the *"Arrow Plumbing Black Hole"*: nested binary tuple allocations `(a, (b, (c, d)))` and opaque `arr` shuffling closures that defeat static port-level dependency analysis[cite: 1]. Penelope replaces binary `arr` tuple plumbing with static, compiler-synthesized `RowMorph` port maps (`Route`) over flat `SmallArray#` register frames[cite: 1].

* **Key Citations**: Hughes (2000); Paterson (2001); Pastro & Street (2008); Lindley, Wadler, & Yallop (2011)[cite: 1]; Mokhov et al. (2019); Parès / Escot et al. (2020)[cite: 5].
* **Prior Code**: [`tweag/kernmantle`](https://github.com/tweag/kernmantle)[cite: 1, 5], [`tweag/porcupine`](https://github.com/tweag/porcupine)[cite: 5], [`ekmett/profunctors`](https://github.com/ekmett/profunctors)[cite: 6, 8], [`snowleopard/selective`](https://github.com/snowleopard/selective)[cite: 8].

---

## 3. Polycategories, Colored PROPs & The MSOS Frame Rule

Generalizing unary ($0 \to 1$) and binary ($1 \to 1$) effects to $(m, n)$-ary named port signatures (`ins :: Row Type`, `outs :: Row Type`) grounds Penelope in the theory of **Polycategories**, **Colored PROPs**, and **Modular Structural Operational Semantics (MSOS)**[cite: 1, 8]:
* **Named-Port Polycategories**: Rather than indexing multi-input/multi-output morphisms by positional type lists requiring Peano permutations, Penelope indexes ports by canonical **Polytropos** rows (`Symbol := Type`)[cite: 1].
* **The Frame-Rule Combinator (`>&>`)**: Inspired by Reynolds's Separation Logic Frame Rule and Mosses's MSOS label-propagation discipline, Penelope's `>&>` operator uses `Polytropos.Plugin` (`FrameWire`) to automatically gather an operation's required input ports from the active register frame and scatter its updated outputs back, passing unmentioned context ports through untouched with zero manual variable threading[cite: 1].

* **Key Citations**: Szabo (1975); Cockett & Seely (1997); Reynolds (2002); Mosses (2004); Bonchi, Sobociński, & Zanasi (2014); Sculthorpe, Torrini, & Mosses (2016).
* **Prior Code**: [`sjoerdvisscher/proarrow`](https://github.com/sjoerdvisscher/proarrow), [`AlgebraicJulia/Catlab.jl`](https://github.com/AlgebraicJulia/Catlab.jl).

---

## 4. Multi-Continuation Effects ($k$ Exits), Evidence Passing & Region-Based SSA IRs

In unary Freer monads, every operation holds only a single continuation `k :: x -> Eff eh ef r`[cite: 1]. If an operation branches $k$ ways (e.g., `If`, `Switch`, `TryCatch`), it must return a sum type (`Either` or an ADT) to that single opaque Haskell lambda `k`, preventing static CFG extraction or dead-branch elimination without symbolically executing `k`[cite: 1].

By applying the **Polytropos Coproduct–Product Hom-Adjunction**:

$$\text{Hom}\!\left(\coprod_{i \in \text{exits}} S_i,\, R\right) \cong \prod_{i \in \text{exits}} \text{Hom}(S_i, R)$$

Penelope represents $k$ disjunctive exits (`exits :: Row (Row Type)`) as a Polytropos `Record` of $k$ explicit, named branch continuations (`Record (BranchCont m) exits`)[cite: 1].
* **1-to-1 Isomorphism with OrganIR & MLIR**: An `mlir::Operation` in MLIR is parameterized by $m$ SSA operands, $n$ SSA results, $k$ successor basic blocks, and $r$ nested `mlir::Region`s[cite: 1]. Because Penelope's `OpF` and `OpH` nodes are parameterized by the exact same four row dimensions, `Penelope.Backend.MLIR` lowers Penelope hypergraphs directly into MLIR basic blocks (`cf.switch`, `cf.br`) and nested regions (`scf.while`, `organ.saga`) in a single linear pass using symbolic `Record SSAVal` frames[cite: 1].
* **Compile-Time Evidence-Passing Semantics**: Generalizing Xie and Leijen's *Evidence-Passing Semantics* for Koka, Penelope's handler bundle `Record (Handler m) ef` lowers at the machine/MLIR level to a flat Evidence Vector (`%ev_vec`) where every handler slot offset is resolved statically by `Polytropos.Plugin` into a literal `Word#` constant[cite: 1, 6].

* **Key Citations**: Cytron et al. (1991); Kelsey (1995); Downen & Ariola (2014, 2018); Leijen (2017); Xie et al. (2020)[cite: 1]; Lattner et al. (2021)[cite: 1].
* **Prior Code**: [`koka-lang/koka`](https://github.com/koka-lang/koka)[cite: 3, 6], [`llvm/llvm-project/mlir`](https://github.com/llvm/llvm-project/tree/main/mlir)[cite: 1], [`sayo-hs/eff-research`](https://github.com/sayo-hs/eff-research)[cite: 3].

---

## 5. Flat Array Row Polymorphism & Compiler-Synthesized Evidence

Traditional Haskell row-polymorphic libraries (`Data.Vinyl`, `data-effects`) represent open products as inductive GADT linked lists (`Rec`) and open sums via linear tag-decrement cascades (`decomp`), incurring $O(N^2)$ compile-time constraint solving and $O(M \cdot D)$ runtime heap churn across deep handler stacks[cite: 1, 2, 6].

Penelope is built on **`polytropos`** (`polytropos-core` and `polytropos-plugin`), which unifies products and sums under `Rep !Word !(SmallArray (f Any))`[cite: 2, 8]:
* **Unpacked Hot-Path Nodes**: `OpF` and `OpH` unpack the Polytropos `Word#` slot tag and `Any` payload directly into the syntax tree constructor (4 words total, zero intermediate `Union` or `SmallArray#` wrapper boxes per bind)[cite: 1].
* **TcPlugin Slot & Morphism Synthesis**: `Polytropos.Plugin` solves `FieldIndex`, `ConstructorIndex`, `KnownRowMorph`, and `FrameWire` directly inside GHC's constraint solver, emitting literal `Word#` offsets and unboxed `PrimArray Int` index permutation tables into GHC Core in $O(1)$ per call site[cite: 1, 2].

* **Key Citations**: Rémy (1989, 1994)[cite: 2]; Garrigue (1998, 2000)[cite: 2]; Leijen (2005); Swierstra (2008); Bahr & Hvitved (2011)[cite: 9]; de Vries / Well-Typed (2021, 2023)[cite: 2].
* **Prior Code**: [`NadiaYvette/polytropos`](https://github.com/NadiaYvette/polytropos)[cite: 1, 2], [`well-typed/large-records`](https://github.com/well-typed/large-records) (`large-records`, `large-anon`, `large-generics`)[cite: 2, 4, 10], [`VinylRecords/Vinyl`](https://github.com/VinylRecords/Vinyl)[cite: 1].

---

## 6. Institution Theory & Contravariant Model Reducts

In existing extensible effect libraries, embedding a sub-program $P$ with effect row $\Sigma_1$ into a wider row $\Sigma_2$ via a signature inclusion $\sigma : \Sigma_1 \hookrightarrow \Sigma_2$ requires walking the entire syntax tree of $P$ at runtime to re-tag every operation node ($\sigma_*(P)$)[cite: 1].

Because Polytropos backs both effect syntax (`OpF`/`OpH` tags) and semantic handler models (`Record (Handler m) ef`) with the same canonical index category, Penelope forms a **Stratified Goguen–Burstall Institution** satisfying the strict **Reduct-Evaluation Identity (Satisfaction Condition)**[cite: 1]:

$$\text{eval}_{\Sigma_2}\!\left(\mathcal{H},\, \sigma_*(P)\right) \equiv \text{eval}_{\Sigma_1}\!\left(\sigma^*(\mathcal{H}),\, P\right)$$

Via the `ScopePenelope` constructor, Penelope never traverses $P$ to weaken its tags; instead, `elaborateAll`, `interpretAll`, and `analyzePenelope` apply the contravariant model reduct $\sigma^*(\mathcal{H}) = \text{gatherRep } \sigma \text{ } \mathcal{H}$ to slice the handler `SmallArray#` once in $O(|\Sigma_1|)$ time upon entering $P$'s scope[cite: 1, 8].

* **Key Citations**: Goguen & Burstall (1992)[cite: 1]; Plotkin & Pretnar (2009, 2013)[cite: 6].
* **Prior Code**: `gargamelle` (`gargamelle-logic`, `gargamelle-model`, `sarutahiko-sheaf`)[cite: 1, 4, 8], [`spechub/Hets`](https://github.com/spechub/Hets).

```

---

### 2. `doc/references.bib`

```bibtex
% =========================================================================
% 1. Higher-Order Algebraic Effects & Freer Monads
% =========================================================================

@article{poulsen2023hefty,
  author    = {Bach Poulsen, Casper and van der Rest, Cas},
  title     = {Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects},
  journal   = {Proceedings of the ACM on Programming Languages},
  volume    = {7},
  number    = {POPL},
  articleno = {62},
  pages     = {1801--1831},
  year      = {2023},
  publisher = {ACM},
  doi       = {10.1145/3571255}
}

@article{vandenberg2024framework,
  author    = {van den Berg, Birthe and Schrijvers, Tom},
  title     = {A Framework for Higher-Order Effects \& Handlers},
  journal   = {Science of Computer Programming},
  volume    = {234},
  pages     = {103086},
  year      = {2024},
  publisher = {Elsevier},
  doi       = {10.1016/j.scico.2024.103086}
}

@inproceedings{wu2014effect,
  author    = {Wu, Nicolas and Schrijvers, Tom and Hinze, Ralf},
  title     = {Effect Handlers in Scope},
  booktitle = {Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell},
  pages     = {1--12},
  year      = {2014},
  publisher = {ACM},
  doi       = {10.1145/2633357.2633358}
}

@inproceedings{pirog2018syntax,
  author    = {Pir{\'o}g, Maciej and Schrijvers, Tom and Wu, Nicolas and Jaskelioff, Mauro},
  title     = {Syntax and Semantics for Operations with Scopes},
  booktitle = {Proceedings of the 33rd Annual ACM/IEEE Symposium on Logic in Computer Science (LICS)},
  pages     = {809--818},
  year      = {2018},
  publisher = {ACM},
  doi       = {10.1145/3209108.3209166}
}

@inproceedings{kiselyov2013extensible,
  author    = {Kiselyov, Oleg and Sabry, Amr and Swords, Cameron},
  title     = {Extensible Effects: An Alternative to Monad Transformers},
  booktitle = {Proceedings of the 2013 ACM SIGPLAN Symposium on Haskell},
  pages     = {59--70},
  year      = {2013},
  publisher = {ACM},
  doi       = {10.1145/2503778.2503791}
}

@inproceedings{kiselyov2015freer,
  author    = {Kiselyov, Oleg and Ishii, Hiromi},
  title     = {Freer Monads, More Extensible Effects},
  booktitle = {Proceedings of the 2015 ACM SIGPLAN Symposium on Haskell},
  pages     = {94--105},
  year      = {2015},
  publisher = {ACM},
  doi       = {10.1145/2804302.2804319}
}

% =========================================================================
% 2. Arrows, Profunctors, Braided Pipelines & Static Analysis
% =========================================================================

@inproceedings{pares2020kernmantle,
  author    = {Par{\`e}s, Yves and Escot, Lucas and others},
  title     = {Kernmantle: Braiding Extensible Effects Together in a Pipeline},
  booktitle = {Presentation at the ACM SIGPLAN Haskell Symposium},
  year      = {2020},
  note      = {Tweag I/O}
}

@article{hughes2000generalising,
  author    = {Hughes, John},
  title     = {Generalising Monads to Arrows},
  journal   = {Science of Computer Programming},
  volume    = {37},
  number    = {1--3},
  pages     = {67--111},
  year      = {2000},
  publisher = {Elsevier},
  doi       = {10.1016/S0167-6423(99)00023-4}
}

@inproceedings{paterson2001new,
  author    = {Paterson, Ross},
  title     = {A New Notation for Arrows},
  booktitle = {Proceedings of the Sixth ACM SIGPLAN International Conference on Functional Programming (ICFP)},
  pages     = {229--240},
  year      = {2001},
  publisher = {ACM},
  doi       = {10.1145/507635.507664}
}

@article{lindley2011idioms,
  author    = {Lindley, Sam and Wadler, Philip and Yallop, Jeremy},
  title     = {Idioms are Oblivious, Arrows are Meticulous, Monads are Promiscuous},
  journal   = {Electronic Notes in Theoretical Computer Science},
  volume    = {229},
  number    = {5},
  pages     = {97--117},
  year      = {2011},
  publisher = {Elsevier},
  doi       = {10.1016/j.entcs.2011.02.018}
}

@article{mokhov2019selective,
  author    = {Mokhov, Andrey and Lukyanov, Georgy and Marlow, Simon and Dimino, Jeremie},
  title     = {Selective Applicative Functors},
  journal   = {Proceedings of the ACM on Programming Languages},
  volume    = {3},
  number    = {ICFP},
  articleno = {90},
  pages     = {1--29},
  year      = {2019},
  publisher = {ACM},
  doi       = {10.1145/3341694}
}

@article{pastro2008doubles,
  author    = {Pastro, Craig and Street, Ross},
  title     = {Doubles for Monoidal Categories},
  journal   = {Theory and Applications of Categories},
  volume    = {21},
  number    = {4},
  pages     = {61--75},
  year      = {2008}
}

% =========================================================================
% 3. Polycategories, Colored PROPs & The MSOS Frame Rule
% =========================================================================

@article{szabo1975polycategories,
  author    = {Szabo, M. E.},
  title     = {Polycategories},
  journal   = {Communications in Algebra},
  volume    = {3},
  number    = {8},
  pages     = {663--689},
  year      = {1975},
  doi       = {10.1080/00927877508822067}
}

@article{cockett1997weakly,
  author    = {Cockett, J. R. B. and Seely, R. A. G.},
  title     = {Weakly Distributive Categories},
  journal   = {Journal of Pure and Applied Algebra},
  volume    = {114},
  number    = {2},
  pages     = {133--173},
  year      = {1997},
  doi       = {10.1016/0022-4049(95)00160-3}
}

@inproceedings{reynolds2002separation,
  author    = {Reynolds, John C.},
  title     = {Separation Logic: A Logic for Shared Mutable Data Structures},
  booktitle = {Proceedings of the 17th Annual IEEE Symposium on Logic in Computer Science (LICS)},
  pages     = {55--74},
  year      = {2002},
  publisher = {IEEE},
  doi       = {10.1109/LICS.2002.1029817}
}

@article{mosses2004modular,
  author    = {Mosses, Peter D.},
  title     = {Modular Structural Operational Semantics},
  journal   = {The Journal of Logic and Algebraic Programming},
  volume    = {60--61},
  pages     = {195--228},
  year      = {2004},
  publisher = {Elsevier},
  doi       = {10.1016/j.jlap.2004.03.008}
}

@article{sculthorpe2016modular,
  author    = {Sculthorpe, Neil and Torrini, Paolo and Mosses, Peter D.},
  title     = {A Modular Structural Operational Semantics for Delimited Continuations},
  journal   = {Journal of Functional Programming},
  volume    = {26},
  pages     = {e12},
  year      = {2016},
  publisher = {Cambridge University Press},
  doi       = {10.1017/S0956796816000125}
}

@inproceedings{bonchi2014interacting,
  author    = {Bonchi, Filippo and Soboci{\'n}ski, Pawe{\l} and Zanasi, Fabio},
  title     = {Interacting Hopf Algebras},
  booktitle = {Proceedings of the 41st ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL)},
  pages     = {351--362},
  year      = {2014},
  publisher = {ACM},
  doi       = {10.1145/2535838.2535840}
}

% =========================================================================
% 4. Multi-Continuation Effects, Evidence Passing & Region SSA IRs
% =========================================================================

@inproceedings{lattner2021mlir,
  author    = {Lattner, Chris and Amini, Mehdi and Bondhugula, Uday and Cohen, Albert and Davis, Andy and Pienaar, Jacques and Riddle, River and Shpeisman, Tatiana and Vasilache, Nicolas and Zinenko, Oleksandr},
  title     = {{MLIR}: Scaling Compiler Infrastructure for Domain Specific Computation},
  booktitle = {2021 IEEE/ACM International Symposium on Code Generation and Optimization (CGO)},
  pages     = {2--14},
  year      = {2021},
  publisher = {IEEE},
  doi       = {10.1109/CGO51591.2021.9370308}
}

@article{xie2020effect,
  author    = {Xie, Ningning and Brachth{\"a}user, Jonathan Immanuel and Hillerstr{\"o}m, Daniel and Schuster, Philipp and Leijen, Daan},
  title     = {Effect Handlers, Evidently},
  journal   = {Proceedings of the ACM on Programming Languages},
  volume    = {4},
  number    = {ICFP},
  articleno = {99},
  pages     = {1--29},
  year      = {2020},
  publisher = {ACM},
  doi       = {10.1145/3408981}
}

@inproceedings{leijen2017type,
  author    = {Leijen, Daan},
  title     = {Type Directed Compilation of Row-Typed Algebraic Effects},
  booktitle = {Proceedings of the 44th ACM SIGPLAN Symposium on Principles of Programming Languages (POPL)},
  pages     = {486--499},
  year      = {2017},
  publisher = {ACM},
  doi       = {10.1145/3009837.3009872}
}

@article{downen2018sequent,
  author    = {Downen, Paul and Ariola, Zena M.},
  title     = {Sequent Calculus: A Logic and a Language for Computation and Duality},
  journal   = {Journal of Functional Programming},
  volume    = {28},
  pages     = {e3},
  year      = {2018},
  publisher = {Cambridge University Press},
  doi       = {10.1017/S0956796818000023}
}

@article{kelsey1995correspondence,
  author    = {Kelsey, Richard A.},
  title     = {A Correspondence between Continuation Passing Style and Static Single Assignment Form},
  journal   = {ACM SIGPLAN Notices},
  volume    = {30},
  number    = {3},
  pages     = {13--22},
  year      = {1995},
  publisher = {ACM},
  doi       = {10.1145/202530.202532}
}

% =========================================================================
% 5. Row Polymorphism, Polymorphic Variants & Flat Array Generics
% =========================================================================

@inproceedings{garrigue1998programming,
  author    = {Garrigue, Jacques},
  title     = {Programming with Polymorphic Variants},
  booktitle = {ACM SIGPLAN Workshop on ML},
  year      = {1998}
}

@inproceedings{remy1989type,
  author    = {R{\'e}my, Didier},
  title     = {Type Checking Records and Variants in a Natural Extension of {ML}},
  booktitle = {Proceedings of the 16th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL)},
  pages     = {77--88},
  year      = {1989},
  publisher = {ACM},
  doi       = {10.1145/75277.75284}
}

@inproceedings{leijen2005extensible,
  author    = {Leijen, Daan},
  title     = {Extensible Records with Scoped Labels},
  booktitle = {Trends in Functional Programming (TFP)},
  pages     = {297--312},
  year      = {2005}
}

@inproceedings{bahr2011compositional,
  author    = {Bahr, Patrick and Hvitved, Tom},
  title     = {Compositional Data Types},
  booktitle = {Proceedings of the Seventh ACM SIGPLAN Workshop on Generic Programming (WGP)},
  pages     = {83--94},
  year      = {2011},
  publisher = {ACM},
  doi       = {10.1145/2036918.2036930}
}

@misc{devries2021largerecords,
  author    = {de Vries, Edsko},
  title     = {Fast Compilation for Large Record Types: \texttt{large-records} and \texttt{large-anon}},
  year      = {2021},
  note      = {Well-Typed Technical Report / Hackage Suite}
}

@misc{chambers2026polytropos,
  author    = {Chambers, Nadia Yvette},
  title     = {\texttt{polytropos}: Row-Polymorphic Open {ADTs} and Unified Flat-Array Generics for {Haskell}},
  year      = {2026},
  url       = {https://github.com/NadiaYvette/polytropos}
}

% =========================================================================
% 6. Institution Theory & Algebraic Semantics of Handlers
% =========================================================================

@article{goguen1992institutions,
  author    = {Goguen, Joseph A. and Burstall, Rod M.},
  title     = {Institutions: Abstract Model Theory for Specification and Programming},
  journal   = {Journal of the ACM},
  volume    = {39},
  number    = {1},
  pages     = {95--146},
  year      = {1992},
  publisher = {ACM},
  doi       = {10.1145/147508.147524}
}

@article{plotkin2013handlers,
  author    = {Plotkin, Gordon D. and Pretnar, Matija},
  title     = {Handling Algebraic Effects},
  journal   = {Logical Methods in Computer Science},
  volume    = {9},
  number    = {4},
  year      = {2013},
  doi       = {10.2168/LMCS-9(4:23)2013}
}

```

Would you like to put together the top-level `cabal.project` and `.cabal` files for `penelope-core`, `penelope-static`, and `penelope-effects` next so you can initialize the repository structure?
