{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Penelope.Core.Wiring
Description : Polycategorical wiring combinators, sequential composition, frame-rule auto-wiring, and row weakening
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides polycategorical composition primitives for the Penelope effect system.
See Note [Core Architecture] [ref:note-core-architecture],
Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation],
Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing],
Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring],
and Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].

{- Note [Polycategorical Wiring and Categorical Frame Rules]
Polycategories (Szabo, 1975) generalize monoidal categories by replacing single-input,
single-output arrows with multi-input, multi-output computational nodes:

@
            ins_1 ... ins_m
               │        │
            ┌──┴────────┴──┐
            │   Penelope   │
            └──┬────────┬──┘
               │        │
            outs_1 ... outs_n  (and disjunctive exits: exit_1 ... exit_k)
@

In this architecture, traditional composition operations take new, principled forms:

1. **Sequential Composition ('seqPenelope' / '(>>>)'):**
   Composing @stage1 >>> stage2@ substitutes @stage2@ into the output continuation leaves
   of @stage1@. If @stage1@ begins with a pure wire routing stage ('Route'), it fuses directly
   via 'preComposeRoute' in $O(1)$ without traversing intermediate continuation trees.

2. **Categorical Frame Rules ('(>&>)' / 'liftFrame'):**
   In separation logic and linear logic, the frame rule allows a local computation to be
   lifted into an ambient heap context:
   @
       {P} C {Q}
     -------------
     {P * R} C {Q * R}
   @
   In Penelope, 'FramePlan' solves this structural framing problem over open Polytropos rows:
   - 'fpGatherIn': Gathers only the local input ports required by the stage from the wider frame.
   - 'executeFrameScatter': Re-scatters the computed outputs back into the ambient frame,
     passing untouched context registers through without dynamic copying.

3. **Disjunctive Fanout ('(>?<)'):**
   When an operation terminates via one of $k$ distinct variant outcomes (such as error,
   eof, or alternate match), '(>?<)' binds an open record of branch continuations in $O(1)$,
   eliminating linear if-then-else ladders and nested coproduct allocations.

4. **Zero-Traversal Row Weakening ('weakenRow'):**
   Embedding a local computation into a wider effect signature (@efSub -> efFull@) is
   accomplished via 'ScopeRow' in $O(1)$ time, preserving institutional satisfaction
   without rewriting AST nodes.
-}

{- Note [Categorical Frame Wire Synthesis and Coyoneda Leaf Substitution]
When composing computational circuits with multiple input and output registers,
managing wire continuity across sub-stages presents two major hazards:

1. **The Wiring Explosion Problem:**
   If stage 1 produces output registers @'["out1" ':= A, "out2" ':= B]@ and an ambient
   frame contains @'["ambient" ':= C]@, passing @C@ around stage 1 manually requires
   wiring boilerplate like @first@ or @second@ in standard arrows.
   Penelope's 'FramePlan' automates this:
   - At compile-time, the type-level relationship between @frameIn@, @opIn@, @opOut@,
     and @frameOut@ is proven by the constraint 'FrameWire'.
   - The solved 'FramePlan' contains two unboxed index vectors: 'fpGatherIn' (which
     projects the sub-record of inputs required by the stage) and 'fpBaseScatter'
     (which specifies where untouched ambient slots are mapped in the output frame).
   - At runtime, 'executeFrameScatter' constructs the final output record in a single
     unboxed pass, copying ambient registers into their new slot positions while
     interleaving newly computed results.

2. **Coyoneda Leaf Continuation Fusion:**
   When sequentially composing @seqPenelope stage1 stage2@, @stage1@ may terminate in
   multiple branches (such as an 'OpF' or 'OpH' with $k$ exit continuations).
   Instead of wrapping each continuation in a thunk or allocating intermediate
   closures, 'rmapFused' applies leaf substitution across all continuation slots
   in a single contiguous 'SmallArray' traversal.
   This guarantees that composition cost scales strictly with the immediate branch
   arity $k$, rather than the total depth of the hypergraph.
-}
-}
module Penelope.Core.Wiring
  ( -- * Sequential Composition
    seqPenelope
  , (>>>)

    -- * Frame-Rule Automatic Port Wiring
  , (>&>)
  , liftFrame

    -- * Disjunctive Multi-Exit Fanout
  , (>?<)

    -- * Zero-Traversal Row Weakening
  , weakenRow
  , weakenRowExplicit
  , weakenEF
  , weakenEH

    -- * Route Pre-Composition & Peephole Fusion
  , preComposeRoute

    -- * Coyoneda Leaf Continuation Record Mapping
  , rmapFused
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Primitive.SmallArray
  ( indexSmallArray
  , newSmallArray
  , runSmallArray
  , sizeofSmallArray
  , writeSmallArray
  )
import GHC.Exts (Int (..), int2Word#, unsafeCoerce#, word2Int#)
import Penelope.Core.Route (fuseRoute, isIdentityRowMorph)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , Region (..)
  )
import Polytropos (Record, RowOf)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph
  ( FramePlan (..)
  , FrameWire (..)
  , KnownRowMorph (..)
  , RowMorph (..)
  , composeRowMorph
  , executeFrameScatter
  , gatherRep
  , indexRowMorph
  , unsafeMkRowMorph
  )
import Unsafe.Coerce (unsafeCoerce)

{- |
Map a continuation or region transformation across all slots of an open record in a
single flat SmallArray pass without intermediate container allocations.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
rmapFused :: (forall x. a x -> b x) -> Record r1 -> Record r2
rmapFused f (RowRep (Rep tag arr)) =
  let !sz = sizeofSmallArray arr
      !arr' = runSmallArray $ do
        m <- newSmallArray sz (Identity (unsafeCoerce# ()))
        let loop !i
              | i < sz = do
                  let Identity !anyVal = indexSmallArray arr i
                  let !res = f (unsafeCoerce anyVal)
                  writeSmallArray m i (Identity (unsafeCoerce res))
                  loop (i + 1)
              | otherwise = pure ()
        loop 0
        pure m
  in RowRep (Rep tag arr')

{- |
Pre-compose a static 't:RowMorph' wire routing stage before a downstream 'Penelope' pipeline.
Peephole-fuses into 'fuseRoute' if the downstream stage is also a 'Route', elides identity
morphisms in O(1) time, or distributes into continuation gather maps.

== Fusion Rules & Invariants
1. __Route-Route Collapse:__
   @Route m1 >>> Route m2@ fuses immediately into @Route (fuseRoute m1 m2)@ via a single
   'PrimArray' gather composition, eliminating intermediate pipeline stages.
2. __Identity Elision:__
   When 'isIdentityRowMorph' detects an identity permutation, the upstream morphism
   is elided in \(\mathcal{O}(1)\) time via 'unsafeCoerce'.
3. __Continuation Distribution:__
   When followed by 'LiftPure', 'OpF', or 'OpH', 'fuseRoute' updates the node's input
   gather map directly, preserving single-pass execution.

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
preComposeRoute ::
  forall mid ins (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf Type).
  RowMorph mid ins
  -> Penelope eh ef mid outs
  -> Penelope eh ef ins outs
-- Case 1: Route followed by Route fuses immediately into a single stage
preComposeRoute m1 (Route m2) = Route (fuseRoute m1 m2)
-- Case 2: Identity row morphism is a pure no-op; elide it entirely
preComposeRoute m1 next
  | isIdentityRowMorph m1 = unsafeCoerce next
-- Case 3: LiftPure node; compose into input gather map and distribute into continuation
preComposeRoute m1 (LiftPure inMap f cont) =
  LiftPure
    (fuseRoute m1 inMap)
    f
    (BranchCont (\res -> preComposeRoute m1 (runBranchCont cont res)))
-- Case 4: First-order algebraic operation; fuse into input map and distribute across exit branches
preComposeRoute m1 (OpF tag# inMap payload conts) =
  OpF
    tag#
    (fuseRoute m1 inMap)
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
-- Case 5: Higher-order scoped operation; fuse into input map and distribute across exit branches
preComposeRoute m1 (OpH tag# inMap regions payload conts) =
  OpH
    tag#
    (fuseRoute m1 inMap)
    regions
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
-- Case 6: Scope boundary; distribute through regional envelope
preComposeRoute m1 (ScopeRow ehM efM inner) =
  ScopeRow ehM efM (preComposeRoute m1 inner)

{- |
Sequential polycategorical composition of Penelope pipelines.
Substitutes downstream pipelines into continuation leaves with Coyoneda fusion.

== Invariants & Operational Semantics
1. __Leaf Substitution:__
   The downstream computation @next@ is substituted into every exit continuation leaf
   of the upstream pipeline.
2. __Coyoneda Record Traversal:__
   Branch continuation records are transformed via 'rmapFused', ensuring traversal
   cost scales strictly with the exit arity \(k\).
3. __Peephole Route Optimization:__
   Leading routing nodes delegate to 'preComposeRoute', guaranteeing that sequential
   wire permutations fuse into single stages without creating nested AST nodes.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
seqPenelope ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  Penelope eh ef a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
-- Route prefix: delegate to preComposeRoute for peephole fusion and identity elision
seqPenelope (Route m) next = preComposeRoute m next
-- LiftPure: substitute downstream pipeline into the single continuation leaf
seqPenelope (LiftPure inMap f (BranchCont k)) next =
  LiftPure inMap f (BranchCont (\res -> seqPenelope (k res) next))
-- OpF: substitute downstream pipeline into all k exit continuation leaves
seqPenelope (OpF tag# inMap payload conts) next =
  OpF tag# inMap payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
-- OpH: substitute downstream pipeline into all k exit continuation leaves
seqPenelope (OpH tag# inMap regions payload conts) next =
  OpH tag# inMap regions payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
-- ScopeRow: sequence through regional scope re-indexing
seqPenelope (ScopeRow ehM efM inner) next =
  seqScopeRow ehM efM inner next

-- | Infix sequential composition operator.
{-# INLINE (>>>) #-}
infixr 1 >>>
(>>>) ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  Penelope eh ef a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
(>>>) = seqPenelope

{- |
Lifts a local @(opIn -> opOut)@ computation into a wider @(frameIn -> frameOut)@
register frame by composing 'fpGatherIn' at the root and 'executeFrameScatter' at the leaves.

== Operational Mechanics & Invariants
1. __Gather Decomposition:__
   At the pipeline entrance, 'fpGatherIn' extracts exactly the unboxed sub-record of
   fields needed by the inner stage from the ambient @frameIn@ record in \(\mathcal{O}(|opIn|)\) time.
2. __Scatter Recomposition:__
   At every terminal continuation leaf, 'executeFrameScatter' interleaves the untouched
   ambient frame registers (routed via 'fpBaseScatter') with newly evaluated output fields,
   constructing the final @frameOut@ record in a single contiguous unboxed write pass.
3. __Peephole Pre-composition:__
   Intermediate routing morphisms are eagerly folded into 'composeRowMorph' during AST
   rebuilding, preventing chain growth and nested 'LiftPure' wrappers across repeated framing.

See Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring].
-}
liftFrame ::
  forall (frameIn :: RowOf Type) (opIn :: RowOf Type) (opOut :: RowOf Type) (frameOut :: RowOf Type)
         (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  FramePlan frameIn opIn opOut frameOut
  -> Penelope eh ef opIn opOut
  -> Penelope eh ef frameIn frameOut
liftFrame plan = go
  where
    !baseSz = sizeofPrimArray (unRowMorph (fpBaseScatter plan))
    !outSz = fpOutSize plan

    liftCont ::
      forall branchOuts.
      BranchCont (Penelope eh ef opIn opOut) branchOuts
      -> BranchCont (Penelope eh ef frameIn frameOut) branchOuts
    liftCont (BranchCont k) = BranchCont (\branchRec -> go (k branchRec))

    go :: Penelope eh ef opIn opOut -> Penelope eh ef frameIn frameOut
    -- Pure route node: gather from frame, apply permutation, scatter outputs back into frame
    go (Route m) =
      LiftPure
        (fpGatherIn plan)
        (\(RowRep inRep) -> RowRep (gatherRep m inRep))
        (BranchCont $ \(RowRep outRep) ->
          LiftPure
            (unsafeMkRowMorph [0 .. baseSz - 1])
            (\(RowRep frameRep) -> RowRep (executeFrameScatter plan frameRep outRep))
            (BranchCont $ \_ -> Route (unsafeMkRowMorph [0 .. outSz - 1])))
    -- LiftPure node: compose local input gather with frame gather; lift continuation
    go (LiftPure inMap f cont) =
      LiftPure (composeRowMorph inMap (fpGatherIn plan)) f (liftCont cont)
    -- OpF node: compose local input gather with frame gather; lift all exit continuations
    go (OpF tag# inMap payload conts) =
      OpF tag# (composeRowMorph inMap (fpGatherIn plan)) payload (rmapFused liftCont conts)
    -- OpH node: compose local input gather with frame gather; lift all exit continuations
    go (OpH tag# inMap regions payload conts) =
      OpH tag# (composeRowMorph inMap (fpGatherIn plan)) regions payload (rmapFused liftCont conts)
    -- ScopeRow node: lift framed inner term through regional scope boundary
    go (ScopeRow ehM efM inner) =
      ScopeRow ehM efM (liftFrame plan inner)

{- |
Frame-Rule Auto-Wiring Composition ('(>&>)'):
Automatically gathers each stage's required input ports from the live register frame
and scatters its outputs back into the frame, preserving untouched context ports.

See Note [Frame-Rule Automatic Wire Synthesis] [ref:note-frame-rule-wiring].
-}
{-# INLINE (>&>) #-}
infixr 1 >&>
(>&>) ::
  forall (frameIn :: RowOf Type) (ins1 :: RowOf Type) (outs1 :: RowOf Type)
         (frameMid :: RowOf Type) (ins2 :: RowOf Type) (outs2 :: RowOf Type)
         (frameOut :: RowOf Type)
         (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect).
  ( FrameWire frameIn ins1 outs1 frameMid
  , FrameWire frameMid ins2 outs2 frameOut
  ) =>
  Penelope eh ef ins1 outs1
  -> Penelope eh ef ins2 outs2
  -> Penelope eh ef frameIn frameOut
stage1 >&> stage2 =
  let plan1 = framePlan @frameIn @ins1 @outs1 @frameMid
      plan2 = framePlan @frameMid @ins2 @outs2 @frameOut
  in liftFrame plan1 stage1 `seqPenelope` liftFrame plan2 stage2

{- |
Disjunctive multi-exit fanout combinator.
Binds an operation's exit channels to a 'Record' of k downstream branch continuations
in O(1) time without nested binary coproducts ('Either').

== Operational Mechanics & Invariants
1. __Zero-Allocation Dispatch:__
   When an 'OpF' or 'OpH' node has empty continuations (leaf position), 'handlers' are
   bound directly into the leaf payload via 'unsafeCoerce' in \(\mathcal{O}(1)\) time.
2. __Branch Homomorphism:__
   When targeting an intermediate hypergraph with existing non-empty branch continuations,
   'rmapFused' recursively distributes the handler record into all downstream continuation leaves
   in a single linear pass over the branch array.
3. __Purity Distribution:__
   Pure transformation nodes ('LiftPure') and scope wrappers ('ScopeRow') commute with
   disjunctive fanout, pushing the handler record inward toward terminal leaves without
   allocating intermediate continuation wrapper nodes.

See Note [Core Architecture] [ref:note-core-architecture]
and Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
{-# INLINE (>?<) #-}
infixr 1 >?<
(>?<) ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect)
         (ins :: RowOf Type) (mid :: RowOf Type) (outs :: RowOf Type)
         handlers.
  Penelope eh ef ins mid
  -> Record handlers
  -> Penelope eh ef ins outs
-- OpF node: bind branch continuations directly into leaves or recursively distribute
(>?<) (OpF tag# inMap payload conts) handlers =
  let RowRep (Rep _ arr) = conts
  in if sizeofSmallArray arr == 0
       then OpF tag# inMap payload (unsafeCoerce handlers)
       else OpF tag# inMap payload
              (rmapFused (\(BranchCont k) -> BranchCont (\res -> k res >?< handlers)) conts)
-- OpH node: bind branch continuations directly into leaves or recursively distribute
(>?<) (OpH tag# inMap regions payload conts) handlers =
  let RowRep (Rep _ arr) = conts
  in if sizeofSmallArray arr == 0
       then OpH tag# inMap regions payload (unsafeCoerce handlers)
       else OpH tag# inMap regions payload
              (rmapFused (\(BranchCont k) -> BranchCont (\res -> k res >?< handlers)) conts)
-- LiftPure node: distribute fanout downstream through the continuation
(>?<) (LiftPure inMap f cont) handlers =
  LiftPure inMap f (BranchCont (\res -> runBranchCont cont res >?< handlers))
-- ScopeRow node: distribute fanout through regional scope boundary
(>?<) (ScopeRow ehM efM inner) handlers =
  ScopeRow ehM efM (inner >?< handlers)
-- Route node: coerce output representation to target row
(>?<) (Route m) _ =
  Route (unsafeCoerce m)

{- |
Embed a computation into wider higher-order and first-order effect rows using compile-time
synthesized 'KnownRowMorph' instances.
Wraps the computation in 'ScopeRow' in O(1) time with zero syntax traversal.

See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
weakenRow ::
  forall {ehSub :: RowOf PolyHEffect} (ehFull :: RowOf PolyHEffect)
         {efSub :: RowOf PolyEffect} (efFull :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  (KnownRowMorph ehSub ehFull, KnownRowMorph efSub efFull) =>
  Penelope ehSub efSub ins outs
  -> Penelope ehFull efFull ins outs
weakenRow = ScopeRow (rowMorph @ehSub @ehFull) (rowMorph @efSub @efFull)

{- |
Embed a computation into wider effect rows using explicit 't:RowMorph' mappings.

See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
weakenRowExplicit ::
  forall (ehSub :: RowOf PolyHEffect) (ehFull :: RowOf PolyHEffect)
         (efSub :: RowOf PolyEffect) (efFull :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  RowMorph ehSub ehFull
  -> RowMorph efSub efFull
  -> Penelope ehSub efSub ins outs
  -> Penelope ehFull efFull ins outs
weakenRowExplicit = ScopeRow

{- |
Weaken first-order effect row while preserving higher-order effect row.

See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
weakenEF ::
  forall {efSub :: RowOf PolyEffect} (efFull :: RowOf PolyEffect)
         {eh :: RowOf PolyHEffect}
         (ins :: RowOf Type) (outs :: RowOf Type).
  (KnownRowMorph eh eh, KnownRowMorph efSub efFull) =>
  Penelope eh efSub ins outs
  -> Penelope eh efFull ins outs
weakenEF = ScopeRow (rowMorph @eh @eh) (rowMorph @efSub @efFull)

{- |
Weaken higher-order effect row while preserving first-order effect row.

See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
weakenEH ::
  forall {ehSub :: RowOf PolyHEffect} (ehFull :: RowOf PolyHEffect)
         {ef :: RowOf PolyEffect}
         (ins :: RowOf Type) (outs :: RowOf Type).
  (KnownRowMorph ehSub ehFull, KnownRowMorph ef ef) =>
  Penelope ehSub ef ins outs
  -> Penelope ehFull ef ins outs
weakenEH = ScopeRow (rowMorph @ehSub @ehFull) (rowMorph @ef @ef)

-- | Helper to sequence through regional 'ScopeRow' boundaries to downstream continuations.
--
-- == Opcode Re-Indexing and Categorical Pushout Mechanics
-- When traversing through an explicit effect row scope boundary, opcodes must be re-indexed
-- through the embedding row morphism ('indexRowMorph') to match the parent scope's opcode numbering:
--
-- 1. __First-Order Opcode Lifting:__
--    For an algebraic operation @OpF tag# ...@, the local tag index is translated via
--    @indexRowMorph efM@. This computes the target opcode slot in \(\mathcal{O}(1)\) time
--    without inspecting or modifying the operation's dynamic payload.
-- 2. __Higher-Order Opcode Lifting:__
--    For a scoped operation @OpH tag# ...@, the local higher-order tag is translated via
--    @indexRowMorph ehM@, and all enclosed sub-regions are recursively wrapped in
--    @ScopeRow ehM efM@ to preserve lexical scope boundaries.
-- 3. __Scope Composition Fusion:__
--    When encountering an inner 'ScopeRow ehM2 efM2', the two embedding morphisms fuse
--    directly via 'composeRowMorph', ensuring that consecutive scope boundaries collapse
--    into a single re-indexing layer rather than nesting.
seqScopeRow ::
  forall (ehSub :: RowOf PolyHEffect) (eh :: RowOf PolyHEffect)
         (efSub :: RowOf PolyEffect) (ef :: RowOf PolyEffect)
         (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  RowMorph ehSub eh
  -> RowMorph efSub ef
  -> Penelope ehSub efSub a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
seqScopeRow ehM efM p next = case p of
  Route m -> preComposeRoute m next
  LiftPure inMap f (BranchCont k) ->
    LiftPure inMap f (BranchCont (\res -> seqScopeRow ehM efM (k res) next))
  OpF tag# inMap payload conts ->
    let !(I# fullIdx#) = indexRowMorph efM (I# (word2Int# tag#))
        !fullTag# = int2Word# fullIdx#
    in OpF fullTag# inMap payload
         (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqScopeRow ehM efM (k res) next)) conts)
  OpH tag# inMap regions payload conts ->
    let !(I# fullIdx#) = indexRowMorph ehM (I# (word2Int# tag#))
        !fullTag# = int2Word# fullIdx#
        !regions' = rmapFused (\(Region reg) -> Region (ScopeRow ehM efM reg)) regions
    in OpH fullTag# inMap regions' payload
         (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqScopeRow ehM efM (k res) next)) conts)
  ScopeRow ehM2 efM2 inner ->
    seqScopeRow (composeRowMorph ehM2 ehM) (composeRowMorph efM2 efM) inner next
