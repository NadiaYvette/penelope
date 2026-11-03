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

See Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
preComposeRoute ::
  forall mid ins (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (outs :: RowOf Type).
  RowMorph mid ins
  -> Penelope eh ef mid outs
  -> Penelope eh ef ins outs
preComposeRoute m1 (Route m2) = Route (fuseRoute m1 m2)
preComposeRoute m1 next
  | isIdentityRowMorph m1 = unsafeCoerce next
preComposeRoute m1 (LiftPure inMap f cont) =
  LiftPure
    (fuseRoute m1 inMap)
    f
    (BranchCont (\res -> preComposeRoute m1 (runBranchCont cont res)))
preComposeRoute m1 (OpF tag# inMap payload conts) =
  OpF
    tag#
    (fuseRoute m1 inMap)
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
preComposeRoute m1 (OpH tag# inMap regions payload conts) =
  OpH
    tag#
    (fuseRoute m1 inMap)
    regions
    payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> preComposeRoute m1 (k res))) conts)
preComposeRoute m1 (ScopeRow ehM efM inner) =
  ScopeRow ehM efM (preComposeRoute m1 inner)

{- |
Sequential polycategorical composition of Penelope pipelines.
Substitutes downstream pipelines into continuation leaves with Coyoneda fusion.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
seqPenelope ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (a :: RowOf Type) (b :: RowOf Type) (c :: RowOf Type).
  Penelope eh ef a b
  -> Penelope eh ef b c
  -> Penelope eh ef a c
seqPenelope (Route m) next = preComposeRoute m next
seqPenelope (LiftPure inMap f (BranchCont k)) next =
  LiftPure inMap f (BranchCont (\res -> seqPenelope (k res) next))
seqPenelope (OpF tag# inMap payload conts) next =
  OpF tag# inMap payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
seqPenelope (OpH tag# inMap regions payload conts) next =
  OpH tag# inMap regions payload
    (rmapFused (\(BranchCont k) -> BranchCont (\res -> seqPenelope (k res) next)) conts)
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
    go (Route m) =
      LiftPure
        (fpGatherIn plan)
        (\(RowRep inRep) -> RowRep (gatherRep m inRep))
        (BranchCont $ \(RowRep outRep) ->
          LiftPure
            (unsafeMkRowMorph [0 .. baseSz - 1])
            (\(RowRep frameRep) -> RowRep (executeFrameScatter plan frameRep outRep))
            (BranchCont $ \_ -> Route (unsafeMkRowMorph [0 .. outSz - 1])))
    go (LiftPure inMap f cont) =
      LiftPure (composeRowMorph inMap (fpGatherIn plan)) f (liftCont cont)
    go (OpF tag# inMap payload conts) =
      OpF tag# (composeRowMorph inMap (fpGatherIn plan)) payload (rmapFused liftCont conts)
    go (OpH tag# inMap regions payload conts) =
      OpH tag# (composeRowMorph inMap (fpGatherIn plan)) regions payload (rmapFused liftCont conts)
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
(>?<) (OpF tag# inMap payload conts) handlers =
  let RowRep (Rep _ arr) = conts
  in if sizeofSmallArray arr == 0
       then OpF tag# inMap payload (unsafeCoerce handlers)
       else OpF tag# inMap payload
              (rmapFused (\(BranchCont k) -> BranchCont (\res -> k res >?< handlers)) conts)
(>?<) (OpH tag# inMap regions payload conts) handlers =
  let RowRep (Rep _ arr) = conts
  in if sizeofSmallArray arr == 0
       then OpH tag# inMap regions payload (unsafeCoerce handlers)
       else OpH tag# inMap regions payload
              (rmapFused (\(BranchCont k) -> BranchCont (\res -> k res >?< handlers)) conts)
(>?<) (LiftPure inMap f cont) handlers =
  LiftPure inMap f (BranchCont (\res -> runBranchCont cont res >?< handlers))
(>?<) (ScopeRow ehM efM inner) handlers =
  ScopeRow ehM efM (inner >?< handlers)
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

{- |
Helper to sequence through regional 'ScopeRow' boundaries to downstream continuations.
-}
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
