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
Description : Polycategorical wiring combinators, sequential composition, and Coyoneda leaf fusion
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides polycategorical composition primitives for the Penelope effect system.
See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation]
and Note [Reified Row Morphisms and Gather-Scatter] [ref:note-row-morph-routing].
-}
module Penelope.Core.Wiring
  ( -- * Sequential Composition
    seqPenelope
  , (>>>)

    -- * Route Pre-Composition & Peephole Fusion
  , preComposeRoute

    -- * Coyoneda Leaf Continuation Record Mapping
  , rmapFused
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray
  ( indexSmallArray
  , newSmallArray
  , runSmallArray
  , sizeofSmallArray
  , writeSmallArray
  )
import GHC.Exts (Int (..), Word (..), int2Word#, unsafeCoerce#, word2Int#)
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
  ( RowMorph (..)
  , composeRowMorph
  , indexRowMorph
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
Pre-compose a static 'RowMorph' wire routing stage before a downstream 'Penelope' pipeline.
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
