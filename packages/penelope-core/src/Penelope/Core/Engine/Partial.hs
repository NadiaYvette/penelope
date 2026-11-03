{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Penelope.Core.Engine.Partial
Description : Layered effect peeling and partial interpretation
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides partial effect peeling ('interpretSub'), lowering a subset of first-order
operations (@efSub@) while leaving residual unhandled effects (@efRem@) intact in the syntax tree.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Core.Engine.Partial
  ( -- * Compile-Time Row Partitioning
    RowPartition (..)
  , PartitionPlan (..)
  , mkPartitionPlan

    -- * Partial Interpretation Engine
  , interpretSub
  ) where

import Control.Monad.ST (runST)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.PrimArray
  ( PrimArray
  , indexPrimArray
  , newPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Int (..), int2Word#, unsafeCoerce#, word2Int#)
import Penelope.Core.Engine.Elaborate (MapRow)
import Penelope.Core.Engine.Interpret (Handler (..))
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  )
import Penelope.Core.Wiring (rmapFused)
import Polytropos (Record, RowOf)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (KnownRowMorph (..), RowMorph (..), composeRowMorph, indexRowMorph)

{- |
Runtime partition plan capturing the static decomposition of row @full@ into
disjoint sub-rows @sub@ and @rem@.
The 'ppLookup' unboxed array maps each constructor tag @fullIdx@ to:
* @val >= 0@: 0-based index in @sub@.
* @val < 0@: 0-based index in @rem@ (encoded as @-1 - val@).

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
data PartitionPlan (sub :: RowOf k) (rem :: RowOf k) (full :: RowOf k) = PartitionPlan
  { ppSubMorph :: !(RowMorph sub full)
  , ppRemMorph :: !(RowMorph rem full)
  , ppLookup   :: !(PrimArray Int)
  }

-- | Synthesizes an unboxed lookup array from compile-time row morphisms.
{-# INLINE mkPartitionPlan #-}
mkPartitionPlan ::
  RowMorph sub full
  -> RowMorph rem full
  -> PartitionPlan sub rem full
mkPartitionPlan mSub@(RowMorph arrSub) mRem@(RowMorph arrRem) =
  let !nSub = sizeofPrimArray arrSub
      !nRem = sizeofPrimArray arrRem
      !totalSz = nSub + nRem
      !lookupArr = runST $ do
        m <- newPrimArray totalSz
        let initLoop !i
              | i >= totalSz = pure ()
              | otherwise    = writePrimArray m i (-1) >> initLoop (i + 1)
        initLoop 0
        let fillSub !k
              | k >= nSub = pure ()
              | otherwise = do
                  let !fullIdx = indexPrimArray arrSub k
                  writePrimArray m fullIdx k
                  fillSub (k + 1)
        fillSub 0
        let fillRem !j
              | j >= nRem = pure ()
              | otherwise = do
                  let !fullIdx = indexPrimArray arrRem j
                  writePrimArray m fullIdx (-1 - j)
                  fillRem (j + 1)
        fillRem 0
        unsafeFreezePrimArray m
  in PartitionPlan mSub mRem lookupArr

{- |
Evidence that row @full@ is partitioned into disjoint sub-rows @sub@ and @rem@.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
class (KnownRowMorph sub full, KnownRowMorph rem full) => RowPartition (sub :: RowOf k) (rem :: RowOf k) (full :: RowOf k) where
  rowPartition :: PartitionPlan sub rem full

instance {-# OVERLAPPABLE #-} (KnownRowMorph sub full, KnownRowMorph rem full) => RowPartition sub rem full where
  {-# INLINE rowPartition #-}
  rowPartition = mkPartitionPlan (rowMorph @sub @full) (rowMorph @rem @full)

{- |
Partially peel and interpret a subset of first-order effects @efSub@ from @efFull@,
leaving residual effects @efRem@ intact in the syntax tree.

Dispatches each target effect via \(O(1)\) unboxed lookup in the partition table,
invoking the provided handler, while re-tagging unhandled residual operations to @efRem@.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
{-# INLINE interpretSub #-}
interpretSub ::
  forall (efSub :: RowOf PolyEffect) (efRem :: RowOf PolyEffect) (efFull :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  RowPartition efSub efRem efFull =>
  Record (MapRow (Handler efRem (Penelope '[] efRem)) efSub)
  -> Penelope '[] efFull ins outs
  -> Penelope '[] efRem ins outs
interpretSub !handlers = go
  where
    plan = rowPartition @_ @efSub @efRem @efFull
    arrLookup = ppLookup plan

    interpCont ::
      BranchCont (Penelope '[] efFull i o) branchOuts
      -> BranchCont (Penelope '[] efRem i o) branchOuts
    interpCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    go :: Penelope '[] efFull i o -> Penelope '[] efRem i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap f cont ->
        LiftPure inMap f (interpCont cont)

      OpF tag# inMap opAny conts ->
        let !idx = I# (word2Int# tag#)
            !entry = indexPrimArray arrLookup idx
            !interpretedConts = rmapFused interpCont conts
        in if entry >= 0
             then
               let !(RowRep (Rep _ arr)) = handlers
                   !(Identity handlerAny) = indexSmallArray arr entry
                   !(Handler handleFn) = unsafeCoerce# handlerAny
               in handleFn (unsafeCoerce# inMap) opAny interpretedConts
             else
               let !(I# i#) = -1 - entry
                   !remIdx# = int2Word# i#
               in OpF remIdx# inMap opAny interpretedConts

      ScopeRow _ehMorph efMorph subPipe ->
        goInScope (unsafeCoerce# efMorph) (unsafeCoerce# subPipe)

      OpH {} ->
        errorWithoutStackTrace "Penelope.Core.Engine.Partial: impossible OpH in empty '[] higher-order row"

    goInScope :: RowMorph efInner efFull -> Penelope '[] efInner i o -> Penelope '[] efRem i o
    goInScope !efM = \case
      Route morph ->
        Route morph

      LiftPure inMap f (BranchCont k) ->
        LiftPure inMap f (BranchCont (\branchFrame -> goInScope efM (k branchFrame)))

      OpF tag# inMap opAny conts ->
        let !idx = indexRowMorph efM (I# (word2Int# tag#))
            !entry = indexPrimArray arrLookup idx
            !interpretedConts = rmapFused (\(BranchCont k) -> BranchCont (\branchFrame -> goInScope efM (k branchFrame))) conts
        in if entry >= 0
             then
               let !(RowRep (Rep _ arr)) = handlers
                   !(Identity handlerAny) = indexSmallArray arr entry
                   !(Handler handleFn) = unsafeCoerce# handlerAny
               in handleFn (unsafeCoerce# inMap) opAny interpretedConts
             else
               let !(I# i#) = -1 - entry
                   !remIdx# = int2Word# i#
               in OpF remIdx# inMap opAny interpretedConts

      ScopeRow _ehInner efMorph2 inner ->
        goInScope (composeRowMorph (unsafeCoerce# efMorph2) efM) (unsafeCoerce# inner)

      OpH {} ->
        errorWithoutStackTrace "Penelope.Core.Engine.Partial: impossible OpH in empty '[] higher-order row"
