{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Penelope.Static.Analysis
Description : Static pre-execution Cayley analysis, capability extraction, and liveness analysis
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides compile-time and pre-execution static inspection of Penelope pipelines without
evaluating runtime payloads.
See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
module Penelope.Static.Analysis
  ( -- * Static Accumulator
    StaticPenelope (..)
  , annotateStatic
  , seqStatic
  , frameWireStatic

    -- * Phantom Port Frame Construction
  , phantomFrame

    -- * Liveness and Capability Telemetry
  , LivenessReport (..)
  , morphReadSet
  , analyzeLivenessAndEffects
  ) where

import Data.Functor.Identity (Identity (..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)
import Data.Primitive.PrimArray (sizeofPrimArray)
import Data.Primitive.SmallArray (indexSmallArray, newSmallArray, runSmallArray, sizeofSmallArray)
import GHC.Exts (Int (..), unsafeCoerce#, word2Int#)
import Penelope.Core
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , PortSig (..)
  , Region (..)
  , (>&>)
  , seqPenelope
  )
import Polytropos (Record, RowOf)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph
  ( FrameWire (..)
  , RowMorph (..)
  , indexRowMorph
  )

{- |
Cayley static wrapper generalizing static effect accumulators to (m, n, k, r)-ary graphs.
Pairs an Applicative metadata accumulator @f ()@ with an executable 'Penelope' pipeline.

See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
data StaticPenelope
       (f    :: Type -> Type)
       (eh   :: RowOf PolyHEffect)
       (ef   :: RowOf PolyEffect)
       (ins  :: RowOf Type)
       (outs :: RowOf Type)
  = StaticPenelope
  { spStatic   :: !(f ())
  , spPipeline :: !(Penelope eh ef ins outs)
  }

-- | Attach a static Applicative metadata requirement to any Penelope computation.
{-# INLINE annotateStatic #-}
annotateStatic ::
  f ()
  -> Penelope eh ef ins outs
  -> StaticPenelope f eh ef ins outs
annotateStatic !meta !pipe = StaticPenelope meta pipe

-- | Sequential composition accumulates static metadata via '*>' in O(1).
{-# INLINE seqStatic #-}
seqStatic ::
  Applicative f =>
  StaticPenelope f eh ef a b
  -> StaticPenelope f eh ef b c
  -> StaticPenelope f eh ef a c
seqStatic (StaticPenelope s1 p1) (StaticPenelope s2 p2) =
  StaticPenelope (s1 *> s2) (p1 `seqPenelope` p2)

-- | Frame-rule auto-wiring preserving and combining static metadata.
{-# INLINE frameWireStatic #-}
frameWireStatic ::
  forall (frameIn :: RowOf Type) (ins1 :: RowOf Type) (outs1 :: RowOf Type)
         (frameMid :: RowOf Type) (ins2 :: RowOf Type) (outs2 :: RowOf Type)
         (frameOut :: RowOf Type)
         (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect)
         f.
  ( Applicative f
  , FrameWire frameIn ins1 outs1 frameMid
  , FrameWire frameMid ins2 outs2 frameOut
  ) =>
  StaticPenelope f eh ef ins1 outs1
  -> StaticPenelope f eh ef ins2 outs2
  -> StaticPenelope f eh ef frameIn frameOut
frameWireStatic (StaticPenelope s1 p1) (StaticPenelope s2 p2) =
  StaticPenelope (s1 *> s2) ((>&>) @_ @_ @_ @frameMid p1 p2)

{- |
Construct a phantom port frame for traversing downstream continuations without evaluating payloads.
Uses uninitialized/dummy identity slots that do not throw exceptions.

See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
{-# INLINE phantomFrame #-}
phantomFrame :: Int -> Record r
phantomFrame !numPorts = RowRep $ Rep 0 $ runSmallArray $ do
  m <- newSmallArray numPorts (Identity (unsafeCoerce# ()))
  pure m

{- |
Static telemetry collected across the hypergraph without executing a single effect handler.

See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
data LivenessReport = LivenessReport
  { lrReadFrameSlots     :: !IntSet
    -- ^ Indices of input ports in @ins@ that are actually read by at least one operation.
  , lrActiveFirstOrder   :: !IntSet
    -- ^ Slot indices in @ef@ actually invoked in the graph (for dead-handler pruning).
  , lrActiveHigherOrder  :: !IntSet
    -- ^ Slot indices in @eh@ actually invoked in the graph.
  , lrTotalRegions       :: !Int
    -- ^ Total number of nested higher-order regions ('OpH') in the graph.
  , lrMaxBranchingFactor :: !Int
    -- ^ Maximum exit arity encountered across all operations.
  } deriving stock (Show, Eq)

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

-- | Extract the set of frame slot indices read by a 't:RowMorph'.
{-# INLINE morphReadSet #-}
morphReadSet :: RowMorph a b -> IntSet
morphReadSet (RowMorph primArr) =
  let !n = sizeofPrimArray primArr
      collect !i !acc
        | i >= n    = acc
        | otherwise = collect (i + 1) (IntSet.insert (indexRowMorph (RowMorph primArr) i) acc)
  in collect 0 IntSet.empty

{- |
Universal structural liveness and effect footprint analysis pass.
Directly inspects unpacked constructor tags, 't:RowMorph' primitive arrays, and 'Record' lengths.

See Note [Static Reflection and Cayley Analysis] [ref:note-static-cayley].
-}
analyzeLivenessAndEffects ::
  forall eh ef ins outs.
  Penelope eh ef ins outs
  -> LivenessReport
analyzeLivenessAndEffects = go
  where
    analyzeRegionArr :: Record regions -> (Int, LivenessReport)
    analyzeRegionArr (RowRep (Rep _ arr)) =
      let !rCount = sizeofSmallArray arr
          step !i !acc
            | i >= rCount = acc
            | otherwise   =
                case (unsafeCoerce# (indexSmallArray arr i) :: Region eh ef ('PortSig ins outs)) of
                  Region sub -> step (i + 1) (acc <> analyzeLivenessAndEffects sub)
      in (rCount, step 0 mempty)

    analyzeContArr :: Record exits -> (Int, LivenessReport)
    analyzeContArr (RowRep (Rep _ arr)) =
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
        let !slotIdx         = I# (word2Int# tag#)
            (!r, regReport)  = analyzeRegionArr regions
            (!k, contReport) = analyzeContArr conts
        in LivenessReport
             { lrReadFrameSlots     = morphReadSet inMap
             , lrActiveFirstOrder   = IntSet.empty
             , lrActiveHigherOrder  = IntSet.singleton slotIdx
             , lrTotalRegions       = r
             , lrMaxBranchingFactor = k
             } <> regReport <> contReport

      ScopeRow (RowMorph ehMap) (RowMorph efMap) subPipe ->
        let inner = analyzeLivenessAndEffects subPipe
            remapSet primArr s =
              IntSet.fromList [ indexRowMorph (RowMorph primArr) i | i <- IntSet.toList s ]
        in inner
             { lrActiveHigherOrder = remapSet ehMap (lrActiveHigherOrder inner)
             , lrActiveFirstOrder  = remapSet efMap (lrActiveFirstOrder inner)
             }
