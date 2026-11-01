{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

{- |
Module      : Penelope.Core.Engine.Interpret
Description : Phase 2 bulk vector-dispatched algebraic interpretation
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides Phase 2 bulk vector-dispatched algebraic interpretation over first-order operations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
module Penelope.Core.Engine.Interpret
  ( -- * Carrier Abstraction
    PolyCarrier (..)
  , FrameKleisli (..)

    -- * Handler Abstraction
  , Handler (..)
  , Handlers

    -- * Phase 2 Interpretation Engine
  , interpretAll
  , runPenelopePure
  ) where

import Control.Category (Category (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Any, Int (..), unsafeCoerce#, word2Int#)
import Penelope.Core.Engine.Elaborate (MapRow)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  )
import Penelope.Core.Wiring (rmapFused)
import Polytropos (Record, RowOf, emptyRecord, gatherRecord)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (RowMorph, gatherRep)
import Prelude hiding (id, (.))

{- |
Typeclass for target semantic carriers \(m :: RowOf Type -> RowOf Type -> Type\)
that can execute compiled first-order 'Penelope' graphs.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
class Category m => PolyCarrier m where
  -- | Execute a pure static 'RowMorph' port permutation or projection.
  evalRoute :: RowMorph outs ins -> m ins outs

  -- | Execute a pure step over a gathered subset of ports and feed outputs to continuation.
  evalLiftPure ::
    RowMorph opIns ins
    -> (Record opIns -> Record opOuts)
    -> (Record opOuts -> m ins outs)
    -> m ins outs

{- |
Standard in-memory frame execution carrier: a Kleisli arrow over flat 'Record' frames.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
newtype FrameKleisli (f :: Type -> Type) (ins :: RowOf Type) (outs :: RowOf Type) = FrameKleisli
  { runFrameKleisli :: Record ins -> f (Record outs)
  }

instance Monad f => Category (FrameKleisli f) where
  id = FrameKleisli pure
  FrameKleisli g . FrameKleisli h = FrameKleisli (\r -> h r >>= g)

instance Monad f => PolyCarrier (FrameKleisli f) where
  {-# INLINE evalRoute #-}
  evalRoute morph = FrameKleisli $ \(RowRep rep) ->
    pure (RowRep (gatherRep morph rep))

  {-# INLINE evalLiftPure #-}
  evalLiftPure inMap f k = FrameKleisli $ \(RowRep inRep) -> do
    let !opInFrame = RowRep (gatherRep inMap inRep)
        !opOutFrame = f opInFrame
    runFrameKleisli (k opOutFrame) (unsafeCoerce# opOutFrame)

{- |
A first-order \((m, n, k)\)-ary effect handler into target carrier @m@.
Receives:
1. 'RowMorph opIns frameIns': Static map to extract the operation's \(m\) input ports.
2. 'Any': Dynamic payload of the first-order operation.
3. 'Record exits': Downstream continuation branches already interpreted into carrier @m@.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
newtype Handler
          (ef :: RowOf PolyEffect)
          (m  :: RowOf Type -> RowOf Type -> Type)
          (e  :: PolyEffect)
  = Handler
  { runHandler
      :: forall (opIns :: RowOf Type) (exits :: RowOf Type)
                (frameIns :: RowOf Type) (frameOuts :: RowOf Type).
         RowMorph opIns frameIns
      -> Any
      -> Record exits
      -> m frameIns frameOuts
  }

-- | Convenient type alias for a record of first-order handlers.
type Handlers ef m = Record (MapRow (Handler '[] m) ef)

{- |
Phase 2 bulk vector-dispatched algebraic interpretation over first-order operations.
Dispatches each 'OpF' constructor tag via branchless \(O(1)\) 'indexSmallArray#' read
into the handler record, with zero linear search cascades.

Contravariant handler reduct gathering via 'gatherRecord' adapts outer handlers to inner
scopes across 'ScopeRow' boundaries in \(O(|ef_{sub}|)\) time without walking the inner sub-tree.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
{-# INLINE interpretAll #-}
interpretAll ::
  forall (ef :: RowOf PolyEffect) m (ins :: RowOf Type) (outs :: RowOf Type).
  PolyCarrier m =>
  Record (MapRow (Handler '[] m) ef)
  -> Penelope '[] ef ins outs
  -> m ins outs
interpretAll !handlers = go
  where
    interpCont ::
      BranchCont (Penelope '[] ef i o) branchOuts
      -> BranchCont (m i o) branchOuts
    interpCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    go :: Penelope '[] ef i o -> m i o
    go = \case
      Route morph ->
        evalRoute morph

      LiftPure inMap f (BranchCont k) ->
        evalLiftPure inMap f (\opOutFrame -> go (k opOutFrame))

      OpF tag# inMap opAny conts ->
        let !interpretedConts = rmapFused interpCont conts
            !(RowRep (Rep _ arr)) = handlers
            !(Identity handlerAny) = indexSmallArray arr (I# (word2Int# tag#))
            !(Handler handleFn) = unsafeCoerce# handlerAny
        in handleFn (unsafeCoerce# inMap) opAny interpretedConts

      ScopeRow _ehMorph efMorph subPipe ->
        let !subHandlers = unsafeCoerce# (gatherRecord (unsafeCoerce# efMorph) handlers)
        in interpretAll subHandlers (unsafeCoerce# subPipe)

      OpH {} ->
        errorWithoutStackTrace "Penelope.Core.Engine.Interpret: impossible OpH in empty '[] higher-order row"

{- |
Run a fully handled pure 'Penelope' pipeline over an initial register frame.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
{-# INLINE runPenelopePure #-}
runPenelopePure ::
  Penelope '[] '[] ins outs
  -> Record ins
  -> Record outs
runPenelopePure pipe inFrame =
  case runFrameKleisli (interpretAll emptyRecord pipe) inFrame of
    Identity outFrame -> outFrame
