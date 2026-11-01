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

{- |
Module      : Penelope.Core.Engine.Elaborate
Description : Phase 1 single-pass bottom-up higher-order effect elaboration
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides Phase 1 single-pass bottom-up macro elaboration of higher-order scoped
effects into pure first-order operations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
module Penelope.Core.Engine.Elaborate
  ( -- * Elaborator Abstraction
    Elaborator (..)
  , Elaborators
  , MapRow

    -- * Phase 1 Elaboration Engine
  , elaborateAll
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Any, Int (..), unsafeCoerce#, word2Int#)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , Region (..)
  )
import Penelope.Core.Wiring (rmapFused)
import Polytropos (Pair (..), Record, RowOf, gatherRecord)
import Polytropos.Core (Rep (..), RowRep (..))
import Polytropos.RowMorph (RowMorph, unsafeMkRowMorph)

-- | Map a higher-order type constructor over each field of a row.
type family MapRow (f :: k -> Type) (r :: RowOf k) :: RowOf Type where
  MapRow f '[] = '[]
  MapRow f ((l ':= v) ': rest) = (l ':= f v) ': MapRow f rest

-- | Convenient type alias for a record of elaborators.
type Elaborators ef eh = Record (MapRow (Elaborator '[] ef) eh)

{- |
An 'Elaborator' translates a higher-order \((m, n, k, r)\)-ary scoped operation @h@
into a pure first-order 'Penelope' computation.

Notice that:
1. All \(r\) nested regions in @Record regions@ have ALREADY
   been recursively elaborated before 'runElaborator' is called (bottom-up catamorphism).
2. The elaborator receives the \(k\) downstream continuation branches
   @Record exits@ directly, allowing it to wire region exits straight into downstream
   branches without intermediate ADT allocations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
newtype Elaborator
          (eh' :: RowOf PolyHEffect)
          (ef  :: RowOf PolyEffect)
          (h   :: PolyHEffect)
  = Elaborator
  { runElaborator
      :: forall (regions :: RowOf Type) (opIns :: RowOf Type) (exits :: RowOf Type)
                (frameIns :: RowOf Type) (frameOuts :: RowOf Type).
         RowMorph opIns frameIns
      -> Record regions
      -> Any
      -> Record exits
      -> Penelope eh' ef frameIns frameOuts
  }

{- |
Elaborates all higher-order scoped effects @eh@ into pure first-order effects @ef@
in a single bottom-up pass over the syntax tree.
Dispatch at each 'OpH' node is a branchless \(O(1)\) 'indexSmallArray#' read into the
Polytropos 'Record' of elaborators.

Contravariant reduct gathering via 'gatherRecord' adapts outer elaborators to inner
scopes across 'ScopeRow' boundaries in \(O(|eh_{sub}|)\) time without walking the inner sub-tree.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine] and
Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
-}
{-# INLINE elaborateAll #-}
elaborateAll ::
  forall (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect)
         (ins :: RowOf Type) (outs :: RowOf Type).
  Record (MapRow (Elaborator '[] ef) eh)
  -> Penelope eh ef ins outs
  -> Penelope '[] ef ins outs
elaborateAll !elabs = go
  where
    elabCont ::
      BranchCont (Penelope eh ef i o) branchOuts
      -> BranchCont (Penelope '[] ef i o) branchOuts
    elabCont (BranchCont k) = BranchCont (\branchFrame -> go (k branchFrame))

    elabRegion :: Region eh ef sig -> Region '[] ef sig
    elabRegion (Region subPipe) = Region (elaborateAll elabs subPipe)

    go :: Penelope eh ef i o -> Penelope '[] ef i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap f cont ->
        LiftPure inMap f (elabCont cont)

      OpF tag# inMap opAny conts ->
        OpF tag# inMap opAny (rmapFused elabCont conts)

      OpH tag# inMap regions opAny conts ->
        let !elaboratedRegions = rmapFused elabRegion regions
            !elaboratedConts   = rmapFused elabCont conts
            !(RowRep (Rep _ arr)) = elabs
            !(Identity elabAny) = indexSmallArray arr (I# (word2Int# tag#))
            !(Elaborator elabFn) = unsafeCoerce# elabAny
        in elabFn (unsafeCoerce# inMap) elaboratedRegions opAny elaboratedConts

      ScopeRow ehMorph efMorph subPipe ->
        let !subElabs = unsafeCoerce# (gatherRecord (unsafeCoerce# ehMorph) elabs)
        in ScopeRow (unsafeMkRowMorph []) efMorph (elaborateAll subElabs subPipe)
