{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Penelope.Core.Syntax
Description : Foundational (m,n,k,r)-ary polycategorical syntax GADT and type-level signatures
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

This module implements the foundational syntax GADT for Penelope's
$(m,n,k,r)$-ary polycategorical effect engine.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation].
-}
module Penelope.Core.Syntax
  ( -- * Type-Level Signatures
    PortSig (..)
  , RegionIns
  , RegionOuts
  , PolyEffect
  , PolyHEffect

    -- * Continuations and Scoped Regions
  , BranchCont (..)
  , Region (..)

    -- * Core Syntax GADT
  , Penelope (..)
  ) where

import Data.Kind (Type)
import GHC.Exts (Any, Word#)
import Polytropos (Record, RowOf)
import Polytropos.RowMorph (RowMorph)

{- |
Polycategorical port signature encoding input ports and output ports.
Used as the sort for higher-order scoped sub-regions.
-}
data PortSig = PortSig (RowOf Type) (RowOf Type)

-- | Extract the input port row from a 'PortSig'.
type family RegionIns (sig :: PortSig) :: RowOf Type where
  RegionIns ('PortSig ins _) = ins

-- | Extract the output port row from a 'PortSig'.
type family RegionOuts (sig :: PortSig) :: RowOf Type where
  RegionOuts ('PortSig _ outs) = outs

{- |
Signature of a first-order polycategorical effect operation.
Takes an input argument row and a row of disjunctive exit rows.
-}
type PolyEffect = RowOf Type -> RowOf (RowOf Type) -> Type

{- |
Signature of a higher-order scoped polycategorical effect operation.
Takes:
1. Region representation runner capability
2. Row of scoped region port signatures
3. Input argument row
4. Row of disjunctive exit rows
-}
type PolyHEffect =
  (PortSig -> Type)
  -> RowOf PortSig
  -> RowOf Type
  -> RowOf (RowOf Type)
  -> Type

{- |
Multi-exit branch continuation mapping a resolved output record into a downstream computation.
-}
newtype BranchCont m (outs :: RowOf Type) = BranchCont
  { runBranchCont :: Record outs -> m
  }

{- |
Scoped sub-region representation wrapping an inner polycategorical computation.
-}
newtype Region (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (sig :: PortSig) = Region
  { runRegion :: Penelope eh ef (RegionIns sig) (RegionOuts sig)
  }

{- |
The core (m,n,k,r)-ary polycategorical syntax GADT.

See Note [Unpacked Polycategorical Representation] [ref:note-unpacked-representation]
for the rationale behind unboxing 'Word#' variant tags and 'Any' payloads directly
into GADT constructors.
-}
data Penelope (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) (ins :: RowOf Type) (outs :: RowOf Type) where
  -- | Static wire permutation or subset extraction.
  Route ::
    !(RowMorph outs ins)
    -> Penelope eh ef ins outs

  -- | Opaque pure computational transformation with explicit return continuation.
  LiftPure ::
    !(RowMorph opIns ins)
    -> (Record opIns -> Record opOuts)
    -> !(BranchCont (Penelope eh ef ins outs) opOuts)
    -> Penelope eh ef ins outs

  -- | Unpacked first-order algebraic operation with unboxed constructor tag,
  -- input gather wiring, dynamic payload, and multi-exit branch continuations.
  OpF ::
    Word#
    -> !(RowMorph opIns ins)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Unpacked higher-order scoped operation with unboxed constructor tag,
  -- input gather wiring, record of scoped sub-regions, dynamic payload,
  -- and multi-exit branch continuations.
  OpH ::
    Word#
    -> !(RowMorph opIns ins)
    -> !(Record regions)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Scoped regional boundary for zero-traversal effect row weakening.
  -- See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
  ScopeRow ::
    !(RowMorph ehSub ehFull)
    -> !(RowMorph efSub efFull)
    -> !(Penelope ehSub efSub ins outs)
    -> Penelope ehFull efFull ins outs
