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

{- Note [Unpacked Polycategorical Syntax Representation]
The 'Penelope' GADT embodies the core mathematical abstraction of our effect system:
computations are structured as open polycategorical nodes over Polytropos row types:

@
  data Penelope (eh   :: RowOf PolyHEffect)  -- r higher-order scoped regions
                (ef   :: RowOf PolyEffect)   -- active first-order algebraic effects
                (ins  :: RowOf Type)         -- m conjunctive input register ports
                (outs :: RowOf Type)         -- n conjunctive output register ports
@

The five constructors of 'Penelope' correspond directly to primitive hypergraph topologies:

1. 'Route' (Pure Wire Permutation):
   Connects input ports to output ports via a static 'RowMorph'. No arithmetic or
   side effects occur; at runtime, it evaluates as a zero-copy or flat array gather.

2. 'LiftPure' (Pure Functional Transform):
   Lifts an opaque Haskell function transforming a subset of input fields 'opIns'
   into output fields 'opOuts', feeding results forward into a 'BranchCont'.

3. 'OpF' (Unpacked First-Order Algebraic Operation):
   Encapsulates an operation from the first-order effect signature @ef@. The constructor
   tag is unboxed directly as a machine word (@Word#@), avoiding constructor boxing.
   Input ports are gathered via 'inMap', the operation payload is held as 'Any',
   and exit branches are represented as a flat 'Record exits' of continuations.

4. 'OpH' (Unpacked Higher-Order Scoped Operation):
   Generalizes 'OpF' by incorporating a record of $r$ scoped sub-hypergraphs ('Record regions').
   Used for delimited constructs like 'Local', 'Catch', 'While', and 'Saga'.

5. 'ScopeRow' (Zero-Traversal Effect Embedding):
   Embeds an inner computation operating on smaller effect rows (@ehSub@, @efSub@)
   into a wider enclosing signature (@ehFull@, @efFull@). Evaluates in $O(1)$ time
   without rewriting syntax nodes, preserving institutional satisfaction.
-}
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

-- | Extract the input port row from a 't:PortSig'.
type family RegionIns (sig :: PortSig) :: RowOf Type where
  RegionIns ('PortSig ins _) = ins

-- | Extract the output port row from a 't:PortSig'.
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
  -- Maps output register indices back to source input register indices via an unboxed
  -- 'PrimArray'. Evaluates as a pure gather without arithmetic or memory allocation.
  Route ::
    !(RowMorph outs ins)
    -> Penelope eh ef ins outs

  -- | Opaque pure computational transformation with explicit return continuation.
  -- Projects input ports via 'opIns', executes the pure transformation function,
  -- and passes the produced 'opOuts' record into the continuation leaf.
  LiftPure ::
    !(RowMorph opIns ins)
    -> (Record opIns -> Record opOuts)
    -> !(BranchCont (Penelope eh ef ins outs) opOuts)
    -> Penelope eh ef ins outs

  -- | Unpacked first-order algebraic operation with unboxed constructor tag,
  -- input gather wiring, dynamic payload, and multi-exit branch continuations.
  -- Tag comparison is performed via 'eqWord#' directly on machine registers.
  OpF ::
    Word#
    -> !(RowMorph opIns ins)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Unpacked higher-order scoped operation with unboxed constructor tag,
  -- input gather wiring, record of scoped sub-regions, dynamic payload,
  -- and multi-exit branch continuations. Enclosed regions are elaborated
  -- in a single bottom-up pass during Phase 1.
  OpH ::
    Word#
    -> !(RowMorph opIns ins)
    -> !(Record regions)
    -> Any
    -> !(Record exits)
    -> Penelope eh ef ins outs

  -- | Scoped regional boundary for zero-traversal effect row weakening.
  -- Embeds smaller effect rows into wider ambient signatures in \(\mathcal{O}(1)\) time.
  -- See Note [Institutional Satisfaction and Zero-Traversal Weakening] [ref:note-satisfaction-weakening].
  ScopeRow ::
    !(RowMorph ehSub ehFull)
    -> !(RowMorph efSub efFull)
    -> !(Penelope ehSub efSub ins outs)
    -> Penelope ehFull efFull ins outs
