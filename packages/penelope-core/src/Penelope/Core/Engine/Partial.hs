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

{- Note [Layered Effect Peeling and Partial Interpretation]
Standard algebraic effect handlers assume an "all-or-nothing" interpretation model:
the entire effect signature is dispatched to a base monad (like 'IO' or 'Identity')
in a single pass. However, complex multi-tier systems require partial or staged interpretation:

1. A sandboxed business-logic layer handles domain effects (e.g. 'State', 'Reader')
   in memory, but leaves system-level effects (e.g. 'Database', 'Network') untouched.
2. An optimization pass compiles pure sub-circuits into GCC -fgimple or MLIR SSA blocks,
   residualizing effect operations that require external JIT handlers.

'Penelope.Core.Engine.Partial' achieves this through 'interpretSub' and 'PartitionPlan':
- At compile-time, 'RowPartition' splits the effect row into handled effects @efSub@
  and residual effects @efRem@.
- A fast unboxed lookup table ('PartitionPlan') maps every original effect opcode
  either to its index in the handler vector (if handled) or to its new index in the
  residual row (if residualized).
- 'interpretSub' walks the hypergraph: handled operations evaluate effectfully into
  carrier continuations, while unhandled operations are faithfully reconstructed as
  residual 'OpF' nodes.
-}

{- Note [Staged Effect Residualization and Code Generation Partitioning]
In heterogeneous compilation pipelines (e.g. compiling to freestanding Linux kernel modules,
eBPF bytecode, or WebAssembly), some effect operations can be fully evaluated or staged away
at compile time, while others must be delegated to runtime host interfaces:

1. **Staged Normalization:**
   Effects representing compiler metadata, gas/fuel accounting ('Fuel'), or local configuration
   ('Reader') can be interpreted completely during staging via 'interpretSub'.
   The resulting residual term 'Penelope \'[] efRem ins outs' is stripped of those effects,
   shrinking the required handler vector and eliminating dispatch branches.

2. **Opcode Re-Indexing Invariant:**
   When an unhandled operation 'OpF tag# inMap opAny conts' is preserved in the residual term,
   its original constructor tag @tag#@ (which corresponded to its position in @efFull@)
   must be re-indexed to match its new position in @efRem@.
   'PartitionPlan' accomplishes this in $O(1)$ by storing @-1 - newIndex@ in 'ppLookup'.
   The decoder @remIdx# = int2Word# (-1 - entry)@ reconstructs the valid residual opcode
   with zero arithmetic branching.

3. **Regional Invariance Across ScopeRow Boundaries:**
   When partial interpretation crosses into a regional 'ScopeRow' envelope, 'goInScope'
   composes the regional embedding morphism with the ambient partition plan, ensuring that
   locally scoped effects are correctly classified as handled or residual.
-}
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
{- |
Construct a 'PartitionPlan' from explicit embedding row morphisms @sub@ and @rem@.

== Algorithm & Invariants
1. Allocates an unboxed 'PrimArray' of size @|sub| + |rem|@ initialized to sentinel values.
2. For each index @k@ in @sub@, stores @k@ at index @arrSub[k]@.
3. For each index @j@ in @rem@, stores @-1 - j@ at index @arrRem[j]@.
4. The resulting lookup array enables \(\mathcal{O}(1)\) branchless classification of any opcode tag.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
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

== Operational Pipeline & Invariants
1. __Bipolar Partition Lookup Table:__
   The unboxed primitive array 'ppLookup' encodes the partition state for every opcode index:
   - Non-negative value @entry >= 0@: Target effect belongs to @efSub@; @entry@ is the
     direct index into the 'handlers' record.
   - Negative value @entry < 0@: Unhandled residual effect belonging to @efRem@;
     the new tag index is derived in \(\mathcal{O}(1)\) time as @newTag# = int2Word# (-1 - entry)@.
2. __Staged Effect Residualization:__
   Handled nodes expand into algebraic handler pipelines whose continuation leaves
   seamlessly re-enter 'interpretSub', while residual nodes are re-tagged and preserved.
3. __No Dynamic Search Overhead:__
   Every opcode classification and tag re-indexing step runs in \(\mathcal{O}(1)\) time
   via unboxed array indexing without string lookups or dictionary passing.

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
               -- Handled effect: dispatch operation to handler function closure
               let !(RowRep (Rep _ arr)) = handlers
                   !(Identity handlerAny) = indexSmallArray arr entry
                   !(Handler handleFn) = unsafeCoerce# handlerAny
               in handleFn (unsafeCoerce# inMap) opAny interpretedConts
             else
               -- Residual unhandled effect: decode new opcode and reconstruct OpF node
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
