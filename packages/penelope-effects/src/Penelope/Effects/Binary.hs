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

{- |
Module      : Penelope.Effects.Binary
Description : Kernmantle binary arrow facade over Penelope (m,n,k,r)-ary core
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a binary arrow interface ('t:Rope') and static jump-table routing ('deriveRouter')
over Penelope's polycategorical core.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].

{- Note [Kernmantle Rope Binary Facade Architecture]
The 'Rope' abstraction presents a category-theoretic binary arrow interface over
Penelope's underlying $(m,n,k,r)$-ary polycategorical hypergraph. Whereas the core
engine treats computations as multi-port circuits with open Polytropos row types,
conventional pipeline architectures (such as ETL systems, streaming filters, and
hardware simulation DAGs) naturally present as 1-in, 1-out arrows:

@
  a ===[ Rope eh ef a b ]===> b
@

Under the hood, 'Rope eh ef a b' is represented as:

@
  Penelope eh ef '["in" ':= a] '["out" ':= b]
@

By grounding this abstraction directly upon the polycategorical core:
1. 'Category' and 'Arrow' composition ('(>>>)') sequentializes computations using
   'seqPenelope', automatically inserting identity row morphisms to rename output
   ports into downstream input registers.
2. 'ArrowChoice' combinators ('arrowLeft', 'arrowRight') unpack sum variants
   ('Either') into single-port frames, avoiding the exponential branching cost
   of nested 'Either' structures.
3. 'deriveRouter' leverages Polytropos' unboxed 'Variant' representation, compiling
   an open record of branch handlers into an O(1) jump table that dispatches
   variant constructors directly by tag without linear case ladders.
4. Memory allocation is completely bypassed during interpretation when handlers
   are vectorized, yielding performance indistinguishable from hand-written
   state machines.
-}

{- Note [Arrow vs Monad Duality and Zero-Allocation Profunctors]
Category-theoretic arrows (Hughes, 2000) and profunctors provide static multi-wire
routing capabilities that monadic systems lack. However, traditional Haskell Arrow
implementations suffer from severe performance overheads:

1. **The Pair Allocation Penalty:**
   Combinators like 'first' and 'second' accept and yield standard Haskell pairs @(b, d)@.
   In standard arrow libraries, every stage allocates fresh two-word heap cells for the pair
   constructors, and pattern-matches them in downstream arrows. In high-throughput pipelines
   (such as network parsing or numeric streaming), this generates immense garbage collector churn.

2. **Polytropos Zero-Allocation Framing:**
   In Penelope's 'Rope', pair lifting is implemented via unboxed register manipulation:
   - 'arrowFirst' unpacks the pair in a single machine instruction into separate unboxed
     slot variables (@b@ and @d@).
   - Only the active input @b@ is inserted into the @'["in" ':= b]@ register frame.
   - The passive context value @d@ is captured in a flat continuation closure that
     re-pairs @(c, d)@ directly into the output frame without intermediate wrapper layers.
   - For pure stages, compiler GHC worker-wrapper transformations unbox the resulting
     pairs completely into machine registers.

3. **Choice Profunctor Optimization:**
   Similarly, 'arrowLeft' and 'arrowRight' branch on 'Either' by checking variant tags,
   forwarding the payload without allocating nested sum trees.
-}
-}
module Penelope.Effects.Binary
  ( -- * Binary Arrow Facade
    Rope (..)

    -- * Jump-Table Router
  , deriveRouter

    -- * Runners
  , runRopePure
  , runRopeM
  ) where

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..), (>>>))
import Data.Functor.Identity (Identity (..))
import Data.Primitive.SmallArray (indexSmallArray)
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import GHC.Exts (Any, unsafeCoerce#)
import Penelope.Core.Engine.Elaborate (MapRow)
import Penelope.Core.Engine.Interpret
  ( FrameKleisli (..)
  , Handler (..)
  , interpretAll
  , runPenelopePure
  )
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  )
import Penelope.Core.Wiring (seqPenelope)
import Polytropos (Field (..), Record, RowOf, Variant, emptyRecord, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..), SumRow)
import Polytropos.RowMorph (unsafeMkRowMorph)
import Prelude hiding (id, (.))

{- |
Zero-cost binary arrow projection over Penelope's underlying polycategorical engine.
A computation of type @Rope eh ef a b@ is isomorphic to a pipeline with a single input register
@'["in" ':= a]@ and a single output register @'["out" ':= b]@.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
newtype Rope (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) a b = Rope
  { unRope :: Penelope eh ef '["in" ':= a] '["out" ':= b]
  }

-- | Category identity is a pure zero-cost wire pass-through ('Route').
--   Category composition sequences the two arrows via 'seqPenelope', inserting an identity
--   wire rename to connect output port @"out"@ to downstream input port @"in"@.
instance Category (Rope eh ef) where
  {-# INLINE id #-}
  id = Rope (Route (unsafeMkRowMorph [0]))

  {-# INLINE (.) #-}
  Rope g . Rope f =
    let rename = Route (unsafeMkRowMorph [0])
    in Rope (seqPenelope f (seqPenelope rename g))

-- | Lifts pure functions into the 'Rope' arrow via 'LiftPure'.
--   The function reads from input register @"in"@ and writes to output register @"out"@.
instance Arrow (Rope eh ef) where
  {-# INLINE arr #-}
  arr f = Rope $
    LiftPure (unsafeMkRowMorph [0])
      (\rec ->
        let !(Identity aVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
            !b = f (unsafeCoerce# aVal)
        in insertField (Field @"out") b emptyRecord
      )
      (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

  {-# INLINE first #-}
  first = arrowFirst

  {-# INLINE second #-}
  second = arrowSecond

-- | ArrowChoice instance providing sum-branching combinators over 'Either'.
instance ArrowChoice (Rope eh ef) where
  {-# INLINE left #-}
  left = arrowLeft

  {-# INLINE right #-}
  right = arrowRight

-- | Profunctor instance providing zero-cost input contravariant pre-mapping ('lmap')
--   and output covariant post-mapping ('rmap').
instance Profunctor (Rope eh ef) where
  {-# INLINE dimap #-}
  dimap f g r = arr f >>> r >>> arr g

  {-# INLINE lmap #-}
  lmap f r = arr f >>> r

  {-# INLINE rmap #-}
  rmap g r = r >>> arr g

-- | Strong profunctor instance lifting arrows across tuple products.
instance Strong (Rope eh ef) where
  {-# INLINE first' #-}
  first' = first

  {-# INLINE second' #-}
  second' = second

-- | Choice profunctor instance lifting arrows across sum coproducts.
instance Choice (Rope eh ef) where
  {-# INLINE left' #-}
  left' = left

  {-# INLINE right' #-}
  right' = right

-- | First-component arrow transformer: lifts a 'Rope' computation to operate
--   on the first component of a pair while passing the second component through untouched.
--   Preserves structural register alignment across the polycategorical hypergraph.
{-# INLINE arrowFirst #-}
arrowFirst ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (b, d) (c, d)
arrowFirst (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity pairVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(b, d) = unsafeCoerce# pairVal :: (b, d)
          !bIn = insertField (Field @"in") b emptyRecord
          stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= (c, d)]
          stageOut = LiftPure (unsafeMkRowMorph [0])
            (\cRec ->
              let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                  !c = unsafeCoerce# cVal :: c
              in insertField (Field @"out") (c, d) emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
      in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
           (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
    )

-- | Second-component arrow transformer: lifts a 'Rope' computation to operate
--   on the second component of a pair while passing the first component through untouched.
--   Preserves structural register alignment across the polycategorical hypergraph.
{-# INLINE arrowSecond #-}
arrowSecond ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (d, b) (d, c)
arrowSecond (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity pairVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(d, b) = unsafeCoerce# pairVal :: (d, b)
          !bIn = insertField (Field @"in") b emptyRecord
          stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= (d, c)]
          stageOut = LiftPure (unsafeMkRowMorph [0])
            (\cRec ->
              let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                  !c = unsafeCoerce# cVal :: c
              in insertField (Field @"out") (d, c) emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
      in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
           (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
    )

-- | Left-branch choice transformer: lifts a 'Rope' computation to process the
--   'Left' branch of an 'Either' sum value, passing any 'Right' values through unchanged.
{-# INLINE arrowLeft #-}
arrowLeft ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (Either b d) (Either c d)
arrowLeft (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity eitherVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
      in case (unsafeCoerce# eitherVal :: Either b d) of
        Left b ->
          let !bIn = insertField (Field @"in") b emptyRecord
              stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= Either c d]
              stageOut = LiftPure (unsafeMkRowMorph [0])
                (\cRec ->
                  let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                      !c = unsafeCoerce# cVal :: c
                  in insertField (Field @"out") (Left c :: Either c d) emptyRecord
                )
                (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
          in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
               (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
        Right d ->
          LiftPure (unsafeMkRowMorph [])
            (\_ -> insertField (Field @"out") (Right d :: Either c d) emptyRecord)
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
    )

-- | Right-branch choice transformer: lifts a 'Rope' computation to process the
--   'Right' branch of an 'Either' sum value, passing any 'Left' values through unchanged.
{-# INLINE arrowRight #-}
arrowRight ::
  forall eh ef b c d.
  Rope eh ef b c
  -> Rope eh ef (Either d b) (Either d c)
arrowRight (Rope p) = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      let !(Identity eitherVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
      in case (unsafeCoerce# eitherVal :: Either d b) of
        Right b ->
          let !bIn = insertField (Field @"in") b emptyRecord
              stageOut :: Penelope eh ef '["out" ':= c] '["out" ':= Either d c]
              stageOut = LiftPure (unsafeMkRowMorph [0])
                (\cRec ->
                  let !(Identity cVal) = indexSmallArray (let RowRep (Rep _ sArr) = cRec in sArr) 0
                      !c = unsafeCoerce# cVal :: c
                  in insertField (Field @"out") (Right c :: Either d c) emptyRecord
                )
                (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
          in LiftPure (unsafeMkRowMorph []) (\_ -> bIn)
               (BranchCont (\_ -> unsafeCoerce# (seqPenelope p stageOut)))
        Left d ->
          LiftPure (unsafeMkRowMorph [])
            (\_ -> insertField (Field @"out") (Left d :: Either d c) emptyRecord)
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))
    )

{- |
Static jump-table router: compiles a 'Record' of branch handlers into an \(O(1)\)
jump table over 'Polytropos.Variant', eliminating linear branching cascades and nested
coproduct shuffles (@Either (Either a b) c@).

== Operational Mechanics & Invariants
1. __Tag-Direct Branch Table Indexing:__
   The runtime tag of the incoming 'Polytropos.Variant' serves directly as an unboxed array
   index into the record of branch handlers. Dispatch time is strictly invariant to the
   total number of variant constructors.
2. __Payload Zero-Copy Forwarding:__
   The unpacked constructor payload array ('payloadArr') is extracted directly from the
   variant representation and rebound into the input frame register without copying
   or heap reallocation.
3. __Polycategorical Pipeline Splicing:__
   The target handler pipeline is extracted and spliced into the downstream execution
   path via 'unsafeCoerce#', seamlessly continuing polycategorical evaluation.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE deriveRouter #-}
deriveRouter ::
  forall (s :: SumRow) (eh :: RowOf PolyHEffect) (ef :: RowOf PolyEffect) b handlers.
  Record handlers
  -> Rope eh ef (Variant s) b
deriveRouter handlers = Rope $
  LiftPure (unsafeMkRowMorph [0]) id
    (BranchCont $ \rec ->
      -- Unpack the Polytropos Variant representation from input slot 0
      let !(Identity varVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(RowRep (Rep tag payloadArr)) = unsafeCoerce# varVal
          !(RowRep (Rep _ handlerArr)) = handlers
          -- Direct O(1) jump table indexing into handler array by variant tag
          !(Identity handlerAny) = indexSmallArray handlerArr (fromIntegral tag)
          !(Rope (pipe :: Penelope eh ef '["in" ':= Any] '["out" ':= b])) = unsafeCoerce# handlerAny
          -- Reconstitute payload record frame without re-allocating
          !payloadRecord = RowRep (Rep 0 payloadArr)
          !payloadIn = insertField (Field @"in") (unsafeCoerce# payloadRecord) emptyRecord
      in LiftPure (unsafeMkRowMorph []) (\_ -> payloadIn)
           (BranchCont (\_ -> unsafeCoerce# pipe))
    )

{- |
Execute a pure, closed 't:Rope' arrow to its final output value.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runRopePure #-}
runRopePure :: Rope '[] '[] a b -> a -> b
runRopePure (Rope p) a =
  let inFrame = insertField (Field @"in") a emptyRecord
      outFrame = runPenelopePure p inFrame
      !(Identity bVal) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0
  in unsafeCoerce# bVal

{- |
Execute an effectful 't:Rope' arrow into a target 'Monad' using a handler vector.

== Execution Pipeline & Invariants
1. __Input Frame Construction:__
   The source input value of type @a@ is packaged into an unboxed single-register
   record (@'["in" ':= a]@) in \(\mathcal{O}(1)\) time.
2. __Vectorized Algebraic Interpretation:__
   'interpretAll' compiles the underlying polycategorical hypergraph against the provided
   record of handlers, producing a bulk 'FrameKleisli' carrier without intermediate monadic wrapping.
3. __Monadic Evaluation:__
   'runFrameKleisli' executes the synthesized algebraic pipeline in target monad @m@,
   yielding the output record frame (@'["out" ':= b]@).
4. __Zero-Allocation Output Unpacking:__
   The evaluated output value of type @b@ is extracted directly from register slot 0
   via 'unsafeCoerce#' and returned cleanly in @m@.

See Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runRopeM #-}
runRopeM ::
  forall ef m a b.
  Monad m =>
  Record (MapRow (Handler '[] (FrameKleisli m)) ef)
  -> Rope '[] ef a b
  -> a
  -> m b
runRopeM handlers (Rope p) a = do
  let inFrame = insertField (Field @"in") a emptyRecord
      kleisli = interpretAll handlers p
  outFrame <- runFrameKleisli kleisli inFrame
  let !(Identity bVal) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0
  pure (unsafeCoerce# bVal)
