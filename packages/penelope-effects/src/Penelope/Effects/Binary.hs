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

instance Category (Rope eh ef) where
  {-# INLINE id #-}
  id = Rope (Route (unsafeMkRowMorph [0]))

  {-# INLINE (.) #-}
  Rope g . Rope f =
    let rename = Route (unsafeMkRowMorph [0])
    in Rope (seqPenelope f (seqPenelope rename g))

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

instance ArrowChoice (Rope eh ef) where
  {-# INLINE left #-}
  left = arrowLeft

  {-# INLINE right #-}
  right = arrowRight

instance Profunctor (Rope eh ef) where
  {-# INLINE dimap #-}
  dimap f g r = arr f >>> r >>> arr g

  {-# INLINE lmap #-}
  lmap f r = arr f >>> r

  {-# INLINE rmap #-}
  rmap g r = r >>> arr g

instance Strong (Rope eh ef) where
  {-# INLINE first' #-}
  first' = first

  {-# INLINE second' #-}
  second' = second

instance Choice (Rope eh ef) where
  {-# INLINE left' #-}
  left' = left

  {-# INLINE right' #-}
  right' = right

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
      let !(Identity varVal) = indexSmallArray (let RowRep (Rep _ sArr) = rec in sArr) 0
          !(RowRep (Rep tag payloadArr)) = unsafeCoerce# varVal
          !(RowRep (Rep _ handlerArr)) = handlers
          !(Identity handlerAny) = indexSmallArray handlerArr (fromIntegral tag)
          !(Rope (pipe :: Penelope eh ef '["in" ':= Any] '["out" ':= b])) = unsafeCoerce# handlerAny
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
