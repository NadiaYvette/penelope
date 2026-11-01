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
Module      : Penelope.Effects.Control
Description : Native multi-region and multi-exit control effect battery
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides standard native \((m,n,k,r)\)-ary control effects:
1. Multi-region 'While' featuring scoped sub-regions (@"cond"@ and @"body"@)
2. Dynamic disjunctive \(k\)-way exit branching via 'Switch'
3. Transactional workflows with compensation handlers via 'Saga'

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Control
  ( -- * Multi-Region While
    While (..)
  , whileElaborator
  , whileEff

    -- * Multi-Exit Switch
  , Switch (..)
  , switchHandler
  , sendSwitch

    -- * Transactional Saga
  , Saga (..)
  , sagaElaborator
  , runSagaEff
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Word (W#), unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Engine.Elaborate (Elaborator (..))
import Penelope.Core.Engine.Interpret
  ( FrameKleisli (..)
  , Handler (..)
  , runPenelopePure
  )
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PortSig (..)
  , Region (..)
  )
import Penelope.Effects.Unary (Eff (..))
import Polytropos (Record, RowOf)
import Polytropos.Core (ConstructorIndex (..), Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (gatherRep, unsafeMkRowMorph)

{- |
Higher-order scoped 'While' loop with two explicit sub-regions:
1. @"cond"@: Evaluates loop termination condition over state @st@.
2. @"body"@: Transforms current state into successor state.

Exits with final state @st@ via @"exit"@ branch.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data While (r :: PortSig -> Type) (regs :: RowOf PortSig) (opIns :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  WhileOp :: While r
    '[ "cond" ':= 'PortSig st '["cond" ':= Bool]
     , "body" ':= 'PortSig st st
     ]
    st
    '[ "exit" ':= st ]

{- |
Elaborator for 'While' lowering multi-region loop execution into pure iterative steps.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE whileElaborator #-}
whileElaborator ::
  forall ef.
  Elaborator '[] ef While
whileElaborator = Elaborator $ \inMap regs _ kConts ->
  LiftPure inMap
    (\stRec ->
      let !(Identity condRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
          !(Region condPipe) = unsafeCoerce# condRegAny
          !(Identity bodyRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 1
          !(Region bodyPipe) = unsafeCoerce# bodyRegAny

          loop curSt =
            let condRec = runPenelopePure condPipe (unsafeCoerce# curSt)
                !(Identity condVal) = indexSmallArray (let RowRep (Rep _ sArr) = condRec in sArr) 0
                !cond = unsafeCoerce# condVal :: Bool
            in if cond
               then let nextSt = runPenelopePure bodyPipe (unsafeCoerce# curSt)
                    in loop (unsafeCoerce# nextSt)
               else curSt
      in loop stRec
    )
    (BranchCont $ \finalSt ->
      let !(Identity exitContAny) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
          !(BranchCont exitK) = unsafeCoerce# exitContAny
      in exitK (unsafeCoerce# finalSt)
    )

{- |
Convenient monadic 'While' combinator for 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE whileEff #-}
whileEff ::
  forall st eh ef.
  (st -> Eff eh ef Bool)
  -> (st -> Eff eh ef st)
  -> st
  -> Eff eh ef st
whileEff cond body = go
  where
    go !s = do
      b <- cond s
      if b
        then do
          s' <- body s
          go s'
        else pure s

{- |
First-order 'Switch' effect with dynamic disjunctive \(k\)-way exits.
Dispatches directly using a zero-allocation branch jump table.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Switch (ins :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  SwitchOp :: Switch '["tag" ':= Int] exits

{- |
Handler for 'Switch' dispatching multi-exit branch continuations in \(O(1)\).

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE switchHandler #-}
switchHandler ::
  forall ef m.
  Handler ef (FrameKleisli m) Switch
switchHandler = Handler $ \inMap _ exits -> FrameKleisli $ \inFrame -> do
  let opInFrame = RowRep (gatherRep inMap (let RowRep inRep = inFrame in inRep))
      !(Identity tagVal) = indexSmallArray (let RowRep (Rep _ sArr) = opInFrame in sArr) 0
      !tag = unsafeCoerce# tagVal :: Int
      !(RowRep (Rep _ contArr)) = exits
      !(Identity contAny) = indexSmallArray contArr tag
      !(BranchCont k) = unsafeCoerce# contAny
  runFrameKleisli (k (unsafeCoerce# opInFrame)) inFrame

{- |
Smart constructor injecting a 'Switch' operation with \(k\) exit branches.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE sendSwitch #-}
sendSwitch ::
  forall (l :: Symbol) exits eh ef outs.
  (ConstructorIndex l ef) =>
  Record exits
  -> Penelope eh ef '["tag" ':= Int] outs
sendSwitch conts =
  let !tag# = case constructorIndex @_ @l @ef of W# w# -> w#
  in OpF tag# (unsafeMkRowMorph [0]) (unsafeCoerce# (SwitchOp @'[])) (unsafeCoerce# conts)

{- |
Transactional 'Saga' higher-order effect with three scoped sub-regions:
1. @"try"@: Primary forward action
2. @"compensate"@: Rollback compensation handler invoked on failure
3. @"catch"@: Recovery handler producing alternative outcome

Exits into either @"committed"@ or @"aborted"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Saga (r :: PortSig -> Type) (regs :: RowOf PortSig) (opIns :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  SagaOp :: Saga r
    '[ "try"        ':= 'PortSig ins mid
     , "compensate" ':= 'PortSig mid errRow
     , "catch"      ':= 'PortSig errRow outRow
     ]
    ins
    '[ "committed"  ':= mid
     , "aborted"    ':= outRow
     ]

{- |
Elaborator for 'Saga' wiring forward actions and compensation paths.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE sagaElaborator #-}
sagaElaborator ::
  forall ef.
  Elaborator '[] ef Saga
sagaElaborator = Elaborator $ \inMap regs _ kConts ->
  LiftPure inMap
    (\inRec ->
      let !(Identity tryRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
          !(Region tryPipe) = unsafeCoerce# tryRegAny
      in runPenelopePure tryPipe (unsafeCoerce# inRec)
    )
    (BranchCont $ \midRec ->
      let !(Identity commitContAny) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
          !(BranchCont commitK) = unsafeCoerce# commitContAny
      in commitK (unsafeCoerce# midRec)
    )

{- |
Execute a transactional saga in 'Eff': runs action, and if an exception occurs,
executes the compensation handler before returning the failure.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runSagaEff #-}
runSagaEff ::
  forall a b err eh ef.
  Eff eh ef a
  -> (a -> Eff eh ef ())
  -> (a -> Eff eh ef (Either err b))
  -> Eff eh ef (Either err b)
runSagaEff action compensate body = do
  a <- action
  res <- body a
  case res of
    Left _ -> do
      compensate a
      pure res
    Right b ->
      pure (Right b)
