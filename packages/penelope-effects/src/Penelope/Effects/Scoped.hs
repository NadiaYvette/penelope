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
Module      : Penelope.Effects.Scoped
Description : Scoped reader and exception semantics with higher-order elaboration
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides scoped effect semantics (@Reader@/'Local' and @Error@/'Catch') guaranteed never
to leak environment handles or clobber lexical scopes across delimited coroutine
suspensions or non-deterministic branching resumptions.

See Note [Core Architecture] [ref:note-core-architecture],
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades], and
Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
module Penelope.Effects.Scoped
  ( -- * Scoped Reader Effect
    AskEff (..)
  , Local
  , ask
  , askReader
  , local
  , localAt
  , localElaborator
  , readerHandler
  , runReaderPure

    -- * Scoped Error Effect
  , ThrowEff (..)
  , Catch
  , throwError
  , throw
  , catchError
  , catchErrorAt
  , catchElaborator
  , errorHandler
  , runErrorPure
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (Any, Word (W#), Word#, eqWord#, isTrue#, unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Engine.Elaborate (Elaborator (..))
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyHEffect
  , Region (..)
  )
import Penelope.Core.Wiring (preComposeRoute, rmapFused, seqPenelope)
import Penelope.Effects.Unary
  ( Eff (..)
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)

{- |
First-order algebraic reader operation querying an environment of type @r@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data AskEff (r :: Type) (a :: Type) where
  Ask :: AskEff r r

{- |
First-order algebraic error operation terminating computation with an error of type @e@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data ThrowEff (e :: Type) (a :: Type) where
  Throw :: e -> ThrowEff e a

{- |
Higher-order scoped effect representing local lexical environment modification.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
data Local (r :: Type) :: PolyHEffect

{- |
Higher-order scoped effect representing dynamic error boundary interception.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
data Catch (e :: Type) :: PolyHEffect

{- |
Query the reader environment at a specified effect label.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE ask #-}
ask ::
  forall (l :: Symbol) r eh ef.
  ConstructorIndex l ef =>
  Eff eh ef r
ask = send @l Ask

{- |
Query the reader environment defaulting to effect label @"reader"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE askReader #-}
askReader ::
  forall r eh ef.
  ConstructorIndex "reader" ef =>
  Eff eh ef r
askReader = ask @"reader"

{- |
Throw an algebraic error at a specified effect label.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE throwError #-}
throwError ::
  forall (l :: Symbol) e eh ef a.
  ConstructorIndex l ef =>
  e
  -> Eff eh ef a
throwError e = send @l (Throw e)

{- |
Throw an algebraic error defaulting to effect label @"error"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE throw #-}
throw ::
  forall e eh ef a.
  ConstructorIndex "error" ef =>
  e
  -> Eff eh ef a
throw = throwError @"error"

{- |
Walk the 'Penelope' polycategorical graph, transforming every 'Ask' node targeted
by @targetTag#@ so its return continuation receives the locally modified environment.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
localWalk ::
  forall eh ef r ins outs.
  Word#
  -> (r -> r)
  -> Penelope eh ef ins outs
  -> Penelope eh ef ins outs
localWalk !targetTag# f pipe = go pipe
  where
    walkCont :: forall i o branchOuts. BranchCont (Penelope eh ef i o) branchOuts -> BranchCont (Penelope eh ef i o) branchOuts
    walkCont (BranchCont k) = BranchCont (\recFrame -> go (k recFrame))

    walkRegion :: forall sig. Region eh ef sig -> Region eh ef sig
    walkRegion (Region p) = Region (go p)

    go :: forall i o. Penelope eh ef i o -> Penelope eh ef i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap pureF (BranchCont k) ->
        LiftPure inMap pureF (BranchCont (\recFrame -> go (k recFrame)))

      OpF tag# inMap opAny conts
        | isTrue# (tag# `eqWord#` targetTag#) ->
            let wrapAskCont :: forall branchOuts. BranchCont (Penelope eh ef i o) branchOuts -> BranchCont (Penelope eh ef i o) branchOuts
                wrapAskCont (BranchCont k) = BranchCont $ \retFrame ->
                  let stage = LiftPure
                        (unsafeMkRowMorph [0])
                        (\recFrame ->
                          let !(RowRep (Rep _ arr)) = recFrame
                              !(Identity valAny) = indexSmallArray arr 0
                              !curEnv = unsafeCoerce# valAny :: r
                              !newEnv = f curEnv
                          in insertField (Field @"val") newEnv emptyRecord
                        )
                        (BranchCont (\_ -> go (k retFrame)))
                  in stage
            in OpF tag# inMap opAny (rmapFused wrapAskCont conts)
        | otherwise ->
            OpF tag# inMap opAny (rmapFused walkCont conts)

      OpH tag# inMap regions opAny conts ->
        OpH tag# inMap (rmapFused walkRegion regions) opAny (rmapFused walkCont conts)

      ScopeRow ehMorph efMorph subPipe ->
        ScopeRow ehMorph efMorph (unsafeCoerce# (go (unsafeCoerce# subPipe)))

{- |
Modify the reader environment within a scoped sub-computation at a custom effect label.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE localAt #-}
localAt ::
  forall (l :: Symbol) r eh ef a.
  ConstructorIndex l ef =>
  (r -> r)
  -> Eff eh ef a
  -> Eff eh ef a
localAt f (Eff pipe) =
  let !targetTag# = case constructorIndex @_ @l @ef of W# w# -> w#
  in Eff (localWalk targetTag# f pipe)

{- |
Modify the reader environment within a scoped sub-computation defaulting to label @"reader"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE local #-}
local ::
  forall r eh ef a.
  ConstructorIndex "reader" ef =>
  (r -> r)
  -> Eff eh ef a
  -> Eff eh ef a
local = localAt @"reader"

{- |
Walk the 'Penelope' polycategorical graph, intercepting every 'Throw' node targeted
by @targetTag#@ and substituting the handler continuation.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
catchWalk ::
  forall eh ef e a ins outs.
  Word#
  -> (e -> Eff eh ef a)
  -> Penelope eh ef ins outs
  -> Penelope eh ef ins outs
catchWalk !targetTag# handler pipe = go pipe
  where
    walkCont :: forall i o branchOuts. BranchCont (Penelope eh ef i o) branchOuts -> BranchCont (Penelope eh ef i o) branchOuts
    walkCont (BranchCont k) = BranchCont (\recFrame -> go (k recFrame))

    walkRegion :: forall sig. Region eh ef sig -> Region eh ef sig
    walkRegion (Region p) = Region (go p)

    go :: forall i o. Penelope eh ef i o -> Penelope eh ef i o
    go = \case
      Route morph ->
        Route morph

      LiftPure inMap pureF (BranchCont k) ->
        LiftPure inMap pureF (BranchCont (\recFrame -> go (k recFrame)))

      OpF tag# _inMap opAny _conts
        | isTrue# (tag# `eqWord#` targetTag#) ->
            let !(Throw err) = unsafeCoerce# opAny :: ThrowEff e Any
                !(Eff handlerPipe) = handler err
            in unsafeCoerce# handlerPipe
        | otherwise ->
            OpF tag# _inMap opAny (rmapFused walkCont _conts)

      OpH tag# inMap regions opAny conts ->
        OpH tag# inMap (rmapFused walkRegion regions) opAny (rmapFused walkCont conts)

      ScopeRow ehMorph efMorph subPipe ->
        ScopeRow ehMorph efMorph (unsafeCoerce# (go (unsafeCoerce# subPipe)))

{- |
Catch and recover from algebraic errors at a custom effect label.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE catchErrorAt #-}
catchErrorAt ::
  forall (l :: Symbol) e eh ef a.
  ConstructorIndex l ef =>
  Eff eh ef a
  -> (e -> Eff eh ef a)
  -> Eff eh ef a
catchErrorAt (Eff pipe) handler =
  let !targetTag# = case constructorIndex @_ @l @ef of W# w# -> w#
  in Eff (catchWalk targetTag# handler pipe)

{- |
Catch and recover from algebraic errors defaulting to effect label @"error"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE catchError #-}
catchError ::
  forall e eh ef a.
  ConstructorIndex "error" ef =>
  Eff eh ef a
  -> (e -> Eff eh ef a)
  -> Eff eh ef a
catchError = catchErrorAt @"error"

{- |
Elaborator for higher-order 'Local' effect unrolling into pure first-order @ef@ operations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
{-# INLINE localElaborator #-}
localElaborator ::
  forall (l :: Symbol) ef r.
  ConstructorIndex l ef =>
  Elaborator '[] ef (Local r)
localElaborator = Elaborator $ \inMap regs opAny kConts ->
  let !(Identity bodyRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
      !(Region bodyPipe) = unsafeCoerce# bodyRegAny
      !f = unsafeCoerce# opAny :: r -> r
      !targetTag# = case constructorIndex @_ @l @ef of W# w# -> w#
      !transformedBody = localWalk targetTag# f (unsafeCoerce# bodyPipe)
      !(Identity retContAny) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# retContAny
  in preComposeRoute inMap (unsafeCoerce# (transformedBody `seqPenelope` retK (unsafeCoerce# emptyRecord)))

{- |
Elaborator for higher-order 'Catch' effect unrolling into pure first-order @ef@ operations.

See Note [Two-Phase Weaving and Unweaving] [ref:note-two-phase-engine].
-}
{-# INLINE catchElaborator #-}
catchElaborator ::
  forall (l :: Symbol) ef e.
  ConstructorIndex l ef =>
  Elaborator '[] ef (Catch e)
catchElaborator = Elaborator $ \inMap regs _opAny kConts ->
  let !(Identity bodyRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
      !(Region bodyPipe) = unsafeCoerce# bodyRegAny
      !(Identity handlerRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 1
      !(Region handlerPipe) = unsafeCoerce# handlerRegAny
      !targetTag# = case constructorIndex @_ @l @ef of W# w# -> w#
      !transformedBody = catchWalk targetTag# (\_err -> Eff (unsafeCoerce# handlerPipe)) (unsafeCoerce# bodyPipe)
      !(Identity retContAny) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
      !(BranchCont retK) = unsafeCoerce# retContAny
  in preComposeRoute inMap (unsafeCoerce# (transformedBody `seqPenelope` retK (unsafeCoerce# emptyRecord)))

{- |
Construct an 'EffHandler' supplying an environment @r@ for 'AskEff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE readerHandler #-}
readerHandler ::
  forall r m.
  Monad m =>
  r
  -> EffHandler m (AskEff r)
readerHandler env = mkEffHandler $ \case
  Ask -> pure env

{- |
Execute a pure 't:Eff' computation supplying an environment @r@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runReaderPure #-}
runReaderPure ::
  forall r a.
  r
  -> Eff '[] '["reader" ':= UnaryEffect (AskEff r)] a
  -> a
runReaderPure env prog =
  let handlers = insertField (Field @"reader") (readerHandler env) emptyRecord
      Identity res = runEffM handlers prog
  in res

{- |
Construct an 'EffHandler' interpreting 'ThrowEff' under the 'Either e' error monad.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE errorHandler #-}
errorHandler ::
  forall e.
  EffHandler (Either e) (ThrowEff e)
errorHandler = mkEffHandler $ \(Throw err) -> Left err

{- |
Execute a pure 't:Eff' computation catching errors into 'Either e a'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runErrorPure #-}
runErrorPure ::
  forall e a.
  Eff '[] '["error" ':= UnaryEffect (ThrowEff e)] a
  -> Either e a
runErrorPure prog =
  let handlers = insertField (Field @"error") errorHandler emptyRecord
  in runEffM handlers prog
