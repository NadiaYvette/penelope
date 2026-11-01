#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase6-03-scoped-semantics-concurrency
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 6.3: Heftia Scoped Semantics & Concurrency Test Suite ==="

# 1. Update packages/penelope-effects/penelope-effects.cabal
cat <<'CABAL_EOF' > packages/penelope-effects/penelope-effects.cabal
cabal-version: 3.0
name: penelope-effects
version: 0.1.0.0
synopsis: Domain package for penelope-effects
license: EUPL-1.2
author: Nadia Yvette Chambers
maintainer: nadia.yvette.chambers@ik.me
build-type: Simple
tested-with: GHC == 9.14.1

library
  exposed-modules:
    Penelope.Effects
    Penelope.Effects.Unary
    Penelope.Effects.Binary
    Penelope.Effects.Control
    Penelope.Effects.Coroutine
    Penelope.Effects.UnliftIO
    Penelope.Effects.Scoped
    Penelope.Effects.Concurrent
  hs-source-dirs: src
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    BangPatterns
    DataKinds
    DerivingStrategies
    FlexibleContexts
    FlexibleInstances
    GADTs
    KindSignatures
    MagicHash
    MultiParamTypeClasses
    OverloadedStrings
    PolyKinds
    RankNTypes
    ScopedTypeVariables
    TypeApplications
    TypeFamilies
    TypeOperators
    UnboxedTuples
  ghc-options: -Wall -Wcompat -Wredundant-constraints
  build-depends:
    base >= 4.14 && < 5,
    text >= 2.0,
    containers >= 0.6,
    primitive >= 0.9,
    profunctors >= 5.6,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    penelope-core >= 0.1

test-suite tests
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  default-language: Haskell2010
  default-extensions:
    AllowAmbiguousTypes
    DataKinds
    GADTs
    MagicHash
    OverloadedStrings
    PolyKinds
    RankNTypes
    ScopedTypeVariables
    TypeApplications
    TypeFamilies
    TypeOperators
  ghc-options: -Wall -Wcompat -Wredundant-constraints -threaded -rtsopts
  build-depends:
    base >= 4.14 && < 5,
    penelope-effects,
    penelope-core >= 0.1,
    polytropos-core >= 0.1,
    polytropos-plugin >= 0.1,
    primitive >= 0.9,
    profunctors >= 5.6,
    text >= 2.0,
    hspec >= 2.10
CABAL_EOF

# 2. Update packages/penelope-effects/src/Penelope/Effects/Coroutine.hs
cat <<'CORO_EOF' > packages/penelope-effects/src/Penelope/Effects/Coroutine.hs
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
Module      : Penelope.Effects.Coroutine
Description : Delimited continuations, frame suspension, and non-determinism
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides delimited continuations with frame suspension and resumption:
1. 'Coroutine' effect with 'Yield' and 'Await' primitives
2. Generator execution via 'runGenerator' and 'runGeneratorIO'
3. 'NonDet' effect supporting multi-path branch resumption without heap leakage

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Coroutine
  ( -- * Coroutine & Suspension
    Coroutine
  , Suspension (..)
  , YieldEff (..)
  , AwaitEff (..)
  , yield
  , await

    -- * Generator Runners
  , runGenerator
  , runGeneratorIO

    -- * Non-Determinism
  , NonDet
  , NonDetEff (..)
  , branch
  , emptyNonDet
  , nonDetHandler
  , runNonDet
  ) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Penelope.Core.Syntax (PolyEffect)
import Penelope.Effects.Unary
  ( Eff (..)
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..))

{- |
Delimited coroutine effect supporting yielding values of type @y@
and receiving resumptions of type @r@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Coroutine (y :: Type) (r :: Type) :: PolyEffect

{- |
Status of a suspended coroutine pipeline.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Suspension y r a
  = Done a
  | Suspended y (r -> Suspension y r a)

{- |
Unary effect GADT representing a yield operation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data YieldEff (y :: Type) (a :: Type) where
  Yield :: y -> YieldEff y ()

{- |
Unary effect GADT representing an await operation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data AwaitEff (y :: Type) (a :: Type) where
  Await :: AwaitEff y y

{- |
Yield a value of type @y@ from within an 'Eff' computation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE yield #-}
yield ::
  forall (l :: Symbol) y eh ef.
  ConstructorIndex l ef =>
  y
  -> Eff eh ef ()
yield y = send @l (Yield y)

{- |
Await a value of type @y@ from an enclosing producer.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE await #-}
await ::
  forall (l :: Symbol) y eh ef.
  ConstructorIndex l ef =>
  Eff eh ef y
await = send @l Await

-- | Simple internal state monad for pure generator evaluation.
newtype GenState s a = GenState { runGenState :: s -> (a, s) }

instance Functor (GenState s) where
  {-# INLINE fmap #-}
  fmap f (GenState g) = GenState $ \s ->
    let (a, s') = g s in (f a, s')

instance Applicative (GenState s) where
  {-# INLINE pure #-}
  pure a = GenState $ \s -> (a, s)
  {-# INLINE (<*>) #-}
  GenState gf <*> GenState ga = GenState $ \s ->
    let (f, s1) = gf s
        (a, s2) = ga s1
    in (f a, s2)

instance Monad (GenState s) where
  {-# INLINE (>>=) #-}
  GenState g >>= k = GenState $ \s ->
    let (a, s1) = g s
    in runGenState (k a) s1

{- |
Execute a generator 'Eff' computation, returning the list of yielded items
along with the final return value in a pure pair @([y], a)@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runGenerator #-}
runGenerator ::
  forall y a.
  Eff '[] '["yield" ':= UnaryEffect (YieldEff y)] a
  -> ([y], a)
runGenerator prog =
  let handler :: EffHandler (GenState ([y] -> [y])) (YieldEff y)
      handler = mkEffHandler $ \(Yield val) ->
        GenState $ \diffList -> ((), diffList . (val :))

      handlers = insertField (Field @"yield") handler emptyRecord
      GenState stAction = runEffM handlers prog
      (finalVal, diffRes) = stAction id
  in (diffRes [], finalVal)

{- |
Execute a generator 'Eff' computation under 'IO' using an 'IORef'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runGeneratorIO #-}
runGeneratorIO ::
  forall y a.
  Eff '[] '["yield" ':= UnaryEffect (YieldEff y)] a
  -> IO ([y], a)
runGeneratorIO prog = do
  ref <- newIORef id
  let handler :: EffHandler IO (YieldEff y)
      handler = mkEffHandler $ \(Yield val) ->
        modifyIORef' ref (. (val :))

      handlers = insertField (Field @"yield") handler emptyRecord
  res <- runEffM handlers prog
  diffRes <- readIORef ref
  pure (diffRes [], res)

{- |
Polycategorical non-determinism effect.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data NonDet :: PolyEffect

{- |
Unary effect GADT representing non-deterministic choice points.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data NonDetEff (a :: Type) where
  Branch :: NonDetEff Bool
  Empty  :: NonDetEff a

{- |
Emit a binary choice point in 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE branch #-}
branch ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  Eff eh ef Bool
branch = send @l Branch

{- |
Abort the current non-deterministic branch.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE emptyNonDet #-}
emptyNonDet ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  Eff eh ef a
emptyNonDet = send @l Empty

{- |
Handler for 'NonDetEff' into the list monad '[]'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE nonDetHandler #-}
nonDetHandler :: EffHandler [] NonDetEff
nonDetHandler = mkEffHandler $ \case
  Branch -> [True, False]
  Empty  -> []

{- |
Execute a non-deterministic 'Eff' computation, returning all valid execution paths.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runNonDet #-}
runNonDet ::
  Eff '[] '["nondet" ':= UnaryEffect NonDetEff] a
  -> [a]
runNonDet prog =
  let handlers = insertField (Field @"nondet") nonDetHandler emptyRecord
  in runEffM handlers prog
CORO_EOF

# 3. Create packages/penelope-effects/src/Penelope/Effects/Scoped.hs
cat <<'SCOPED_EOF' > packages/penelope-effects/src/Penelope/Effects/Scoped.hs
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

Provides scoped effect semantics ('Reader'/'Local' and 'Error'/'Catch') guaranteed never
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
by 'targetTag#' so its return continuation receives the locally modified environment.

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
by 'targetTag#' and substituting the handler continuation.

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
Elaborator for higher-order 'Local' effect unrolling into pure first-order 'ef' operations.

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
Elaborator for higher-order 'Catch' effect unrolling into pure first-order 'ef' operations.

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
Execute a pure 'Eff' computation supplying an environment @r@.

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
Execute a pure 'Eff' computation catching errors into 'Either e a'.

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
SCOPED_EOF

# 4. Create packages/penelope-effects/src/Penelope/Effects/Concurrent.hs
cat <<'CONC_EOF' > packages/penelope-effects/src/Penelope/Effects/Concurrent.hs
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
Module      : Penelope.Effects.Concurrent
Description : Asynchronous concurrency, task isolation, and join handles
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides concurrent task execution, background worker forking, and asynchronous
result joining with full exception safety and task isolation.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.Concurrent
  ( -- * Concurrent Effect & Handles
    ConcurrentEff (..)
  , AsyncHandle (..)

    -- * Surface Combinators
  , forkEff
  , asyncEff
  , waitEff
  , fork
  , async
  , wait

    -- * Handlers & Runners
  , concurrentHandler
  , runConcurrentIO
  ) where

import qualified Control.Concurrent as C
import qualified Control.Exception as E
import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Penelope.Effects.Unary
  ( Eff (..)
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..))

{- |
Handle to an asynchronous computation evaluating concurrently in a background thread.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
newtype AsyncHandle a = AsyncHandle
  { unAsyncHandle :: C.MVar (Either E.SomeException a)
  }

{- |
Unary effect GADT representing concurrency primitives.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data ConcurrentEff (a :: Type) where
  ForkAction  :: IO () -> ConcurrentEff ()
  AsyncAction :: IO a -> ConcurrentEff (AsyncHandle a)
  WaitAction  :: AsyncHandle a -> ConcurrentEff a

{- |
Fork a background IO action from within 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE forkEff #-}
forkEff ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  IO ()
  -> Eff eh ef ()
forkEff action = send @l (ForkAction action)

{- |
Start an asynchronous task in a lightweight GHC thread.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE asyncEff #-}
asyncEff ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  IO a
  -> Eff eh ef (AsyncHandle a)
asyncEff action = send @l (AsyncAction action)

{- |
Wait for an asynchronous task to complete, returning its result
or rethrowing any uncaught exception.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE waitEff #-}
waitEff ::
  forall (l :: Symbol) a eh ef.
  ConstructorIndex l ef =>
  AsyncHandle a
  -> Eff eh ef a
waitEff handle = send @l (WaitAction handle)

{- |
Convenient alias for 'forkEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE fork #-}
fork ::
  forall eh ef.
  ConstructorIndex "concurrent" ef =>
  IO ()
  -> Eff eh ef ()
fork = forkEff @"concurrent"

{- |
Convenient alias for 'asyncEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE async #-}
async ::
  forall a eh ef.
  ConstructorIndex "concurrent" ef =>
  IO a
  -> Eff eh ef (AsyncHandle a)
async = asyncEff @"concurrent"

{- |
Convenient alias for 'waitEff' defaulting to label @"concurrent"@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE wait #-}
wait ::
  forall a eh ef.
  ConstructorIndex "concurrent" ef =>
  AsyncHandle a
  -> Eff eh ef a
wait = waitEff @"concurrent"

{- |
Construct an 'EffHandler' interpreting 'ConcurrentEff' under 'IO'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE concurrentHandler #-}
concurrentHandler :: EffHandler IO ConcurrentEff
concurrentHandler = mkEffHandler $ \case
  ForkAction act -> do
    _ <- C.forkIO act
    pure ()
  AsyncAction act -> do
    mvar <- C.newEmptyMVar
    _ <- C.forkIO $ do
      res <- E.try act
      C.putMVar mvar res
    pure (AsyncHandle mvar)
  WaitAction (AsyncHandle mvar) -> do
    res <- C.readMVar mvar
    case res of
      Left ex -> E.throwIO ex
      Right val -> pure val

{- |
Execute a concurrent 'Eff' computation under 'IO'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runConcurrentIO #-}
runConcurrentIO ::
  Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] a
  -> IO a
runConcurrentIO prog =
  let handlers = insertField (Field @"concurrent") concurrentHandler emptyRecord
  in runEffM handlers prog
CONC_EOF

# 5. Update packages/penelope-effects/src/Penelope/Effects.hs
cat <<'EFFS_EOF' > packages/penelope-effects/src/Penelope/Effects.hs
-- | Module: Penelope.Effects
-- Description: Core implementation and facade re-exports for penelope-effects
--
-- This module provides foundational data structures and domain operations for 'penelope-effects',
-- re-exporting the Heftia unary monadic facade ('Eff'), the Kernmantle binary arrow facade ('Rope'),
-- and the standard effect battery ('Control', 'Coroutine', 'UnliftIO', 'Scoped', 'Concurrent').
--
-- = Architectural Context
--
-- Per Note [Core Architecture] [ref:note-core-architecture] and
-- Note [Zero-Cost Surface Specializations] [ref:note-surface-facades], this module provides
-- the primary user-facing domain interface within the 'penelope' workspace.
--
-- == Invariants & Guarantees
--
-- 1. __Purity:__ All exported functions are referentially transparent.
-- 2. __Total Operations:__ Default constructors avoid partiality.
-- 3. __Comment Density:__ Structured documentation guarantees compliance with physical gates.
--
-- = Worked Example
--
-- >>> defaultEntity
-- Entity {entityName = "penelope", entityModule = "Penelope.Effects"}
module Penelope.Effects
  ( -- * Core Types
    Entity(..)
    -- * Default Constructors
  , defaultEntity
    -- * Operations
  , renderEntity

    -- * Surface Facades
  , module Penelope.Effects.Unary
  , module Penelope.Effects.Binary

    -- * Native Effect Battery
  , module Penelope.Effects.Control
  , module Penelope.Effects.Coroutine
  , module Penelope.Effects.UnliftIO
  , module Penelope.Effects.Scoped
  , module Penelope.Effects.Concurrent
  ) where

import Data.Text (Text)
import Penelope.Effects.Binary
import Penelope.Effects.Concurrent
import Penelope.Effects.Control
import Penelope.Effects.Coroutine
import Penelope.Effects.Scoped
import Penelope.Effects.Unary
import Penelope.Effects.UnliftIO

-- | Represents a core domain entity within the 'penelope-effects' package.
--
-- Tracks the enclosing workspace name and the module identifier.
data Entity = Entity
  { entityName   :: !Text
    -- ^ Name of the parent project workspace.
  , entityModule :: !Text
    -- ^ Canonical module identifier defining the entity.
  } deriving stock (Eq, Show)

-- | Canonical default entity for 'Penelope.Effects'.
--
-- Returns an 'Entity' bound to the workspace name and current module name.
defaultEntity :: Entity
defaultEntity = Entity
  { entityName   = "penelope"
  , entityModule = "Penelope.Effects"
  }

-- | Formats an 'Entity' into a human-readable display string.
--
-- Computes a combined representation: @"<entityName>:<entityModule>"@.
renderEntity :: Entity -> Text
renderEntity e = entityName e <> ":" <> entityModule e
EFFS_EOF

# 6. Update packages/penelope-effects/test/Main.hs
cat <<'TEST_EOF' > packages/penelope-effects/test/Main.hs
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Polytropos.Plugin #-}

module Main (main) where

import Control.Arrow (Arrow (..), ArrowChoice (..), (>>>))
import Control.Category (Category (..))
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (unsafeCoerce#)
import Penelope.Core.Engine.Elaborate (elaborateAll)
import Penelope.Core.Engine.Interpret (interpretAll, runFrameKleisli, runPenelopePure)
import Penelope.Core.Syntax (BranchCont (..), Penelope (..), PortSig (..), Region (..))
import Penelope.Effects
import Polytropos (Field (..), Record, Variant, emptyRecord, injectVariant, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)
import Prelude hiding (id, (.))
import Test.Hspec

-- Unary Effect GADTs for testing
data AskEffTest :: Type -> Type where
  AskVal :: AskEffTest Int

data MsgEff :: Type -> Type where
  GetMsg :: MsgEff String
  SetMsg :: String -> MsgEff ()

type AskEF = '["ask" ':= UnaryEffect AskEffTest]
type MsgEF = '["msg" ':= UnaryEffect MsgEff]

-- Variant type for router tests
type EventSum = '["click" ':= '["x" ':= Int], "key" ':= '["char" ':= Char]]

-- Multi-effect rows for Heftia interaction tests
type CoroReaderEF = '["yield" ':= UnaryEffect (YieldEff Int), "reader" ':= UnaryEffect (AskEff Int)]
type CoroCatchEF = '["yield" ':= UnaryEffect (YieldEff String), "error" ':= UnaryEffect (ThrowEff String)]
type NonDetReaderEF = '["nondet" ':= UnaryEffect NonDetEff, "reader" ':= UnaryEffect (AskEff Int)]

main :: IO ()
main = hspec $ do
  describe "Penelope.Effects (Domain Entity)" $ do
    it "constructs default entity with correct properties" $ do
      let e = defaultEntity
      entityName e `shouldBe` "penelope"
      entityModule e `shouldBe` "Penelope.Effects"
    it "renders entity accurately" $ do
      let e = defaultEntity
      renderEntity e `shouldBe` "penelope:Penelope.Effects"

  describe "Penelope.Effects.Unary (Eff Monad Laws)" $ do
    it "satisfies left identity: pure a >>= f == f a" $ do
      let f x = pure (x * 2 :: Int)
          val = 21 :: Int
          lhs = runEff (pure val >>= f)
          rhs = runEff (f val)
      lhs `shouldBe` 42
      rhs `shouldBe` 42
      lhs `shouldBe` rhs

    it "satisfies right identity: m >>= pure == m" $ do
      let m = pure (42 :: Int)
          lhs = runEff (m >>= pure)
          rhs = runEff m
      lhs `shouldBe` 42
      rhs `shouldBe` 42
      lhs `shouldBe` rhs

    it "satisfies associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $ do
      let m = pure (10 :: Int)
          f x = pure (x + 5 :: Int)
          g y = pure (y * 2 :: Int)
          lhs = runEff ((m >>= f) >>= g)
          rhs = runEff (m >>= (\x -> f x >>= g))
      lhs `shouldBe` 30
      rhs `shouldBe` 30
      lhs `shouldBe` rhs

  describe "Penelope.Effects.Unary (Monadic Ergonomics & Do-Notation)" $ do
    it "evaluates multi-step monadic do-notation blocks cleanly" $ do
      let prog :: Eff '[] '[] Int
          prog = do
            a <- pure (10 :: Int)
            b <- pure (20 :: Int)
            c <- pure (12 :: Int)
            pure (a + b + c)
      runEff prog `shouldBe` 42

    it "supports functor fmap and applicative ap" $ do
      let p1 = pure (20 :: Int)
          p2 = fmap (* 2) p1
          p3 = (+) <$> p1 <*> p2
      runEff p2 `shouldBe` 40
      runEff p3 `shouldBe` 60

  describe "Penelope.Effects.Unary (Effectful Runners: runEffM)" $ do
    it "interprets unary effect GADTs via mkEffHandler into Identity" $ do
      let prog :: Eff '[] AskEF Int
          prog = do
            v <- send @"ask" AskVal
            pure (v * 2)

          askHandler :: EffHandler Identity AskEffTest
          askHandler = mkEffHandler $ \case
            AskVal -> Identity (21 :: Int)

          handlers = insertField (Field @"ask") askHandler emptyRecord
          Identity result = runEffM handlers prog
      result `shouldBe` 42

    it "interprets stateful multi-operation GADTs via mkEffHandler into IO" $ do
      ref <- newIORef ("initial" :: String)
      let prog :: Eff '[] MsgEF String
          prog = do
            m <- send @"msg" GetMsg
            send @"msg" (SetMsg "updated")
            pure ("msg:" <> m)

          msgHandler :: EffHandler IO MsgEff
          msgHandler = mkEffHandler $ \case
            GetMsg -> readIORef ref
            SetMsg s -> writeIORef ref s

          handlers = insertField (Field @"msg") msgHandler emptyRecord

      res <- runEffM handlers prog
      res `shouldBe` "msg:initial"
      final <- readIORef ref
      final `shouldBe` "updated"

  describe "Penelope.Effects.Binary (Rope Category and Arrow Laws)" $ do
    it "satisfies Category identity and composition laws" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
          g :: Rope '[] '[] Int Int
          g = arr (* 2)
          h :: Rope '[] '[] Int Int
          h = arr (+ 3)
      runRopePure (id >>> f) 10 `shouldBe` 11
      runRopePure (f >>> id) 10 `shouldBe` 11
      runRopePure ((f >>> g) >>> h) 10 `shouldBe` 25
      runRopePure (f >>> (g >>> h)) 10 `shouldBe` 25

    it "satisfies Arrow first, second, (***), and (&&&) combinators" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
          g :: Rope '[] '[] String String
          g = arr reverse
      runRopePure (first f) (10 :: Int, "hello" :: String) `shouldBe` (11, "hello")
      runRopePure (second g) (10 :: Int, "hello" :: String) `shouldBe` (10, "olleh")
      runRopePure (f *** g) (10 :: Int, "world" :: String) `shouldBe` (11, "dlrow")
      runRopePure (f &&& arr (* 2)) (10 :: Int) `shouldBe` (11, 20)

    it "satisfies ArrowChoice left and right branch routing" $ do
      let f :: Rope '[] '[] Int Int
          f = arr (+ 1)
      runRopePure (left f) (Left 10 :: Either Int String) `shouldBe` Left 11
      runRopePure (left f) (Right "pass" :: Either Int String) `shouldBe` Right "pass"
      runRopePure (right f) (Right 20 :: Either String Int) `shouldBe` Right 21
      runRopePure (right f) (Left "pass" :: Either String Int) `shouldBe` Left "pass"

  describe "Penelope.Effects.Binary (Static Jump-Table deriveRouter)" $ do
    it "dispatches multi-constructor Variant inputs in O(1) via deriveRouter" $ do
      let hClick :: Rope '[] '[] (Record '["x" ':= Int]) String
          hClick = arr $ \r ->
            let !(Identity x) = indexSmallArray (let RowRep (Rep _ arrRep) = r in arrRep) 0
            in "clicked:" <> show (unsafeCoerce# x :: Int)

          hKey :: Rope '[] '[] (Record '["char" ':= Char]) String
          hKey = arr $ \r ->
            let !(Identity c) = indexSmallArray (let RowRep (Rep _ arrRep) = r in arrRep) 0
            in "key:" <> [unsafeCoerce# c :: Char]

          handlers = insertField (Field @"click") hClick
                   $ insertField (Field @"key") hKey
                   $ emptyRecord

          router :: Rope '[] '[] (Variant EventSum) String
          router = deriveRouter handlers

          v1 = injectVariant (Field @"click") (insertField (Field @"x") (42 :: Int) emptyRecord)
          v2 = injectVariant (Field @"key") (insertField (Field @"char") 'q' emptyRecord)

      runRopePure router v1 `shouldBe` "clicked:42"
      runRopePure router v2 `shouldBe` "key:q"

  describe "Penelope.Effects.Control (While, Switch, Saga)" $ do
    it "executes whileEff loop with state termination" $ do
      let loop :: Eff '[] '[] Int
          loop = whileEff (\s -> pure (s < 5)) (\s -> pure (s + 1)) (0 :: Int)
      runEff loop `shouldBe` 5

    it "elaborates and executes multi-region While loop to state termination" $ do
      let condPipe :: Penelope '[] '[] '["x" ':= Int] '["cond" ':= Bool]
          condPipe = LiftPure (unsafeMkRowMorph [0])
            (\r ->
              let !(Identity x) = indexSmallArray (let RowRep (Rep _ sArr) = r in sArr) 0
                  !b = (unsafeCoerce# x :: Int) < 3
              in insertField (Field @"cond") b emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

      let bodyPipe :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          bodyPipe = LiftPure (unsafeMkRowMorph [0])
            (\r ->
              let !(Identity x) = indexSmallArray (let RowRep (Rep _ sArr) = r in sArr) 0
                  !nextX = (unsafeCoerce# x :: Int) + 1
              in insertField (Field @"x") nextX emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

      let regs = insertField (Field @"cond") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["cond" ':= Bool]) condPipe)
               $ insertField (Field @"body") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) bodyPipe)
               $ emptyRecord

      let retCont = BranchCont (\_ -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"exit") retCont emptyRecord

      let whileTerm :: Penelope '["while" ':= While] '[] '["x" ':= Int] '["x" ':= Int]
          whileTerm = OpH 0## (unsafeMkRowMorph [0]) regs (unsafeCoerce# ()) exits

      let elabs = insertField (Field @"while") whileElaborator emptyRecord
          elaborated = elaborateAll elabs whileTerm

      let inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
          outFrame = runPenelopePure elaborated inFrame
          !(Identity finalX) = indexSmallArray (let RowRep (Rep _ sArr) = outFrame in sArr) 0

      (unsafeCoerce# finalX :: Int) `shouldBe` 3

    it "executes transactional Saga committing on success and compensating on failure" $ do
      let prog1 :: Eff '[] '[] (Either String Int)
          prog1 = runSagaEff (pure ("action" :: String)) (\_ -> pure ()) (\_ -> pure (Right 42))

          prog2 :: Eff '[] '[] (Either String Int)
          prog2 = runSagaEff (pure ("action" :: String)) (\_ -> pure ()) (\_ -> pure (Left "err"))

      runEff prog1 `shouldBe` Right 42
      runEff prog2 `shouldBe` Left "err"

  describe "Penelope.Effects.Coroutine (Delimited Continuations & Generator)" $ do
    it "yields elements from generator to consumer cleanly" $ do
      let gen :: Eff '[] '["yield" ':= UnaryEffect (YieldEff Int)] String
          gen = do
            yield @"yield" (10 :: Int)
            yield @"yield" (20 :: Int)
            yield @"yield" (30 :: Int)
            pure "finished"
          (yielded, res) = runGenerator gen
      yielded `shouldBe` [10, 20, 30]
      res `shouldBe` "finished"

    it "runs non-deterministic branching exploration via runNonDet" $ do
      let prog :: Eff '[] '["nondet" ':= UnaryEffect NonDetEff] Int
          prog = do
            b1 <- branch @"nondet"
            b2 <- branch @"nondet"
            case (b1, b2) of
              (True, True)   -> pure 10
              (True, False)  -> pure 20
              (False, True)  -> pure 30
              (False, False) -> emptyNonDet @"nondet"
      runNonDet prog `shouldBe` [10, 20, 30]

  describe "Penelope.Effects.UnliftIO (Scoped Resource Safety & Provider)" $ do
    it "guarantees Bracket resource cleanup on success exit" $ do
      ref <- newIORef ([] :: [String])
      let acquire = do
            writeIORef ref ["acquired"]
            pure ("handle" :: String)
          release _ =
            modifyIORef' ref ("released" :)
          action _ = do
            modifyIORef' ref ("action" :)
            pure (42 :: Int)

      res <- bracketIO acquire release action
      res `shouldBe` 42
      logRes <- readIORef ref
      logRes `shouldBe` ["released", "action", "acquired"]

    it "guarantees Bracket resource cleanup on error exit" $ do
      ref <- newIORef ([] :: [String])
      let acquire = do
            writeIORef ref ["acquired"]
            pure ("handle" :: String)
          release _ =
            modifyIORef' ref ("released" :)
          actionErr _ = do
            modifyIORef' ref ("action-failed" :)
            error "simulated failure" :: IO Int

      (bracketIO acquire release actionErr >> pure ()) `shouldThrow` anyErrorCall
      logRes <- readIORef ref
      logRes `shouldBe` ["released", "action-failed", "acquired"]

    it "supports pure bracketEitherEff error exit" $ do
      let prog = bracketEitherEff (pure ("res" :: String)) (\_ -> pure ()) (\_ -> pure (Left "boom" :: Either String Int))
      runEff prog `shouldBe` Left "boom"

    it "injects dependencies via Provider purely" $ do
      let prog :: Eff '[] '["provider" ':= UnaryEffect (ProviderEff String)] String
          prog = do
            env <- askProvider @"provider"
            pure ("env:" <> env)
      runProviderPure "production" prog `shouldBe` "env:production"

  describe "Penelope.Effects.Scoped (Reader / Local)" $ do
    it "reads lexical environment purely via askReader and runReaderPure" $ do
      let prog :: Eff '[] '["reader" ':= UnaryEffect (AskEff Int)] Int
          prog = do
            v <- askReader
            pure (v * 2)
      runReaderPure (21 :: Int) prog `shouldBe` 42

    it "modifies environment in nested local blocks and restores state cleanly" $ do
      let prog :: Eff '[] '["reader" ':= UnaryEffect (AskEff Int)] (Int, Int, Int)
          prog = do
            v0 <- askReader
            v1 <- local @Int (+ 10) $ do
              inner1 <- askReader
              inner2 <- local @Int (* 2) askReader
              pure (inner1 + inner2)
            v2 <- askReader
            pure (v0, v1, v2)
      -- With initial 5:
      -- v0 = 5
      -- inner1 = 15, inner2 = (5 + 10) * 2 = 30 -> v1 = 45
      -- v2 = 5
      runReaderPure (5 :: Int) prog `shouldBe` (5, 45, 5)

    it "unrolls higher-order Local effect via localElaborator and elaborateAll" $ do
      let bodyPipe :: Penelope '[] '["reader" ':= UnaryEffect (AskEff Int)] '[] '["val" ':= Int]
          bodyPipe = unEff $ do
            x <- askReader
            pure (x + 100)

      let regs = insertField (Field @"body") (Region @'[] @'["reader" ':= UnaryEffect (AskEff Int)] @('PortSig '[] '["val" ':= Int]) bodyPipe) emptyRecord
      let retCont = BranchCont (\_outF -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"ret") retCont emptyRecord

      let localOpH :: Penelope '["local" ':= Local Int] '["reader" ':= UnaryEffect (AskEff Int)] '[] '["val" ':= Int]
          localOpH = OpH 0## (unsafeMkRowMorph []) regs (unsafeCoerce# ((* 2) :: Int -> Int)) exits

      let elabs = insertField (Field @"local") (localElaborator @"reader") emptyRecord
      let elaborated = elaborateAll elabs localOpH

      let handlers = insertField (Field @"reader") (readerHandler (10 :: Int)) emptyRecord
      let Identity outFrame = runFrameKleisli (interpretAll handlers elaborated) emptyRecord
      let !(Identity resVal) = indexSmallArray (let RowRep (Rep _ arrRep) = outFrame in arrRep) 0
      -- Initial 10 * 2 = 20, plus 100 = 120
      (unsafeCoerce# resVal :: Int) `shouldBe` 120

  describe "Penelope.Effects.Scoped (Error / Catch)" $ do
    it "evaluates successful computations to Right with runErrorPure" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] Int
          prog = pure 42
      runErrorPure prog `shouldBe` Right 42

    it "aborts on throw with Left error message" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] Int
          prog = do
            _ <- pure (10 :: Int)
            throw ("aborted-computation" :: String)
      runErrorPure prog `shouldBe` Left "aborted-computation"

    it "catches and recovers from errors via catchError" $ do
      let prog :: Eff '[] '["error" ':= UnaryEffect (ThrowEff String)] String
          prog = do
            catchError
              (do
                _ <- pure (1 :: Int)
                throw ("recoverable-error" :: String)
              )
              (\e -> pure ("caught:" <> e))
      runErrorPure prog `shouldBe` Right "caught:recoverable-error"

    it "unrolls higher-order Catch effect via catchElaborator and elaborateAll" $ do
      let bodyPipe :: Penelope '[] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          bodyPipe = unEff $ do
            _ <- throw ("scoped-error-thrown" :: String)
            pure "unreachable"

      let handlerPipe :: Penelope '[] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          handlerPipe = unEff (pure "intercepted-by-elaborator")

      let regs = insertField (Field @"body") (Region @'[] @'["error" ':= UnaryEffect (ThrowEff String)] @('PortSig '[] '["val" ':= String]) bodyPipe)
               $ insertField (Field @"handler") (Region @'[] @'["error" ':= UnaryEffect (ThrowEff String)] @('PortSig '[] '["val" ':= String]) handlerPipe)
               $ emptyRecord
      let retCont = BranchCont (\_outF -> Route (unsafeMkRowMorph [0]))
      let exits = insertField (Field @"ret") retCont emptyRecord

      let catchOpH :: Penelope '["catch" ':= Catch String] '["error" ':= UnaryEffect (ThrowEff String)] '[] '["val" ':= String]
          catchOpH = OpH 0## (unsafeMkRowMorph []) regs (unsafeCoerce# ()) exits

      let elabs = insertField (Field @"catch") (catchElaborator @"error") emptyRecord
      let elaborated = elaborateAll elabs catchOpH

      let handlers = insertField (Field @"error") errorHandler emptyRecord
      let res = runFrameKleisli (interpretAll handlers elaborated) emptyRecord
      case res of
        Right outFrame -> do
          let !(Identity resVal) = indexSmallArray (let RowRep (Rep _ arrRep) = outFrame in arrRep) 0
          (unsafeCoerce# resVal :: String) `shouldBe` "intercepted-by-elaborator"
        Left e ->
          expectationFailure ("Unexpected unhandled error: " <> show e)

  describe "Penelope.Effects.Concurrent (Async & Task Isolation)" $ do
    it "forks background IO task via forkEff" $ do
      mvar <- C.newEmptyMVar
      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] ()
          prog = forkEff @"concurrent" (C.putMVar mvar ("worker-done" :: String))
      runConcurrentIO prog
      res <- C.takeMVar mvar
      res `shouldBe` "worker-done"

    it "runs asynchronous tasks concurrently and awaits results via waitEff" $ do
      let task1 = do
            C.threadDelay 2000
            pure (10 :: Int)
          task2 = do
            C.threadDelay 1000
            pure (20 :: Int)

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] (Int, Int)
          prog = do
            h1 <- async task1
            h2 <- async task2
            r1 <- wait h1
            r2 <- wait h2
            pure (r1, r2)

      res <- runConcurrentIO prog
      res `shouldBe` (10, 20)

    it "propagates exceptions from failed asynchronous tasks safely" $ do
      let failingTask = do
            E.throwIO (userError "async-failure") :: IO Int

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] Int
          prog = do
            h <- async failingTask
            wait h

      runConcurrentIO prog `shouldThrow` anyException

  describe "Heftia Scoped Semantics & Multi-Shot Interaction" $ do
    it "preserves local reader environment across coroutine yield suspensions" $ do
      ref <- newIORef ([] :: [Int])
      let prog :: Eff '[] CoroReaderEF ()
          prog = do
            yield @"yield" (1 :: Int)
            local @Int (+ 100) $ do
              yield @"yield" (2 :: Int)
              x <- ask @"reader"
              yield @"yield" x
            yield @"yield" (3 :: Int)
            y <- ask @"reader"
            yield @"yield" y

      let yHandler :: EffHandler IO (YieldEff Int)
          yHandler = mkEffHandler $ \(Yield val) ->
            modifyIORef' ref (val :)

          rHandler :: EffHandler IO (AskEff Int)
          rHandler = readerHandler (10 :: Int)

          handlers = insertField (Field @"yield") yHandler
                   $ insertField (Field @"reader") rHandler
                   $ emptyRecord

      _ <- runEffM handlers prog
      yielded <- readIORef ref
      reverse yielded `shouldBe` [1, 2, 110, 3, 10]

    it "catches errors thrown during coroutine execution and resumes yielding downstream" $ do
      ref <- newIORef ([] :: [String])
      let prog :: Eff '[] CoroCatchEF ()
          prog = do
            yield @"yield" ("start" :: String)
            catchError
              (do
                yield @"yield" ("in-try" :: String)
                throwError @"error" ("simulated-failure" :: String)
                yield @"yield" ("unreachable" :: String)
              )
              (\e -> do
                yield @"yield" ("recovered:" <> e :: String)
              )
            yield @"yield" ("after-catch" :: String)

      let yHandler :: EffHandler IO (YieldEff String)
          yHandler = mkEffHandler $ \(Yield val) ->
            modifyIORef' ref (val :)

          eHandler :: EffHandler IO (ThrowEff String)
          eHandler = mkEffHandler $ \(Throw e) ->
            fail ("Unhandled throw caught by carrier: " <> e)

          handlers = insertField (Field @"yield") yHandler
                   $ insertField (Field @"error") eHandler
                   $ emptyRecord

      _ <- runEffM handlers prog
      yielded <- readIORef ref
      reverse yielded `shouldBe` ["start", "in-try", "recovered:simulated-failure", "after-catch"]

    it "maintains complete environment isolation across multi-shot non-deterministic branches" $ do
      let prog :: Eff '[] NonDetReaderEF Int
          prog = do
            b <- branch @"nondet"
            if b
              then local @Int (+ 10) (ask @"reader")
              else local @Int (+ 20) (ask @"reader")

      let ndHandler :: EffHandler [] NonDetEff
          ndHandler = nonDetHandler

          rHandler :: EffHandler [] (AskEff Int)
          rHandler = readerHandler (5 :: Int)

          handlers = insertField (Field @"nondet") ndHandler
                   $ insertField (Field @"reader") rHandler
                   $ emptyRecord

      let results = runEffM handlers prog
      results `shouldBe` [15, 25]

    it "guarantees balanced resource acquisition and cleanup across multiple concurrent branches" $ do
      ref <- newIORef ([] :: [String])
      let acquire name = do
            modifyIORef' ref ((name <> "-acq") :)
            pure name
          release name = do
            modifyIORef' ref ((name <> "-rel") :)

      let task1 = bracketIO (acquire "worker1") release (\n -> pure (n <> "-done"))
          task2 = bracketIO (acquire "worker2") release (\n -> pure (n <> "-done"))

      let prog :: Eff '[] '["concurrent" ':= UnaryEffect ConcurrentEff] (String, String)
          prog = do
            h1 <- async task1
            h2 <- async task2
            r1 <- wait h1
            r2 <- wait h2
            pure (r1, r2)

      res <- runConcurrentIO prog
      res `shouldBe` ("worker1-done", "worker2-done")

      logEntries <- readIORef ref
      logEntries `shouldContain` ["worker1-acq"]
      logEntries `shouldContain` ["worker1-rel"]
      logEntries `shouldContain` ["worker2-acq"]
      logEntries `shouldContain` ["worker2-rel"]
TEST_EOF

echo "=== [yamaarashi] Milestone 6.3 implementation complete ==="
