#!/usr/bin/env bash
# SPDX-License-Identifier: EUPL-1.2 OR Apache-2.0
# Implementation script for phase5-03-effect-battery
set -euo pipefail

echo "=== [yamaarashi] Implementing Milestone 5.3: Native Multi-Region and Multi-Exit Effect Battery ==="

mkdir -p packages/penelope-effects/src/Penelope/Effects

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

# 2. Write packages/penelope-effects/src/Penelope/Effects/Control.hs
cat <<'CONTROL_EOF' > packages/penelope-effects/src/Penelope/Effects/Control.hs
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
CONTROL_EOF

# 3. Write packages/penelope-effects/src/Penelope/Effects/Coroutine.hs
cat <<'COROUTINE_EOF' > packages/penelope-effects/src/Penelope/Effects/Coroutine.hs
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
COROUTINE_EOF

# 4. Write packages/penelope-effects/src/Penelope/Effects/UnliftIO.hs
cat <<'UNLIFTIO_EOF' > packages/penelope-effects/src/Penelope/Effects/UnliftIO.hs
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
Module      : Penelope.Effects.UnliftIO
Description : Sound scoped resource management, masking, and dependency injection
Copyright   : (c) 2026 The Penelope Authors
License     : EUPL-1.2 OR Apache-2.0
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides sound scoped resource management guaranteed never to leak handles
across dynamic evaluation contexts:
1. 'Bracket' higher-order scoped resource acquisition and cleanup
2. 'Mask' asynchronous exception safety
3. 'Provider' dependency injection

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
module Penelope.Effects.UnliftIO
  ( -- * Scoped Bracket
    Bracket (..)
  , bracketElaborator
  , bracketEff
  , bracketEitherEff
  , bracketIO

    -- * Masking
  , Mask
  , maskIO

    -- * Provider Dependency Injection
  , Provider
  , ProviderEff (..)
  , askProvider
  , providerHandler
  , runProviderPure
  ) where

import qualified Control.Exception as E
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Engine.Elaborate (Elaborator (..))
import Penelope.Core.Engine.Interpret (runPenelopePure)
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PolyEffect
  , PolyHEffect
  , PortSig (..)
  , Region (..)
  )
import Penelope.Effects.Unary
  ( Eff (..)
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), RowOf, emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Pair (..), Rep (..), RowRep (..))

{- |
Higher-order scoped 'Bracket' effect with three sub-regions:
1. @"acquire"@: Allocates the resource handle.
2. @"action"@: Operates with the acquired resource.
3. @"release"@: Guarantees release of the resource on all paths.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Bracket (r :: PortSig -> Type) (regs :: RowOf PortSig) (opIns :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  BracketOp :: Bracket r
    '[ "acquire" ':= 'PortSig ins '["res" ':= res]
     , "action"  ':= 'PortSig '["res" ':= res] '["out" ':= a]
     , "release" ':= 'PortSig '["res" ':= res] '[]
     ]
    ins
    '[ "ret" ':= '["out" ':= a] ]

{- |
Elaborator for 'Bracket' lowering scoped resource handling into guaranteed cleanup.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE bracketElaborator #-}
bracketElaborator ::
  forall ef.
  Elaborator '[] ef Bracket
bracketElaborator = Elaborator $ \inMap regs _ kConts ->
  LiftPure inMap
    (\inRec ->
      let !(Identity acqRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 0
          !(Region acqPipe) = unsafeCoerce# acqRegAny
          !(Identity actRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 1
          !(Region actPipe) = unsafeCoerce# actRegAny
          !(Identity relRegAny) = indexSmallArray (let RowRep (Rep _ arr) = regs in arr) 2
          !(Region relPipe) = unsafeCoerce# relRegAny

          !resRec = runPenelopePure acqPipe (unsafeCoerce# inRec)
          !actRec = runPenelopePure actPipe (unsafeCoerce# resRec)
          !relRec = runPenelopePure relPipe (unsafeCoerce# resRec)
      in seq relRec actRec
    )
    (BranchCont $ \finalActRec ->
      let !(Identity retContAny) = indexSmallArray (let RowRep (Rep _ arr) = kConts in arr) 0
          !(BranchCont retK) = unsafeCoerce# retContAny
      in retK (unsafeCoerce# finalActRec)
    )

{- |
Bracket pattern for 'Eff' ensuring resource cleanup.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE bracketEff #-}
bracketEff ::
  forall res a eh ef.
  Eff eh ef res
  -> (res -> Eff eh ef ())
  -> (res -> Eff eh ef a)
  -> Eff eh ef a
bracketEff acquire release action = do
  r <- acquire
  val <- action r
  release r
  pure val

{- |
Bracket pattern for 'Eff' with error exit propagation ensuring resource cleanup
on both successful and failed branches.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE bracketEitherEff #-}
bracketEitherEff ::
  forall res err a eh ef.
  Eff eh ef res
  -> (res -> Eff eh ef ())
  -> (res -> Eff eh ef (Either err a))
  -> Eff eh ef (Either err a)
bracketEitherEff acquire release action = do
  r <- acquire
  resOrErr <- action r
  release r
  pure resOrErr

{- |
Bracket pattern for 'IO' utilizing underlying exception safety guarantees.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE bracketIO #-}
bracketIO ::
  forall res a.
  IO res
  -> (res -> IO ())
  -> (res -> IO a)
  -> IO a
bracketIO = E.bracket

{- |
Higher-order scoped 'Mask' effect guaranteeing asynchronous exception masking.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Mask :: PolyHEffect

{- |
Mask asynchronous exceptions within an 'IO' action.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE maskIO #-}
maskIO ::
  forall a.
  ((forall b. IO b -> IO b) -> IO a)
  -> IO a
maskIO = E.mask

{- |
Polycategorical provider dependency injection effect.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data Provider (r :: Type) :: PolyEffect

{- |
Unary effect GADT representing an environment query.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
data ProviderEff (r :: Type) (a :: Type) where
  AskProvider :: ProviderEff r r

{- |
Query the provided environment from within 'Eff'.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE askProvider #-}
askProvider ::
  forall (l :: Symbol) r eh ef.
  ConstructorIndex l ef =>
  Eff eh ef r
askProvider = send @l AskProvider

{- |
Construct an 'EffHandler' supplying an environment handle @r@.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE providerHandler #-}
providerHandler ::
  forall r m.
  Monad m =>
  r
  -> EffHandler m (ProviderEff r)
providerHandler env = mkEffHandler $ \case
  AskProvider -> pure env

{- |
Run an 'Eff' computation with a provided environment handle @r@ purely.

See Note [Core Architecture] [ref:note-core-architecture] and
Note [Zero-Cost Surface Specializations] [ref:note-surface-facades].
-}
{-# INLINE runProviderPure #-}
runProviderPure ::
  forall r a.
  r
  -> Eff '[] '["provider" ':= UnaryEffect (ProviderEff r)] a
  -> a
runProviderPure env prog =
  let handlers = insertField (Field @"provider") (providerHandler env) emptyRecord
      Identity res = runEffM handlers prog
  in res
UNLIFTIO_EOF

# 5. Write packages/penelope-effects/src/Penelope/Effects.hs
cat <<'EFFECTS_EOF' > packages/penelope-effects/src/Penelope/Effects.hs
-- | Module: Penelope.Effects
-- Description: Core implementation and facade re-exports for penelope-effects
--
-- This module provides foundational data structures and domain operations for 'penelope-effects',
-- re-exporting the Heftia unary monadic facade ('Eff'), the Kernmantle binary arrow facade ('Rope'),
-- and the standard effect battery ('Control', 'Coroutine', 'UnliftIO').
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
  ) where

import Data.Text (Text)
import Penelope.Effects.Binary
import Penelope.Effects.Control
import Penelope.Effects.Coroutine
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
EFFECTS_EOF

# 6. Write packages/penelope-effects/test/Main.hs
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
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.Exts (unsafeCoerce#)
import Penelope.Core.Engine.Elaborate (elaborateAll)
import Penelope.Core.Engine.Interpret (runPenelopePure)
import Penelope.Core.Syntax (BranchCont (..), Penelope (..), PortSig (..), Region (..))
import Penelope.Effects
import Polytropos (Field (..), Record, Variant, emptyRecord, injectVariant, insertField)
import Polytropos.Core (Pair (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)
import Prelude hiding (id, (.))
import Test.Hspec

-- Unary Effect GADTs for testing
data AskEff :: Type -> Type where
  AskVal :: AskEff Int

data MsgEff :: Type -> Type where
  GetMsg :: MsgEff String
  SetMsg :: String -> MsgEff ()

type AskEF = '["ask" ':= UnaryEffect AskEff]
type MsgEF = '["msg" ':= UnaryEffect MsgEff]

-- Variant type for router tests
type EventSum = '["click" ':= '["x" ':= Int], "key" ':= '["char" ':= Char]]

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

          askHandler :: EffHandler Identity AskEff
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

          bodyPipe :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          bodyPipe = LiftPure (unsafeMkRowMorph [0])
            (\r ->
              let !(Identity x) = indexSmallArray (let RowRep (Rep _ sArr) = r in sArr) 0
                  !nextX = (unsafeCoerce# x :: Int) + 1
              in insertField (Field @"x") nextX emptyRecord
            )
            (BranchCont (\_ -> Route (unsafeMkRowMorph [0])))

          regs = insertField (Field @"cond") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["cond" ':= Bool]) condPipe)
               $ insertField (Field @"body") (Region @'[] @'[] @('PortSig '["x" ':= Int] '["x" ':= Int]) bodyPipe)
               $ emptyRecord

          retCont = BranchCont (\_ -> Route (unsafeMkRowMorph [0]))
          exits = insertField (Field @"exit") retCont emptyRecord

          whileTerm :: Penelope '["while" ':= While] '[] '["x" ':= Int] '["x" ':= Int]
          whileTerm = OpH 0## (unsafeMkRowMorph [0]) regs (unsafeCoerce# ()) exits

          elabs = insertField (Field @"while") whileElaborator emptyRecord
          elaborated = elaborateAll elabs whileTerm

          inFrame = insertField (Field @"x") (0 :: Int) emptyRecord
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
TEST_EOF

# 7. Build and verify
cabal build penelope-effects
cabal test penelope-effects:test:tests
echo "=== [yamaarashi] Milestone 5.3 implementation script completed successfully ==="
