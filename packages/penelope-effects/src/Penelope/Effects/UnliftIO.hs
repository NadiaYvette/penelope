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
Bracket pattern for 't:Eff' ensuring resource cleanup.

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
Bracket pattern for 't:Eff' with error exit propagation ensuring resource cleanup
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
Query the provided environment from within 't:Eff'.

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
Run an 't:Eff' computation with a provided environment handle @r@ purely.

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
