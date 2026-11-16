-- VERBATIM_CODE_SEED: TP-007B
-- TARGET_REPO: ~/src/penelope
-- TARGET_PACKAGE: penelope-core, penelope-stage
-- TARGET_FILES:
--   src/Penelope/Backend/Plugin.hs
--   src/Penelope/Stage/OrganIR.hs

{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.Plugin
  ( Endianness(..)
  , TargetTriple(..)
  , TargetLayout(..)
  , MergeStyle(..)
  , BackendPlugin(..)
  , defaultHostTargetLayout
  , resolveSlotByteOffset
  , compileWithBackend
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import GHC.Exts (Any, unsafeCoerce#)

import Polytropos (Record, RecordF)
import Polytropos.Core (Row)
import Penelope.Core.Syntax (Penelope)
import Penelope.Engine (PolyCarrier, Handler, interpretAll)
import Penelope.Perceus (RTSProfile(..))

data Endianness = LittleEndian | BigEndian
  deriving (Show, Eq)

data TargetTriple = TargetTriple
  { ttArch   :: !Text
  , ttVendor :: !Text
  , ttOS     :: !Text
  , ttABI    :: !Text
  } deriving (Show, Eq)

-- | Hardware layout descriptor evaluated late at the backend boundary so front-ends
--   never bake host pointer widths or endianness into OrganIR.
data TargetLayout = TargetLayout
  { tlTriple     :: !TargetTriple
  , tlEndianness :: !Endianness
  , tlPtrBits    :: !Word16
  , tlWordAlign  :: !Word8
  , tlCacheLine  :: !Word16
  } deriving (Show, Eq)

defaultHostTargetLayout :: TargetLayout
defaultHostTargetLayout = TargetLayout
  { tlTriple     = TargetTriple "x86_64" "unknown" "linux" "gnu"
  , tlEndianness = LittleEndian
  , tlPtrBits    = 64
  , tlWordAlign  = 8
  , tlCacheLine  = 64
  }

-- | Computes the physical byte offset of canonical Polytropos field rank `c_idx`
--   under the given `TargetLayout` and `RTSProfile`.
{-# INLINE resolveSlotByteOffset #-}
resolveSlotByteOffset :: TargetLayout -> RTSProfile -> Int -> Int
resolveSlotByteOffset !layout !rts !canonicalSlotIdx =
  let !wordBytes = fromIntegral (tlWordAlign layout) :: Int
      !headerWords = case rts of
        RTSFreestandingLinear -> 0 -- Zero RC header in freestanding kernel mode!
        _                     -> 1 -- 1-word header (tag + m_rc)
  in (headerWords + canonicalSlotIdx) * wordBytes

data MergeStyle
  = MergeBlockArgs       -- ^ MLIR (`^bb(%arg0: T)`)
  | MergePhiNodes        -- ^ GIMPLE (`__PHI`) and LLVM IR (`phi`)
  | MergeStructuredStack -- ^ WASM and JVM
  deriving (Show, Eq)

-- | Universal plug-in interface for code-generating and JIT `PolyCarrier` back-ends.
data BackendPlugin (val :: Type -> Type) (carrier :: Row -> Type) (artifact :: Type) =
  BackendPlugin
    { bpName         :: !Text
    , bpMergeStyle   :: !MergeStyle
    , bpCarrier      :: !(PolyCarrier carrier)
    , bpCoreHandlers :: forall ef. TargetLayout -> RTSProfile -> Any
    , bpFinalize     :: forall ins outs.
                        TargetLayout
                     -> RTSProfile
                     -> carrier outs
                     -> artifact
    }

-- | Coerce a symbolic register frame `RecordF val ins` into `Record ins`
--   and drive `Penelope.Engine.interpretAll`.
compileWithBackend
  :: BackendPlugin val carrier artifact
  -> TargetLayout
  -> RTSProfile
  -> Penelope '[] ef ins outs
  -> RecordF val ins
  -> artifact
compileWithBackend (BackendPlugin _ _ carrier handlers finalize) !layout !rts prog initSymbolicFrame =
  let !coercedFrame = unsafeCoerce# initSymbolicFrame :: Record ins
  in finalize layout rts $
       interpretAll carrier (handlers layout rts) prog coercedFrame