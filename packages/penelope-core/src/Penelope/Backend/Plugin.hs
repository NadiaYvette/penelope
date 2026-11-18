-- |
-- Module      : Penelope.Backend.Plugin
-- Copyright   : (c) 2026 The Penelope Authors
-- License     : EUPL-1.2 OR Apache-2.0
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Universal plug-in interface for Penelope code-generation and JIT backends.
-- Decouples hardware layout descriptions (word size, endianness, pointer alignment)
-- from front-end polycategorical eDSLs, enabling uniform compilation to GCC -fgimple,
-- MLIR, LLVM IR, and WebAssembly.
--
-- {- Note [Hardware Layout Decoupling in Penelope Backends]
-- Front-ends in Penelope construct polycategorical ASTs purely in terms of type-level
-- open rows and canonical slot indices. Host platform characteristics (such as whether
-- pointers are 32-bit or 64-bit, or whether byte order is little-endian or big-endian)
-- are deliberately omitted from the AST. Instead, the backend boundary receives a
-- 'TargetLayout' and an 'RTSProfile', computing physical memory offsets on demand via
-- 'resolveSlotByteOffset'. This allows the same polycategorical term to be compiled
-- interchangeably for freestanding 64-bit Linux kernels, 32-bit embedded targets, or
-- virtual machine environments.
-- -}
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.Plugin
  ( -- * Target Architecture Descriptors
    Endianness(..)
  , TargetTriple(..)
  , TargetLayout(..)
  , defaultHostTargetLayout
  , resolveSlotByteOffset

    -- * Control Flow Merge Styles
  , MergeStyle(..)

    -- * Backend Plugin Abstraction
  , BackendPlugin(..)
  , compileWithBackend
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import GHC.Exts (Any, unsafeCoerce#)

import Polytropos (Record, RecordF)
import Polytropos.Core (Row)
import Penelope.Core.Syntax (Penelope)
import Penelope.Engine (PolyCarrier, interpretAll)
import Penelope.Perceus (RTSProfile(..))

-- | Byte order endianness for target architecture memory serialization.
data Endianness
  = LittleEndian -- ^ Least significant byte stored at lowest address (x86_64, AArch64 little-endian).
  | BigEndian    -- ^ Most significant byte stored at lowest address (s390x, SPARC).
  deriving (Show, Eq)

-- | LLVM / GCC target triple representation identifying architecture, vendor, OS, and ABI.
data TargetTriple = TargetTriple
  { ttArch   :: !Text -- ^ Target CPU architecture (e.g. @"x86_64"@, @"aarch64"@, @"riscv64"@).
  , ttVendor :: !Text -- ^ Target vendor string (e.g. @"unknown"@, @"apple"@, @"pc"@).
  , ttOS     :: !Text -- ^ Target operating system (e.g. @"linux"@, @"darwin"@, @"none"@).
  , ttABI    :: !Text -- ^ Target ABI standard (e.g. @"gnu"@, @"eabi"@, @"elf"@).
  } deriving (Show, Eq)

-- | Complete hardware layout descriptor evaluated late at the backend boundary.
--   Prevents front-end eDSLs from baking host-specific assumptions into staged ASTs.
data TargetLayout = TargetLayout
  { tlTriple     :: !TargetTriple -- ^ Target system identification triple.
  , tlEndianness :: !Endianness   -- ^ Target memory byte ordering.
  , tlPtrBits    :: !Word16       -- ^ Target pointer bitwidth (e.g. 64 or 32).
  , tlWordAlign  :: !Word8        -- ^ Native machine word alignment in bytes (e.g. 8 or 4).
  , tlCacheLine  :: !Word16       -- ^ L1 data cache line size in bytes (e.g. 64).
  } deriving (Show, Eq)

-- | Default layout descriptor for the standard 64-bit Linux x86_64 host system.
defaultHostTargetLayout :: TargetLayout
defaultHostTargetLayout = TargetLayout
  { tlTriple     = TargetTriple "x86_64" "unknown" "linux" "gnu"
  , tlEndianness = LittleEndian
  , tlPtrBits    = 64
  , tlWordAlign  = 8
  , tlCacheLine  = 64
  }

-- | Computes the physical byte offset of a canonical Polytropos field rank @c_idx@
--   under the given 'TargetLayout' and runtime profile ('RTSProfile').
--
--   Under 'RTSFreestandingLinear' (Profile 0), the object header is stripped (0 words),
--   allowing direct zero-overhead slot indexing. Under dynamic runtime profiles,
--   a 1-word header (containing the variant tag and reference count) precedes the payload slots.
{-# INLINE resolveSlotByteOffset #-}
resolveSlotByteOffset :: TargetLayout -> RTSProfile -> Int -> Int
resolveSlotByteOffset !layout !rts !canonicalSlotIdx =
  let !wordBytes = fromIntegral (tlWordAlign layout) :: Int
      !headerWords = case rts of
        RTSFreestandingLinear -> 0 -- Zero RC header in freestanding kernel mode!
        _                     -> 1 -- 1-word header (tag + m_rc)
  in (headerWords + canonicalSlotIdx) * wordBytes

-- | Control flow merge mechanism supported by the target intermediate representation.
data MergeStyle
  = MergeBlockArgs       -- ^ Basic block argument passing, as used in MLIR (@^bb(%arg0: T)@).
  | MergePhiNodes        -- ^ Static Single Assignment phi nodes, as used in GCC GIMPLE (@__PHI@) and LLVM IR (@phi@).
  | MergeStructuredStack -- ^ Structured control stack instructions, as used in WebAssembly and JVM bytecode.
  deriving (Show, Eq)

-- | Universal plug-in interface for code-generating and JIT 'PolyCarrier' backends.
--
-- == Compilation Stages & Pipeline Architecture
-- 1. __Target Configuration Binding:__
--    The backend receives physical 'TargetLayout' constraints (pointer size, endianness, word alignment)
--    and the active 'RTSProfile' (Freestanding Linear Profile 0, Perceus RC Profile 1, or Managed GC Profile 2).
-- 2. __Algebraic Handler Table Synthesis:__
--    'bpCoreHandlers' instantiates a target-specialized handler vector implementing hardware-level
--    effects (heap allocation, slot projection, reference counter increment/decrement).
-- 3. __Polycategorical IR Carrier Execution:__
--    'compileWithBackend' maps the polycategorical hypergraph across 'bpCarrier', accumulating
--    SSA variables, basic blocks, and virtual memory tokens without creating intermediate AST nodes.
-- 4. __Artifact Finalization:__
--    'bpFinalize' renders the accumulated carrier into the target compiler IR representation
--    (e.g., GCC @-fgimple@ source text or MLIR dialect modules).
--
-- Parameterised over:
-- [@val@]: The symbolic value wrapper (e.g. 'Penelope.Backend.MLIR.SSAVal' or 'Penelope.Backend.Gimple.GimpleVal').
-- [@carrier@]: The row-parameterised carrier monad threading SSA state across emission steps.
-- [@artifact@]: The emitted compilation product (e.g. 'Text' containing formatted IR).
data BackendPlugin (val :: Type -> Type) (carrier :: Row -> Type) (artifact :: Type) =
  BackendPlugin
    { bpName         :: !Text
      -- ^ Human-readable identifier for the backend plugin (e.g. @"MLIR"@, @"GIMPLE"@).
    , bpMergeStyle   :: !MergeStyle
      -- ^ Control flow merge convention used by this backend's IR.
    , bpCarrier      :: !(PolyCarrier carrier)
      -- ^ Polycategorical carrier implementation providing wire routing and lifting.
    , bpCoreHandlers :: TargetLayout -> RTSProfile -> Any
      -- ^ Target-specialized algebraic effect handler table for primitive operations.
    , bpFinalize     :: forall outs.
                        TargetLayout
                     -> RTSProfile
                     -> carrier outs
                     -> artifact
      -- ^ Final packaging callback converting the accumulated carrier into the target artifact.
    }

-- | Compiles a first-order polycategorical hypergraph using the supplied 'BackendPlugin'.
--
-- == Invariants & Operational Guarantees
-- 1. __Symbolic Frame Preservation:__
--    The initial symbolic register frame 'RecordF val ins' is coerced via 'unsafeCoerce#'
--    into 'Record ins' without altering in-memory layout or traversing child elements.
-- 2. __Carrier Interpretation:__
--    'Penelope.Engine.interpretAll' evaluates the hypergraph against the backend carrier,
--    folding polycategorical routing morphisms and algebraic operations into linear IR instructions.
-- 3. __Late Target Binding:__
--    Machine-specific offsets and calling conventions are deferred until this invocation,
--    ensuring the same polycategorical IR term can target multiple backends simultaneously.
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
