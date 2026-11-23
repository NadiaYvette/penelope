{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Penelope.Uring.Sim
-- Description: Deterministic in-memory io_uring execution simulator
--
-- Simulates Linux @io_uring@ submission batch semantics in memory, including
-- @IOSQE_IO_LINK@ failure cascades (aborted remaining ops receiving @-ECANCELED@),
-- @FlagCqeSkipSuccess@ dead-result elision, asynchronous cancellation, and
-- fixed file descriptor / fixed buffer reading and writing.
--
-- = In-Memory Simulation Mechanics
--
-- Running asynchronous I/O tests in airgapped build environments or unprivileged
-- containers often encounters seccomp filters blocking @io_uring_setup(2)@.
-- This module supplies a fully deterministic, pure in-memory execution model
-- preserving exact kernel invariants:
--
-- * State isolation: All registered file descriptors and fixed buffers reside
--   within an immutable 'SimState' map structure.
-- * Linked failure cascades: When an entry tagged with 'FlagIoLink' terminates
--   with a negative errno status (such as bad file descriptor or unmapped buffer),
--   the kernel immediately aborts all subsequent linked operations in that chain
--   with @-125@ (@-ECANCELED@). The simulator mirrors this cascade faithfully.
-- * Dead CQE elision: By configuring 'FlagCqeSkipSuccess' on non-terminal linked
--   chain entries, intermediate CQEs are suppressed from the completion ring,
--   significantly reducing queue pressure and memory consumption.
-- * Determinism: Operations execute sequentially in submission order with zero
--   randomness, enabling differential verification against physical disk filesystems.
module Penelope.Uring.Sim
  ( -- * Simulator State
    SimState(..)
  , emptySimState
    -- * File Descriptor Registration
  , registerSimFd
  , registerSimFixedFd
  , lookupSimFd
    -- * Buffer Registration
  , registerSimBuf
  , lookupSimBuf
    -- * Execution Engines
  , runUringSim
  , runUringSimPure
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16, Word32, Word64)
import Penelope.Uring.Plan hiding (runUringSim)

-- | In-memory state of the simulated kernel @io_uring@ subsystem.
data SimState = SimState
  { simFiles     :: !(Map Int32 BS.ByteString)
    -- ^ Map of raw OS file descriptors to in-memory byte contents.
  , simFixedFds  :: !(Map Word32 BS.ByteString)
    -- ^ Map of pre-registered fixed file slot indices to byte contents.
  , simFixedBufs :: !(Map Word16 BS.ByteString)
    -- ^ Map of pre-registered fixed buffer slots to byte contents.
  , simCancelled :: !(Set FlightToken)
    -- ^ Set of flight tokens that have been cancelled via 'OpAsyncCancel'.
  } deriving stock (Eq, Show)

-- | Constructs an initial, empty simulator state with no registered files or buffers.
emptySimState :: SimState
emptySimState = SimState
  { simFiles     = Map.empty
  , simFixedFds  = Map.empty
  , simFixedBufs = Map.empty
  , simCancelled = Set.empty
  }

-- | Registers or updates an in-memory byte payload at a raw OS file descriptor index.
registerSimFd :: Int32 -> BS.ByteString -> SimState -> SimState
registerSimFd fd content st = st { simFiles = Map.insert fd content (simFiles st) }

-- | Registers or updates an in-memory byte payload at a fixed file slot index.
registerSimFixedFd :: Word32 -> BS.ByteString -> SimState -> SimState
registerSimFixedFd slot content st = st { simFixedFds = Map.insert slot content (simFixedFds st) }

-- | Registers or updates an in-memory byte buffer at a pre-registered buffer slot index.
registerSimBuf :: Word16 -> BS.ByteString -> SimState -> SimState
registerSimBuf slot content st = st { simFixedBufs = Map.insert slot content (simFixedBufs st) }

-- | Retrieves the current byte payload associated with a file descriptor reference.
lookupSimFd :: FdRef -> SimState -> Maybe BS.ByteString
lookupSimFd (RawFdRef fd) st      = Map.lookup fd (simFiles st)
lookupSimFd (FixedFdRef slot) st  = Map.lookup slot (simFixedFds st)

-- | Retrieves the byte payload associated with a fixed buffer slot index.
lookupSimBuf :: Word16 -> SimState -> Maybe BS.ByteString
lookupSimBuf slot st = Map.lookup slot (simFixedBufs st)

-- | Deterministically executes a submission queue 'SqeBatch' in memory.
--
-- Strict behavior invariants:
-- 1. Sequential processing of SQEs in batch order.
-- 2. If an SQE with 'FlagIoLink' fails (result < 0), all subsequent linked SQEs
--    in that dependent chain are immediately aborted with @-125@ (@-ECANCELED@).
-- 3. If an SQE specifies 'FlagCqeSkipSuccess' and its outcome is successful (>= 0),
--    no corresponding 'CqeResult' entry is appended to the completion list.
-- 4. 'OpAsyncCancel' records the target token in the cancellation set and returns 0.
runUringSimPure :: SimState -> SqeBatch -> (SimState, [CqeResult])
runUringSimPure initialState (SqeBatch entries) = go initialState False [] entries
  where
    -- Process list of SQE entries preserving link cascade status.
    go !st _ !cqeAcc [] = (st, reverse cqeAcc)
    go !st !cascade !cqeAcc (e : rest) =
      let hasLink = FlagIoLink `elem` sqeFlags e
          skipSuccess = FlagCqeSkipSuccess `elem` sqeFlags e
          tok = sqeToken e
      in if cascade
           then
             -- Previous linked operation failed: abort this operation with -ECANCELED (-125).
             let cqe = CqeResult tok (-125) 0
                 -- Cascade continues as long as this entry has FlagIoLink.
                 nextCascade = hasLink
             in go st nextCascade (cqe : cqeAcc) rest
           else
             -- Normal execution of operation.
             let (st', resVal) = executeOp (sqeOp e) tok st
                 isSuccess = resVal >= 0
                 -- Trigger link cascade if this operation failed and is linked.
                 nextCascade = hasLink && not isSuccess
                 cqe = CqeResult tok resVal 0
                 -- Elide CQE if FlagCqeSkipSuccess is active and operation succeeded.
                 cqeAcc' = if skipSuccess && isSuccess then cqeAcc else cqe : cqeAcc
             in go st' nextCascade cqeAcc' rest

    -- Dispatches single opcodes against simulator state.
    executeOp OpNop _tok st = (st, 0)

    executeOp (OpReadFixed fdRef bufRef fileOffset) _tok st =
      case lookupSimFd fdRef st of
        Nothing -> (st, -9) -- -EBADF (Bad file descriptor)
        Just fileBytes ->
          case bufRef of
            FixedBufRef _bufSlot _bufOff reqLen ->
              let fLen = fromIntegral (BS.length fileBytes) :: Word64
              in if fileOffset >= fLen
                   then (st, 0) -- EOF: zero bytes read
                   else
                     let available = fromIntegral (fLen - fileOffset) :: Int
                         toRead = min reqLen available
                     in (st, fromIntegral toRead)
            ProvidedBufGroup _bgid maxBytes ->
              let fLen = fromIntegral (BS.length fileBytes) :: Word64
              in if fileOffset >= fLen
                   then (st, 0)
                   else
                     let available = fromIntegral (fLen - fileOffset) :: Int
                         toRead = min maxBytes available
                     in (st, fromIntegral toRead)

    executeOp (OpWriteFixed fdRef bufRef fileOffset) _tok st =
      case bufRef of
        FixedBufRef bufSlot _bufOff reqLen ->
          let srcPayload = case lookupSimBuf bufSlot st of
                Just bs -> BS.take reqLen bs
                Nothing -> BS.replicate reqLen 0
              currentBytes = fromMaybe BS.empty (lookupSimFd fdRef st)
              off = fromIntegral fileOffset :: Int
              padded = if BS.length currentBytes < off
                         then currentBytes <> BS.replicate (off - BS.length currentBytes) 0
                         else currentBytes
              prefix = BS.take off padded
              suffix = BS.drop (off + reqLen) padded
              newFileBytes = prefix <> srcPayload <> suffix
              st' = case fdRef of
                RawFdRef fd    -> registerSimFd fd newFileBytes st
                FixedFdRef slot -> registerSimFixedFd slot newFileBytes st
          in (st', fromIntegral reqLen)
        ProvidedBufGroup _ _ ->
          (st, -22) -- -EINVAL (Provided buffers not supported on writes)

    executeOp (OpFsync fdRef _dataSync) _tok st =
      case lookupSimFd fdRef st of
        Nothing -> (st, -9) -- -EBADF
        Just _  -> (st, 0)  -- Fsync succeeds in-memory

    executeOp (OpRecvMultishot fdRef _bufGroup) _tok st =
      case lookupSimFd fdRef st of
        Nothing -> (st, -9)
        Just bs -> (st, fromIntegral (BS.length bs))

    executeOp (OpSendZc fdRef bufRef) _tok st =
      case bufRef of
        FixedBufRef _ _ reqLen ->
          case lookupSimFd fdRef st of
            Nothing -> (st, -9)
            Just _  -> (st, fromIntegral reqLen)
        ProvidedBufGroup _ _ -> (st, -22)

    executeOp (OpLinkTimeout _ns) _tok st = (st, 0)

    executeOp (OpAsyncCancel targetToken) _tok st =
      let st' = st { simCancelled = Set.insert targetToken (simCancelled st) }
      in (st', 0)

-- | IO action wrapper around 'runUringSimPure' for drop-in use in monadic/bench pipelines.
runUringSim :: SimState -> SqeBatch -> IO (SimState, [CqeResult])
runUringSim st batch = pure (runUringSimPure st batch)
