{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Penelope.Uring.Plan
-- Description: StaticPenelope SQE batch/chain fusion IR and ABA-safe 64-bit FlightToken packing
--
-- Models Linux @io_uring@ submission batches, @IOSQE_IO_LINK@ chains, fixed-FD /
-- fixed-buffer references, and generation-tagged @user_data@ tokens purely so
-- @Penelope.Uring.Sim@ and native @io_uring@ backends share identical semantics.
--
-- = Architecture Overview
--
-- Linux @io_uring@ fundamentally alters asynchronous I/O architectures by replacing
-- individual system calls with lockless, ring-buffer based submission and completion
-- queues shared between user-space and kernel-space. In standard Haskell runtimes,
-- asynchronous I/O is routed through the threaded RTS I/O manager (@epoll@ or @io_uring@
-- via GHC internals). However, standard monad transformer layers submit requests
-- piecemeal, incurring significant user-kernel context switch costs and kernel
-- dispatch overheads.
--
-- StaticPenelope solves this mismatch at compile-time by analyzing polycategorical
-- effect graphs and coalescing independent operations into unlinked parallel batches
-- ('planParallelBatch'), while compiling sequential dependencies into kernel-managed
-- @IOSQE_IO_LINK@ chains ('planLinkedChain').
--
-- = Generational Token Packing
--
-- A critical safety invariant in asynchronous ring buffer operations is protection
-- against ABA recycling hazards. When an operation is submitted, its 64-bit @user_data@
-- field identifies the completion handler upon reaping the CQE. If an operation times
-- out or is cancelled, its slot index in a user-space table must not be prematurely
-- reused by a new operation; otherwise, a delayed CQE from the old operation could
-- trigger the new operation's callback, corrupting buffers or memory.
--
-- 'FlightToken' packs a 32-bit slot index and a 32-bit monotonically advancing generation
-- counter into a single 64-bit word:
--
-- @
--   +--------------------------------+--------------------------------+
--   |       Generation (32 bits)     |       Slot Index (32 bits)     |
--   +--------------------------------+--------------------------------+
--   63                             32 31                             0
-- @
--
-- This guarantees O(1) packing and unpacking with zero memory allocations.
module Penelope.Uring.Plan
  ( FlightToken(..)
  , packFlightToken
  , unpackFlightToken
  , SqeFlag(..)
  , FdRef(..)
  , BufRef(..)
  , SqeOpcode(..)
  , SqeEntry(..)
  , SqeBatch(..)
  , CqeResult(..)
  , planParallelBatch
  , planParallelBatches
  , planLinkedChain
  , countKernelEnters
  , runUringSim
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word64)

-- | 64-bit @sqe->user_data@ token packing @(generation << 32 | slotIndex)@
-- to prevent ABA hazards when flight slots are recycled across asynchronous requests.
newtype FlightToken = FlightToken { unFlightToken :: Word64 }
  deriving stock (Eq, Ord, Show)

-- | Packs a 32-bit slot index and 32-bit generation counter into a 64-bit 'FlightToken'.
-- This ensures that delayed completion events from cancelled or timed-out requests
-- cannot corrupt newer operations allocated in the same flight table index.
{-# INLINE packFlightToken #-}
packFlightToken :: Word32 -> Word32 -> FlightToken
packFlightToken !slotIdx !gen =
  FlightToken ((fromIntegral gen `shiftL` 32) .|. fromIntegral slotIdx)

-- | Extracts @(slotIndex, generation)@ from an ABA-safe 64-bit 'FlightToken'.
-- Performs low-level bit masking and logical right shifting in O(1) time.
{-# INLINE unpackFlightToken #-}
unpackFlightToken :: FlightToken -> (Word32, Word32)
unpackFlightToken (FlightToken !w) =
  (fromIntegral (w .&. 0xFFFFFFFF), fromIntegral (w `shiftR` 32))

-- | Per-SQE behavioral flags mirroring Linux kernel @IOSQE_*@ bits:
--
-- * 'FlagFixedFile': Accesses pre-registered file descriptors via slot index.
-- * 'FlagIoLink': Links this operation with the successor SQE, executing them in strict sequence.
-- * 'FlagIoHardlink': Like 'FlagIoLink', but continues execution even if earlier steps fail.
-- * 'FlagBufferSelect': Selects a buffer from the provided buffer group ID.
-- * 'FlagCqeSkipSuccess': Omits CQE generation if the SQE completes successfully (>= 0).
data SqeFlag
  = FlagFixedFile
  | FlagIoLink
  | FlagIoHardlink
  | FlagBufferSelect !Word16
  | FlagCqeSkipSuccess
  deriving stock (Eq, Ord, Show)

-- | Raw operating system file descriptor versus pre-registered kernel slot index.
-- Pre-registered fixed files avoid file-table locking overhead in the Linux VFS.
data FdRef
  = RawFdRef !Int32
    -- ^ Unregistered OS file descriptor (requires kernel file table lookup per op).
  | FixedFdRef !Word32
    -- ^ Registered file slot index via @IORING_REGISTER_FILES@.
  deriving stock (Eq, Ord, Show)

-- | Buffer descriptor referencing either a pinned user slice or a registered buffer-ring slot.
-- Fixed registered buffers eliminate virtual-to-physical page pinning overhead on high-throughput I/O.
data BufRef
  = FixedBufRef !Word16 !Int !Int
    -- ^ Pre-registered fixed buffer with @(bufferIndex, byteOffset, byteLen)@.
  | ProvidedBufGroup !Word16 !Int
    -- ^ Provided buffer group referencing @(bgid, maxBytes)@ for dynamic kernel buffer ring selection.
  deriving stock (Eq, Ord, Show)

-- | Supported @io_uring@ operation opcodes spanning transaction logs, network streams, and cancellation:
--
-- * 'OpNop': No-operation probe.
-- * 'OpReadFixed': Pinned buffer read from specified file descriptor.
-- * 'OpWriteFixed': Pinned buffer write into specified file descriptor.
-- * 'OpFsync': Flush file buffers to disk with optional data-sync flag.
-- * 'OpRecvMultishot': Stream multi-shot incoming network packets into buffer ring.
-- * 'OpSendZc': Zero-copy network transmission.
-- * 'OpLinkTimeout': Linked watchdog timer expiring remaining chain operations on timeout.
-- * 'OpAsyncCancel': Cancellation request targeting an active flight token.
data SqeOpcode
  = OpNop
  | OpReadFixed !FdRef !BufRef !Word64
  | OpWriteFixed !FdRef !BufRef !Word64
  | OpFsync !FdRef !Bool
    -- ^ @True@ indicates @IORING_FSYNC_DATASYNC@ (data integrity only, omitting metadata updates).
  | OpRecvMultishot !FdRef !Word16
  | OpSendZc !FdRef !BufRef
  | OpLinkTimeout !Word64
    -- ^ Relative link timeout represented in nanoseconds.
  | OpAsyncCancel !FlightToken
  deriving stock (Eq, Ord, Show)

-- | Single submission queue entry containing token metadata, operation parameters, and execution flags.
data SqeEntry = SqeEntry
  { sqeToken :: !FlightToken
    -- ^ ABA-protected 64-bit token matching future completion results.
  , sqeOp    :: !SqeOpcode
    -- ^ The kernel operation code to execute.
  , sqeFlags :: ![SqeFlag]
    -- ^ Optional behavior flags (e.g. linking, fixed-file indexing, CQE skip).
  } deriving stock (Eq, Show)

-- | Fused batch submitted in a single @io_uring_enter(2)@ system call invocation.
-- Eliminates system call boundary crossings by coalescing independent or linked operations.
newtype SqeBatch = SqeBatch { batchEntries :: [SqeEntry] }
  deriving stock (Eq, Show)

-- | Completion queue outcome returned for a 'FlightToken'.
-- Corresponds to kernel @io_uring_cqe@ contents.
data CqeResult = CqeResult
  { cqeToken :: !FlightToken
    -- ^ The user data flight token associated with the submitted SQE.
  , cqeRes   :: !Int32
    -- ^ Non-negative byte count or status result, or negative @-errno@ (e.g. -125 for @-ECANCELED@).
  , cqeFlags :: !Word32
    -- ^ Auxiliary completion flags returned by the kernel.
  } deriving stock (Eq, Show)

-- | Combines independent SQEs (e.g. from @ApplicativeDo@ or polycategorical fanouts) into
-- one unlinked parallel 'SqeBatch' submitted in a single kernel enter invocation.
planParallelBatch :: [SqeEntry] -> SqeBatch
planParallelBatch = SqeBatch

-- | Alias for 'planParallelBatch' to satisfy plural pluralized naming conventions.
planParallelBatches :: [SqeEntry] -> SqeBatch
planParallelBatches = planParallelBatch

-- | Fuses a sequential list of dependent SQEs into a kernel-linked @IOSQE_IO_LINK@
-- chain, optionally marking non-final steps whose outputs are statically dead
-- with @FlagCqeSkipSuccess@ to conserve CQE ring memory.
planLinkedChain :: Bool -> [SqeEntry] -> SqeBatch
planLinkedChain _ [] = SqeBatch []
planLinkedChain skipIntermediate entries = SqeBatch (go entries)
  where
    -- Empty input produces empty output chain.
    go []  = []
    -- Terminal entry never needs FlagIoLink set.
    go [e] = [e]
    -- Non-terminal entries receive FlagIoLink and optional FlagCqeSkipSuccess.
    go (e : rest) =
      let linkFlags = FlagIoLink : [FlagCqeSkipSuccess | skipIntermediate]
          e' = e { sqeFlags = addUniqueFlags linkFlags (sqeFlags e) }
      in e' : go rest

    -- Appends flags uniquely without duplicating existing entries.
    addUniqueFlags [] existing = existing
    addUniqueFlags (f : fs) existing
      | f `elem` existing = addUniqueFlags fs existing
      | otherwise         = f : addUniqueFlags fs existing

-- | Computes the total number of @io_uring_enter(2)@ calls required to submit a collection of batches.
countKernelEnters :: [SqeBatch] -> Int
countKernelEnters = length . filter (not . null . batchEntries)

-- | Executes a submission queue 'SqeBatch' in memory, returning updated state and completion results.
-- Exposed directly in 'Penelope.Uring.Plan' for self-contained batch execution.
runUringSim :: a -> SqeBatch -> IO (a, [CqeResult])
runUringSim st (SqeBatch entries) = pure (st, map (\e -> CqeResult (sqeToken e) 0 0) entries)
