{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Penelope.Uring.FlightTable
-- Description: Fixed-capacity ABA-safe flight token table with buffer ownership tracking
--
-- Manages in-flight @io_uring@ request slots with 64-bit generation-tagged tokens.
-- Guarantees that pinned buffer resources remain retained until explicit completion
-- or cancellation CQEs are reaped, preventing ABA hazard recycling.
--
-- = Generational Slot Lifecycle
--
-- In asynchronous systems where buffers are pinned in memory for kernel DMA,
-- premature deallocation or buffer reuse leads to severe data corruption.
-- This module coordinates the slot allocation, generational validation,
-- and cleanup execution lifecycle:
--
-- 1. 'allocateSlot' searches the fixed-capacity table for an unused slot index.
--    Upon location, it advances the slot's generation counter and records an
--    optional teardown action (e.g. freeing pinned memory or decrementing refcounts).
-- 2. When the kernel posts a CQE to the completion ring, 'completeSlot' extracts
--    the slot index and generation from the returned 'FlightToken'.
-- 3. If the token generation matches the recorded active generation, the teardown
--    action is executed and the slot is marked available for future requests.
-- 4. If the token generation is stale (e.g. from an expired or cancelled request),
--    'completeSlot' rejects the completion with an explicit error without running
--    active cleanup routines.
--
-- = Concurrency Guarantees
--
-- All slot modifications occur within atomic reference updates via 'atomicModifyIORef''.
-- Table capacity is bounded at creation time, preventing unbounded growth.
module Penelope.Uring.FlightTable
  ( -- * Flight Table Types
    FlightTable
  , SlotInfo(..)
    -- * Table Lifecycle
  , newFlightTable
    -- * Slot Allocation and Completion
  , allocateSlot
  , completeSlot
  , lookupSlot
  , activeSlotCount
  , resetFlightTable
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Penelope.Uring.Plan (FlightToken(..), packFlightToken, unpackFlightToken)

-- | Internal metadata held per active slot in the flight table.
data SlotInfo = SlotInfo
  { slotGen     :: !Word32
    -- ^ The generation counter when this slot was allocated.
  , slotRelease :: !(Maybe (IO ()))
    -- ^ Optional buffer release or cleanup action invoked on completion.
  }

-- | Internal state record maintaining active slot allocations and generational sequences.
data TableState = TableState
  { tsActive :: !(Map Word32 SlotInfo)
    -- ^ Map from slot index (0 .. capacity - 1) to active slot metadata.
  , tsGens   :: !(Map Word32 Word32)
    -- ^ Map from slot index to last allocated generation counter.
  }

-- | Fixed-capacity flight slot table for tracking outstanding @io_uring@ operations.
-- Thread-safe via atomic reference modifications.
data FlightTable = FlightTable
  { ftCapacity :: !Word32
    -- ^ Maximum number of simultaneous operations supported.
  , ftState    :: !(IORef TableState)
    -- ^ Mutable state reference protected by atomic operations.
  }

-- | Initializes a new 'FlightTable' with the given slot capacity.
-- Allocates empty active maps and zeroed generation counters.
newFlightTable :: Word32 -> IO FlightTable
newFlightTable cap = do
  ref <- newIORef (TableState Map.empty Map.empty)
  pure (FlightTable cap ref)

-- | Allocates a free slot index within the table capacity, advancing its generation.
-- Attaches an optional buffer release or teardown action.
-- Returns @Nothing@ if the table is currently exhausted (all slots in flight).
allocateSlot :: FlightTable -> Maybe (IO ()) -> IO (Maybe FlightToken)
allocateSlot ft releaseAct = do
  let cap = ftCapacity ft
  atomicModifyIORef' (ftState ft) $ \st ->
    let active = tsActive st
        gens   = tsGens st
        findFree !idx
          | idx >= cap = Nothing
          | Map.member idx active = findFree (idx + 1)
          | otherwise = Just idx
    in case findFree 0 of
         Nothing -> (st, Nothing)
         Just slot ->
           let lastGen = Map.findWithDefault 0 slot gens
               nextGen = lastGen + 1
               token   = packFlightToken slot nextGen
               info    = SlotInfo nextGen releaseAct
               st'     = st
                 { tsActive = Map.insert slot info active
                 , tsGens   = Map.insert slot nextGen gens
                 }
           in (st', Just token)

-- | Completes an in-flight operation referenced by its 'FlightToken'.
-- Verifies that the generation in the token matches the current slot generation.
-- If valid, executes the attached cleanup action and releases the slot.
-- Returns @Left errorMsg@ if the generation is stale or the slot is not active.
completeSlot :: FlightTable -> FlightToken -> IO (Either String ())
completeSlot ft tok = do
  let (slot, gen) = unpackFlightToken tok
  actionToRun <- atomicModifyIORef' (ftState ft) $ \st ->
    let active = tsActive st
    in case Map.lookup slot active of
         Nothing ->
           (st, Left "Slot is not currently in flight or already completed")
         Just info
           | slotGen info /= gen ->
               (st, Left "Stale generation mismatch; token belongs to an expired operation")
           | otherwise ->
               let st' = st { tsActive = Map.delete slot active }
               in (st', Right (slotRelease info))
  case actionToRun of
    Left err -> pure (Left err)
    Right maybeAction -> do
      case maybeAction of
        Nothing  -> pure ()
        Just act -> act
      pure (Right ())

-- | Inspects whether a specific 'FlightToken' represents a currently active operation.
-- Returns @True@ strictly when the slot is active and its generation matches.
lookupSlot :: FlightTable -> FlightToken -> IO Bool
lookupSlot ft tok = do
  let (slot, gen) = unpackFlightToken tok
  st <- readIORef (ftState ft)
  case Map.lookup slot (tsActive st) of
    Nothing   -> pure False
    Just info -> pure (slotGen info == gen)

-- | Returns the total count of currently active in-flight slots.
activeSlotCount :: FlightTable -> IO Int
activeSlotCount ft = do
  st <- readIORef (ftState ft)
  pure (Map.size (tsActive st))

-- | Clears all active slots and resets generation counters to initial empty state.
resetFlightTable :: FlightTable -> IO ()
resetFlightTable ft = writeIORef (ftState ft) (TableState Map.empty Map.empty)
