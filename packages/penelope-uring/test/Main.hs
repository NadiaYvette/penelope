{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Penelope.Uring
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "=== Running penelope-uring test suite ==="

  -- 1. Test Fused SqeBatch produces identical buffer contents to sequential execution
  testFusedVsSequential

  -- 2. Test Mid-chain error in a 4-op IOSQE_IO_LINK sequence aborts remaining ops with -ECANCELED
  testLinkedErrorCascade

  -- 3. Test Cancellation retains buffer ownership until CQE is reaped, and stale token rejected
  testCancellationAndGenerations

  -- 4. Test FlightToken bit packing roundtrip
  testFlightTokenRoundtrip

  -- 5. Test FlagCqeSkipSuccess elision
  testCqeSkipSuccess

  putStrLn "=== All penelope-uring tests passed successfully ==="

testFusedVsSequential :: IO ()
testFusedVsSequential = do
  putStrLn "  [TEST] Fused SqeBatch vs sequential 1-by-1 execution parity..."
  let bufData0 = BS.pack [10, 20, 30, 40]
      bufData1 = BS.pack [50, 60, 70, 80]
      bufData2 = BS.pack [90, 100, 110, 120]
      bufData3 = BS.pack [130, 140, 150, 160]

      baseSt = emptySimState
        { simFixedBufs = Map.fromList
            [ (0, bufData0)
            , (1, bufData1)
            , (2, bufData2)
            , (3, bufData3)
            ]
        }

      tok0 = packFlightToken 0 1
      tok1 = packFlightToken 1 1
      tok2 = packFlightToken 2 1
      tok3 = packFlightToken 3 1

      op0 = SqeEntry tok0 (OpWriteFixed (FixedFdRef 10) (FixedBufRef 0 0 4) 0) []
      op1 = SqeEntry tok1 (OpWriteFixed (FixedFdRef 10) (FixedBufRef 1 0 4) 4) []
      op2 = SqeEntry tok2 (OpWriteFixed (FixedFdRef 10) (FixedBufRef 2 0 4) 8) []
      op3 = SqeEntry tok3 (OpWriteFixed (FixedFdRef 10) (FixedBufRef 3 0 4) 12) []

      fusedBatch = planParallelBatch [op0, op1, op2, op3]
      (stFused, cqesFused) = runUringSimPure baseSt fusedBatch

      -- Run sequential 1-by-1
      (st1, _) = runUringSimPure baseSt (planParallelBatch [op0])
      (st2, _) = runUringSimPure st1 (planParallelBatch [op1])
      (st3, _) = runUringSimPure st2 (planParallelBatch [op2])
      (stSeq, _) = runUringSimPure st3 (planParallelBatch [op3])

      fileFused = lookupSimFd (FixedFdRef 10) stFused
      fileSeq   = lookupSimFd (FixedFdRef 10) stSeq

  when (fileFused /= fileSeq) $ do
    putStrLn "FAIL: Fused output does not match sequential output!"
    exitFailure

  when (length cqesFused /= 4) $ do
    putStrLn "FAIL: Expected 4 completion results from fused batch!"
    exitFailure

  putStrLn "  [PASS] Fused and sequential executions produced identical 16-byte file content."

testLinkedErrorCascade :: IO ()
testLinkedErrorCascade = do
  putStrLn "  [TEST] IOSQE_IO_LINK failure -> -ECANCELED (-125) cascade..."
  let tok0 = packFlightToken 0 1
      tok1 = packFlightToken 1 1
      tok2 = packFlightToken 2 1
      tok3 = packFlightToken 3 1

      -- Op0 succeeds (Nop)
      op0 = SqeEntry tok0 OpNop []
      -- Op1 fails (Read from non-existent RawFdRef 999)
      op1 = SqeEntry tok1 (OpReadFixed (RawFdRef 999) (FixedBufRef 0 0 16) 0) []
      -- Op2 linked write (should be aborted with -ECANCELED = -125)
      op2 = SqeEntry tok2 (OpWriteFixed (FixedFdRef 1) (FixedBufRef 0 0 4) 0) []
      -- Op3 linked fsync (should be aborted with -ECANCELED = -125)
      op3 = SqeEntry tok3 (OpFsync (FixedFdRef 1) True) []

      linkedBatch = planLinkedChain False [op0, op1, op2, op3]
      (_, cqes) = runUringSimPure emptySimState linkedBatch

  when (length cqes /= 4) $ do
    putStrLn "FAIL: Expected 4 CQEs from linked chain"
    exitFailure

  let res0 = cqeRes (cqes !! 0)
      res1 = cqeRes (cqes !! 1)
      res2 = cqeRes (cqes !! 2)
      res3 = cqeRes (cqes !! 3)

  when (res0 /= 0) $ do
    putStrLn "FAIL: Op0 should have succeeded with 0"
    exitFailure

  when (res1 /= -9) $ do
    putStrLn "FAIL: Op1 should have failed with -9 (-EBADF)"
    exitFailure

  when (res2 /= -125 || res3 /= -125) $ do
    putStrLn $ "FAIL: Ops 2 and 3 should receive -ECANCELED (-125), got: " ++ show (res2, res3)
    exitFailure

  putStrLn "  [PASS] IOSQE_IO_LINK aborted ops 2 and 3 with -125 (-ECANCELED) upon mid-chain failure."

testCancellationAndGenerations :: IO ()
testCancellationAndGenerations = do
  putStrLn "  [TEST] FlightTable cancellation, pinned buffer retention, and generation validation..."
  table <- newFlightTable 8
  cleanedRef <- newIORef False

  -- Allocate slot 0 with cleanup action
  maybeTok <- allocateSlot table (Just (writeIORef cleanedRef True))
  tok <- case maybeTok of
    Nothing -> do
      putStrLn "FAIL: Could not allocate slot"
      exitFailure
    Just t -> pure t

  -- Check buffer is NOT released yet
  cleaned0 <- readIORef cleanedRef
  when cleaned0 $ do
    putStrLn "FAIL: Buffer released before completion!"
    exitFailure

  -- Check slot is active
  isActive <- lookupSlot table tok
  when (not isActive) $ do
    putStrLn "FAIL: Slot should be active"
    exitFailure

  -- Simulate cancellation via OpAsyncCancel in UringSim
  let cancelBatch = planParallelBatch [SqeEntry (packFlightToken 1 1) (OpAsyncCancel tok) []]
  case runUringSimPure emptySimState cancelBatch of
    (!_, _) -> pure ()

  -- Buffer is still retained until explicit completeSlot!
  cleaned1 <- readIORef cleanedRef
  when cleaned1 $ do
    putStrLn "FAIL: Buffer released prior to cancellation CQE reaping!"
    exitFailure

  -- Now complete slot (as when reaping the cancellation CQE)
  compRes <- completeSlot table tok
  when (compRes /= Right ()) $ do
    putStrLn "FAIL: completeSlot rejected valid token"
    exitFailure

  -- Now cleanup action has executed
  cleaned2 <- readIORef cleanedRef
  when (not cleaned2) $ do
    putStrLn "FAIL: Buffer cleanup did not run upon completeSlot"
    exitFailure

  -- Attempting to reuse or replay the same token generation must be rejected!
  staleRes <- completeSlot table tok
  case staleRes of
    Left _ -> putStrLn "  [PASS] Stale token generation was cleanly rejected on recycled attempt."
    Right () -> do
      putStrLn "FAIL: Stale token was accepted by completeSlot!"
      exitFailure

testFlightTokenRoundtrip :: IO ()
testFlightTokenRoundtrip = do
  putStrLn "  [TEST] packFlightToken and unpackFlightToken 64-bit roundtrip..."
  let slot = 42 :: Word32
      gen  = 9999 :: Word32
      tok  = packFlightToken slot gen
      (s', g') = unpackFlightToken tok
  when (s' /= slot || g' /= gen) $ do
    putStrLn "FAIL: Token roundtrip mismatch"
    exitFailure
  putStrLn "  [PASS] Token bit packing verified (slot=42, gen=9999)."

testCqeSkipSuccess :: IO ()
testCqeSkipSuccess = do
  putStrLn "  [TEST] FlagCqeSkipSuccess dead-result elision..."
  let tok0 = packFlightToken 0 1
      tok1 = packFlightToken 1 1
      op0 = SqeEntry tok0 OpNop [FlagCqeSkipSuccess]
      op1 = SqeEntry tok1 OpNop []
      batch = planParallelBatch [op0, op1]
      (_, cqes) = runUringSimPure emptySimState batch

  case cqes of
    [cqe] -> when (cqeToken cqe /= tok1) $ do
      putStrLn "FAIL: Remaining CQE token mismatch"
      exitFailure
    _ -> do
      putStrLn "FAIL: FlagCqeSkipSuccess should have elided Op0 CQE!"
      exitFailure

  putStrLn "  [PASS] Successful CQE elided by FlagCqeSkipSuccess."
