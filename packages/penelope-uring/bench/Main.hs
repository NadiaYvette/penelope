{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString as BS
import Penelope.Uring
import Test.Tasty.Bench

-- Prepares 32 independent write operations into fixed FD slots.
mk32Ops :: [SqeEntry]
mk32Ops =
  [ SqeEntry
      (packFlightToken (fromIntegral i) 1)
      (OpWriteFixed (FixedFdRef 1) (FixedBufRef 0 0 8) (fromIntegral (i * 8)))
      []
  | i <- [0 .. 31 :: Int]
  ]

-- Base state pre-populating buffer 0 with 8 bytes.
baseState :: SimState
baseState = registerSimBuf 0 (BS.pack [1, 2, 3, 4, 5, 6, 7, 8]) emptySimState

-- Fused 32-op batch submitted in 1 enter.
benchFused32 :: [SqeEntry] -> SimState
benchFused32 ops =
  let batch = planParallelBatch ops
      (st, _) = runUringSimPure baseState batch
  in st

-- Sequential 32 submissions simulating 32 separate enter calls.
benchIndividual32 :: [SqeEntry] -> SimState
benchIndividual32 ops = foldl step baseState ops
  where
    step !st e =
      let (st', _) = runUringSimPure st (planParallelBatch [e])
      in st'

-- Linked WAL chain: 1 write + 1 fsync fused with FlagIoLink.
benchLinkedWalChain :: (SqeEntry, SqeEntry) -> SimState
benchLinkedWalChain (wOp, sOp) =
  let batch = planLinkedChain False [wOp, sOp]
      (st, _) = runUringSimPure baseState batch
  in st

walPair :: (SqeEntry, SqeEntry)
walPair =
  ( SqeEntry (packFlightToken 0 1) (OpWriteFixed (FixedFdRef 1) (FixedBufRef 0 0 8) 0) []
  , SqeEntry (packFlightToken 1 1) (OpFsync (FixedFdRef 1) True) []
  )

main :: IO ()
main = defaultMain
  [ bgroup "penelope-uring"
      [ bench "fused_batch_32_ops" $ whnf benchFused32 mk32Ops
      , bench "individual_32_enters" $ whnf benchIndividual32 mk32Ops
      , bench "linked_wal_chain" $ whnf benchLinkedWalChain walPair
      ]
  ]
