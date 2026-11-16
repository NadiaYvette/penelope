-- FILE: src/Penelope/Backend/Gimple.hs
{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Penelope.Backend.Gimple
  ( GimpleVal(..)
  , GimpleCarrier(..)
  , emitGimpleComponentRefStore
  , gimplePolyCarrier
  , gimpleBackendPlugin
  ) where

import Data.Kind (Type)
import Data.Primitive.SmallArray (sizeofSmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (Any, Int(..), unsafeCoerce#, word2Int#)

import Polytropos (Record)
import Polytropos.Core (Rep(..), RowRep(..), Row)
import Polytropos.RowMorph (gatherRep)
import Penelope.Core.Syntax (BranchCont(..))
import Penelope.Engine (PolyCarrier(..), Handler(..))
import Penelope.Backend.Plugin (MergeStyle(..), BackendPlugin(..))
import Penelope.Perceus (RTSProfile(..), PlacePlan(..), SizeClass(..))

-- | Symbolic GIMPLE scalar/pointer SSA variable name (e.g. `r_3(D)`, `res_5`).
newtype GimpleVal (a :: Type) = GimpleVal { unGimpleVal :: Text }
  deriving (Show, Eq)

-- | Threads `(ssaVarCounter, bbCounter, memTokenCounter)` to emit valid `-fgimple` SSA
--   with explicit `# .MEM_{k+1} = VDEF <.MEM_k>` virtual memory state transitions.
newtype GimpleCarrier (outs :: Row) = GimpleCarrier
  { runGimpleCarrier
      :: Int -> Int -> Int
      -> (Int, Int, Int, [Text], Any)
  }

-- | Emits an in-place GIMPLE `COMPONENT_REF` + `ARRAY_REF` store (`r->slots[c_idx] = val`)
--   paired with the `.MEM` virtual SSA `VDEF` token update and optional Lean 4 `m_rc == 1` diamond.
emitGimpleComponentRefStore
  :: RTSProfile
  -> PlacePlan
  -> GimpleVal a    -- ^ Source `PolytroposRep*` pointer SSA name (e.g. `r_3(D)`)
  -> Int            -- ^ Canonical slot index `c_idx`
  -> GimpleVal b    -- ^ Scalar/pointer value to store
  -> Int            -- ^ Current SSA variable counter
  -> Int            -- ^ Current Basic Block (`__BB`) counter
  -> Int            -- ^ Current virtual `.MEM` version counter
  -> (Int, Int, Int, [Text], GimpleVal a)
emitGimpleComponentRefStore !rts !plan (GimpleVal !basePtr) !slotIdx (GimpleVal !valName) !ssaC !bbC !memC =
  let !idxText = T.pack (show slotIdx)
  in case (rts, plan) of
       -- Profile 0 (#[freestanding_linear]): Zero `m_rc` check, direct in-place mutation!
       (RTSFreestandingLinear, _) ->
         let !memNext = memC + 1
             !linesOut =
               [ "  // #[freestanding_linear] PlaceInPlaceLinear: zero RC branch"
               , "  // # .MEM_" <> T.pack (show memNext) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> basePtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC, bbC, memNext, linesOut, GimpleVal basePtr)

       -- Profiles 1 & 2 (Lean 4 / Perceus `EReuse`): `if (r->m_rc == 1)` diamond with `__PHI` merge
       (_, PlacePerceusReuse _ (SizeClass !sc)) ->
         let !rcVar    = "rc_"    <> T.pack (show ssaC)
             !cowPtr   = "r_cow_" <> T.pack (show (ssaC + 1))
             !dstPtr   = "r_dst_" <> T.pack (show (ssaC + 2))
             !bbCow    = bbC
             !bbJoin   = bbC + 1
             !memCow   = memC + 1
             !memJoin  = memC + 2
             !memFinal = memC + 3
             !linesOut =
               [ "  // # VUSE <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> rcVar <> " = " <> basePtr <> "->m_rc;"
               , "  if (" <> rcVar <> " == 1)"
               , "    goto __BB(" <> T.pack (show bbJoin) <> ");"
               , "  else"
               , "    goto __BB(" <> T.pack (show bbCow) <> ");"
               , ""
               , "  __BB(" <> T.pack (show bbCow) <> "):"
               , "  // # .MEM_" <> T.pack (show memCow) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> cowPtr <> " = __organ_cow_dup" <> T.pack (show sc) <> " (" <> basePtr <> ");"
               , "  goto __BB(" <> T.pack (show bbJoin) <> ");"
               , ""
               , "  __BB(" <> T.pack (show bbJoin) <> "):"
               , "  " <> dstPtr <> " = __PHI (__BB(" <> T.pack (show (bbCow - 1)) <> "): "
                        <> basePtr <> ", __BB(" <> T.pack (show bbCow) <> "): " <> cowPtr <> ");"
               , "  // # .MEM_" <> T.pack (show memJoin) <> " = PHI <.MEM_"
                        <> T.pack (show memC) <> ", .MEM_" <> T.pack (show memCow) <> ">"
               , "  // # .MEM_" <> T.pack (show memFinal) <> " = VDEF <.MEM_" <> T.pack (show memJoin) <> ">"
               , "  " <> dstPtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC + 3, bbC + 2, memFinal, linesOut, GimpleVal dstPtr)

       -- Fallback: Fresh allocation + in-place store
       (_, _) ->
         let !newPtr   = "r_new_" <> T.pack (show ssaC)
             !memAlloc = memC + 1
             !memStore = memC + 2
             !linesOut =
               [ "  // # .MEM_" <> T.pack (show memAlloc) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
               , "  " <> newPtr <> " = __organ_alloc_rep (" <> basePtr <> ");"
               , "  // # .MEM_" <> T.pack (show memStore) <> " = VDEF <.MEM_" <> T.pack (show memAlloc) <> ">"
               , "  " <> newPtr <> "->slots[" <> idxText <> "] = " <> valName <> ";"
               ]
         in (ssaC + 1, bbC, memStore, linesOut, GimpleVal newPtr)

gimplePolyCarrier :: PolyCarrier GimpleCarrier
gimplePolyCarrier = PolyCarrier
  { pcRoute = \inMap symFrame -> GimpleCarrier $ \ssaC bbC memC ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep GimpleVal Any Any ()
          !outRep         = gatherRep inMap inRep
      in (ssaC, bbC, memC, ["  // Route RowMorph (GIMPLE SSA copy)"], unsafeCoerce# (RowRep outRep))
  , pcLiftPure = \inMap _ (BranchCont k) symFrame -> GimpleCarrier $ \ssaC bbC memC ->
      let !(RowRep inRep) = unsafeCoerce# symFrame :: RowRep GimpleVal Any Any ()
          !opInRep        = gatherRep inMap inRep
          !coercedMid     = unsafeCoerce# (RowRep opInRep) :: Record opOuts
      in runGimpleCarrier (k coercedMid) ssaC bbC memC
  }

-- | Universal `BackendPlugin` for GCC `-fgimple` SSA (`MergePhiNodes`).
gimpleBackendPlugin :: BackendPlugin GimpleVal GimpleCarrier Text
gimpleBackendPlugin = BackendPlugin
  { bpName         = "gcc-gimple-ssa-backend"
  , bpMergeStyle   = MergePhiNodes
  , bpCarrier      = gimplePolyCarrier
  , bpCoreHandlers = \_layout !rts ->
      let mkGimpleHandler :: Handler GimpleCarrier e
          mkGimpleHandler = Handler $ \tag# _opAny inObj contObj ->
            GimpleCarrier $ \ssaC bbC memC ->
              let !(RowRep (Rep _ inArr))   = unsafeCoerce# inObj :: RowRep GimpleVal Any Any ()
                  !(RowRep (Rep _ contArr)) = unsafeCoerce# contObj :: RowRep Any Any Any ()
                  !slotTag = I# (word2Int# tag#)
                  !basePtr = if sizeofSmallArray inArr > 0
                               then unGimpleVal (unsafeCoerce# (indexSmallArray inArr 0) :: GimpleVal Any)
                               else "r_3(D)"
                  !valExpr = "ev_res_" <> T.pack (show ssaC)
                  !memCall = memC + 1
                  !evCall  =
                    [ "  // # .MEM_" <> T.pack (show memCall) <> " = VDEF <.MEM_" <> T.pack (show memC) <> ">"
                    , "  " <> valExpr <> " = ev_vec_2(D)->handlers[" <> T.pack (show slotTag)
                           <> "] (ev_vec_2(D), " <> basePtr <> ");"
                    ]
                  !plan = case rts of
                    RTSFreestandingLinear -> PlaceInPlaceLinear 0 (SizeClass 3)
                    _                     -> PlacePerceusReuse  0 (SizeClass 3)
                  (!ssaC', !bbC', !memC', !storeLines, !dstVal) =
                    emitGimpleComponentRefStore
                      rts plan (GimpleVal basePtr) slotTag (GimpleVal valExpr)
                      (ssaC + 1) bbC memCall
                  !outFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# dstVal]))
              in if sizeofSmallArray contArr == 0
                   then (ssaC', bbC', memC', evCall ++ storeLines, unsafeCoerce# outFrame)
                   else
                     let !(BranchCont k0) = unsafeCoerce# (indexSmallArray contArr 0)
                         (!ssaF, !bbF, !memF, !downLines, !finalOut) =
                           runGimpleCarrier (k0 (unsafeCoerce# outFrame)) ssaC' bbC' memC'
                     in (ssaF, bbF, memF, evCall ++ storeLines ++ downLines, finalOut)
      in unsafeCoerce# (RowRep (Rep 0 (smallArrayFromList (replicate 16 (unsafeCoerce# mkGimpleHandler)))))
  , bpFinalize = \_layout _rts (GimpleCarrier f) ->
      let (_, _, !memFinal, !bodyLines, outObj) = f 4 3 1
          !(RowRep (Rep _ outArr)) = unsafeCoerce# outObj :: RowRep GimpleVal Any outs ()
          !retVal = if sizeofSmallArray outArr > 0
                      then unGimpleVal (unsafeCoerce# (indexSmallArray outArr 0) :: GimpleVal Any)
                      else "r_3(D)"
      in T.unlines $
           [ "struct PolytroposRep * __GIMPLE (ssa)"
           , "penelope_gimple_entry (struct PenelopeEffVec * ev_vec_2, struct PolytroposRep * r_3)"
           , "{"
           , "  __BB(2):"
           ] ++ bodyLines ++
           [ "  // # VUSE <.MEM_" <> T.pack (show memFinal) <> ">"
           , "  return " <> retVal <> ";"
           , "}"
           ]
  }
