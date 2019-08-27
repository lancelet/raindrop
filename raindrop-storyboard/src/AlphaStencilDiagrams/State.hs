{-|
Module      : AlphaStencilDiagrams.State
Description : Interpreting AlphaStencil events into a renderable state.
-}
{-# LANGUAGE DerivingStrategies #-}
module AlphaStencilDiagrams.State where

import           Foreign.Storable (Storable)

import           AlphaStencil     (Seg)
import qualified AlphaStencil.Log as Log
import           AlphaStencil.Seg (ClipSeg, PxDivision)
import           Image            (I, Image, Ix, J)
import qualified Image            as Image

data RenderState a
  = RenderState
    { rsImage      :: Maybe (Image a)
    , rsSeg        :: Maybe (Seg a)
    , rsClipSeg    :: Maybe (I, ClipSeg a)
    , rsPxDivision :: Maybe (Ix, PxDivision a)
    , rsPxSet      :: Maybe (Ix, a)
    , rsProjArea   :: Maybe (I, J, a)
    }
  deriving stock Show

data AnimAction
  = OutputFrame
  | SkipToNext

defaultRenderState :: RenderState a
defaultRenderState =
  RenderState
  { rsImage      = Nothing
  , rsSeg        = Nothing
  , rsClipSeg    = Nothing
  , rsPxDivision = Nothing
  , rsPxSet      = Nothing
  , rsProjArea   = Nothing
  }

interpretEvent
  :: (Num a, Storable a)
  => Log.Event a
  -> RenderState a
  -> (RenderState a, AnimAction)
interpretEvent event s = case event of
  Log.ENewImage sz ->
    ( defaultRenderState { rsImage = Just $ Image.new sz 0 }, SkipToNext )
  Log.EStartSeg segm ->
    ( defaultRenderState
      { rsImage = rsImage s
      , rsSeg   = Just segm }
    , OutputFrame )
  Log.EClipSegToColumn i _ clipSeg ->
    ( s { rsClipSeg = Just (i, clipSeg) }, OutputFrame )
  Log.EPxDivision i pxDivision ->
    ( s { rsPxDivision = Just (i, pxDivision) }, OutputFrame )
  Log.EPxAdd ix value ->
    ( s { rsPxSet = Just (ix, value) }, OutputFrame )
  Log.EProjArea i jMax value ->
    ( s { rsProjArea = Just (i, jMax, value) }, OutputFrame )
