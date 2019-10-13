{-|
Module      : AlphaStencilDiagrams.State
Description : Interpreting AlphaStencil events into a renderable state.
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencilDiagrams.State
  ( RenderStep(rsState, rsAnimAction)
  , RenderState(rsImage, rsSeg, rsClipSeg, rsPxDivision, rsPxSet, rsProjArea)
  , AnimAction(OutputFrame, SkipToNext)
  , interpretEvents
  )
where

import           Foreign.Storable               ( Storable )

import           AlphaStencil                   ( Seg )
import qualified AlphaStencil.Log              as Log
import           AlphaStencil.Seg               ( ClipSeg
                                                , PxDivision
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Image                          ( I
                                                , Image
                                                , Ix
                                                , J
                                                )
import qualified Image

data RenderStep a
  = RenderStep
    { rsState      :: RenderState a
    , rsAnimAction :: AnimAction
    }
  deriving stock Show

defaultRenderStep :: RenderStep a
defaultRenderStep =
  RenderStep { rsState = defaultRenderState, rsAnimAction = SkipToNext }

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
  deriving stock Show

defaultRenderState :: RenderState a
defaultRenderState = RenderState { rsImage      = Nothing
                                 , rsSeg        = Nothing
                                 , rsClipSeg    = Nothing
                                 , rsPxDivision = Nothing
                                 , rsPxSet      = Nothing
                                 , rsProjArea   = Nothing
                                 }

interpretEvent
  :: (Num a, Storable a) => Log.Event a -> RenderState a -> RenderStep a
interpretEvent event s = case event of
  Log.ENewImage sz ->
    RenderStep defaultRenderState { rsImage = Just $ Image.new sz 0 } SkipToNext
  Log.EStartSeg segm -> RenderStep
    defaultRenderState { rsImage = rsImage s, rsSeg = Just segm }
    OutputFrame
  Log.EClipSegToColumn i _ clipSeg ->
    RenderStep s { rsClipSeg = Just (i, clipSeg) } OutputFrame
  Log.EPxDivision i pxDivision ->
    RenderStep s { rsPxDivision = Just (i, pxDivision) } OutputFrame
  Log.EPxAdd ix value -> RenderStep
    s { rsImage = fmap (addToPixel ix value) (rsImage s)
      , rsPxSet = Just (ix, value)
      }
    OutputFrame
  Log.EProjArea i jMax value ->
    RenderStep s { rsProjArea = Just (i, jMax, value) } OutputFrame

addToPixel :: (Storable a, Num a) => Ix -> a -> Image a -> Image a
addToPixel ix value img = Image.setPixel img ix newValue
  where newValue = value + Image.getPixel img ix

interpretEvents
  :: forall a
   . (Num a, Storable a)
  => Vector (Log.Event a)
  -> Vector (RenderStep a)
interpretEvents = V.prescanl' f defaultRenderStep
 where
  f :: RenderStep a -> Log.Event a -> RenderStep a
  f (RenderStep state _) event = interpretEvent event state
