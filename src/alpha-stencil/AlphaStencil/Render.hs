{-|
Module      : AlphaStencil.Render
Description : Rendering alpha mask.
-}
module AlphaStencil.Render
  ( renderSegs
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST        (runST)
import           Data.Foldable           (traverse_)
import           Foreign.Storable        (Storable)

import           AlphaStencil.Seg        (Seg, clip, clipSegProjX,
                                          oClipSegAreaInPixel,
                                          oClipSegPixelYRange, oSegPixelXRange,
                                          orient, orientClipped)
import           Image                   (DirLimit (Down, DownTo, UpTo), Image,
                                          Ix (Ix), MImage, Size, getMSize,
                                          loopI, loopJ, newMutable,
                                          unsafeFreeze, unsafeModify)


renderSegs
  :: ( Foldable t
     , RealFrac a, Storable a )
  => Size
  -> t (Seg a)
  -> Image a
renderSegs sz segs = runST $ do
  mimg <- newMutable sz 0
  traverse_ (renderSeg mimg) segs
  unsafeFreeze mimg


renderSeg
  :: ( PrimMonad m
     , RealFrac a, Storable a )
  => MImage (PrimState m) a
  -> Seg a
  -> m ()
renderSeg mimg seg = do
  let
    sz = getMSize mimg
    oseg = orient seg
    (minI, maxI) = oSegPixelXRange oseg
  loopI sz minI (UpTo maxI) $ \i ->
    case clip i oseg of
      Nothing -> error "renderSeg: segment did not cross pixel!"
      Just clipSeg -> do
        let
          oClipSeg = orientClipped clipSeg
          (minJ, maxJ) = oClipSegPixelYRange oClipSeg
        loopJ sz maxJ (DownTo minJ) $ \j -> do
          let
            pxArea = oClipSegAreaInPixel oClipSeg j
          unsafeModify mimg (+ pxArea) (Ix i j)
        let projArea = clipSegProjX clipSeg
        loopJ sz (minJ - 1) Down $ \j ->
          unsafeModify mimg (+ projArea) (Ix i j)
