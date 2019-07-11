{-|
Module      : AlphaStencil.Render
Description : Rendering alpha mask.
-}
module AlphaStencil.Render
  ( -- * Functions
    renderSegs
  , logRenderSegs
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Control.Monad.ST        (runST)
import           Data.Foldable           (traverse_)
import           Data.Vector             (Vector)
import           Foreign.Storable        (Storable)

import           AlphaStencil.Log        (Event (EventAddToPixel, EventPixelAreaDivision, EventPixelLineClip, EventPixelProjectX, EventStartSeg),
                                          Logger, mkLogger, noOpLogger)
import           AlphaStencil.Seg        (Epsilon, Seg, clip, pixelXRange,
                                          pixelYRange, projXArea, pxDivision,
                                          pxDivisionArea, pxDivisionArea)
import           Image                   (DirLimit (Down, DownTo, UpTo), Image,
                                          Ix (Ix), MImage, Size, getMSize,
                                          loopI, loopJ, newMutable,
                                          unsafeFreeze, unsafeModify)


renderSegs
  :: ( Foldable t
     , RealFrac a, Storable a )
  => Epsilon a
  -> Size
  -> t (Seg a)
  -> Image a
renderSegs eps sz segs = runST $ do
  mimg <- newMutable sz 0
  traverse_ (renderSeg noOpLogger eps mimg) segs
  unsafeFreeze mimg

logRenderSegs
  :: ( Foldable t
     , RealFrac a, Storable a )
  => Epsilon a
  -> Size
  -> t (Seg a)
  -> (Image a, Vector (Event a))
logRenderSegs eps sz segs = runST $ do
  mimg <- newMutable sz 0
  (logEvent, retrieveLog) <- mkLogger
  traverse_ (renderSeg logEvent eps mimg) segs
  img <- unsafeFreeze mimg
  eventVector <- retrieveLog
  pure (img, eventVector)

renderSeg
  :: ( PrimMonad m
     , RealFrac a, Storable a )
  => Logger m a
  -> Epsilon a
  -> MImage (PrimState m) a
  -> Seg a
  -> m ()
renderSeg logEvent eps mimg seg = do
  logEvent (EventStartSeg seg)
  let
    sz = getMSize mimg
    (minI, maxI) = pixelXRange eps seg
  loopI sz minI (UpTo maxI) $ \i -> do
    let
      clipSeg = clip i seg
      (minJ, maxJ) = pixelYRange eps clipSeg
    logEvent (EventPixelLineClip clipSeg)
    loopJ sz maxJ (DownTo minJ) $ \j -> do
      let
        divs = pxDivision clipSeg j
        area = pxDivisionArea clipSeg divs
      logEvent (EventPixelAreaDivision (Ix i j) clipSeg divs)
      unsafeModify mimg (+ area) (Ix i j)
      logEvent (EventAddToPixel (Ix i j) area)
    let projArea = projXArea clipSeg
    loopJ sz (minJ - 1) Down $ \j -> do
      unsafeModify mimg (+ projArea) (Ix i j)
      logEvent (EventPixelProjectX (Ix i j) clipSeg)
      logEvent (EventAddToPixel (Ix i j) projArea)
