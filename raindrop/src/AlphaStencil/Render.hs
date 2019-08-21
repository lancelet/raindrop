{-|
Module      : AlphaStencil.Render
Description : Rendering alpha mask.
-}
{-# LANGUAGE FlexibleContexts #-}
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

import           AlphaStencil.Log        (Event (EClipSegToColumn, ENewImage,
                                                 EProjArea, EPxAdd, EPxDivision,
                                                 EStartSeg),
                                          Logger (NoOp, Record), logMessage,
                                          mkRecordingLogger, retrieveLog)
import           AlphaStencil.Seg        (Epsilon, Seg, clip, pixelXRange,
                                          pixelYRange, projXArea, pxDivision,
                                          pxDivisionArea, segSign, unClipSeg)
import           Image                   (Image, Ix (Ix), Size, unsafeFreeze)
import           Image.Loop              (dec, inc, loop)
import           Image.Mutable           (MImage)
import qualified Image.Mutable           as M

renderSegs
  :: ( Foldable t
     , RealFrac a, Storable a, Show a )
  => Epsilon a
  -> Size
  -> t (Seg a)
  -> Image a
renderSegs eps sz segs = runST $ do
  mImg <- M.new sz 0
  traverse_ (renderSeg NoOp eps mImg) segs
  unsafeFreeze mImg

logRenderSegs
  :: ( Foldable t
     , RealFrac a, Storable a, Show a )
  => Epsilon a
  -> Size
  -> t (Seg a)
  -> (Image a, Vector (Event a))
logRenderSegs eps sz segs = runST $ do
  recordingLogger <- mkRecordingLogger
  let logger = Record recordingLogger
  mImg <- M.new sz 0
  logMessage logger $ ENewImage sz
  traverse_ (renderSeg logger eps mImg) segs
  logEvents <- retrieveLog recordingLogger
  image <- unsafeFreeze mImg
  pure (image, logEvents)

renderSeg
  :: ( PrimMonad m
     , RealFrac a, Storable a, Show a )
  => Logger (Event a) m
  -> Epsilon a
  -> MImage (PrimState m) a
  -> Seg a
  -> m ()
renderSeg logger eps mImg seg = do
  logMessage logger $ EStartSeg seg
  let
    (minI, maxI) = pixelXRange eps seg
  loop minI (<= maxI) inc $ \i -> do
    let
      clipSeg = clip i seg
      (minJ, maxJ) = pixelYRange eps clipSeg
    logMessage logger $ EClipSegToColumn i clipSeg
    loop maxJ (>= minJ) dec $ \j -> do
      let
        ix = Ix i j
        divs = pxDivision clipSeg j
        sign = segSign . unClipSeg $ clipSeg
        area = pxDivisionArea sign divs
      M.unsafeModify mImg (+ area) ix
      logMessage logger $ EPxDivision ix divs
      logMessage logger $ EPxAdd ix area
    let
      projArea = projXArea clipSeg
    logMessage logger $ EProjArea i projArea
    loop (minJ - 1) (>= 0) dec $ \j -> do
      let
        ix = Ix i j
      M.unsafeModify mImg (+ projArea) ix
      logMessage logger $ EPxAdd ix projArea
