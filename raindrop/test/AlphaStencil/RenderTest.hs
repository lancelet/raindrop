{-|
Module      : AlphaStencil.RenderTest
Description : Tests for the AlphaStencil.Render module
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module AlphaStencil.RenderTest (tests) where

import           Hedgehog              ((===))
import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import           Data.Functor.Identity (Identity)
import           Data.List             (sort, zipWith)
import           Data.Maybe            (catMaybes, mapMaybe)
import qualified Data.Vector           as DV
import qualified Data.Vector.Storable  as V

import           AlphaStencil.Log      (Event (EClipSegToColumn, ENewImage,
                                               EPxAdd, EStartSeg))
import           AlphaStencil.Render   (logRenderSegs, renderSegs)
import           AlphaStencil.Seg      (Epsilon (Epsilon), P (P), Seg,
                                        pixelXRange, seg)
import           Image                 (I, Ix, Size (Size))
import qualified Image

tests :: H.Group
tests = H.Group "AlphaStencil.Render"
  [ ("render sanity test triangle", unit_render_triangle)
  , ("log render sanity test triangle", unit_logrender_triangle)
  , ("ENewImage events must have correct image size", prop_ENewImage)
  , ("EStartSeg events must occur for every segment", prop_EStartSeg)
  , ("EClipSegToColumn must output an event for every column touched by each "
     <> "segment", prop_EClipSegToColumn)
  , ("EPxAdd should be able to reconstruct the image", prop_EPxAdd)
  ]

unit_render_triangle :: H.Property
unit_render_triangle = H.withTests 1 $ H.property $ do
  let
    image = renderSegs (Epsilon 1e-5) (Size 4 4) sampleTri
    elems = V.toList . Image.rowMajor $ image
    expected = [ 0, 0.125, 0.35, 0
               , 0, 0.625, 0.8, 0
               , 0, 0.0416667, 0.433333, 0
               , 0, 0, 0, 0 ]
  H.annotate "elems:"
  H.annotateShow elems
  H.assert $ and $ zipWith (aeq (Epsilon 1e-6)) elems expected

unit_logrender_triangle :: H.Property
unit_logrender_triangle = H.withTests 1 $ H.property $ do
  let
    (image, logv) = logRenderSegs (Epsilon 1e-5) (Size 4 4) sampleTri
    elems = V.toList . Image.rowMajor $ image
    expected_elems = [ 0, 0.125, 0.35, 0
                     , 0, 0.625, 0.8, 0
                     , 0, 0.0416667, 0.433333, 0
                     , 0, 0, 0, 0 ]
  H.annotate "elems:"
  H.annotateShow elems
  H.assert $ and $ zipWith (aeq (Epsilon 1e-6)) elems expected_elems
  DV.length logv === 33

-- | The size returned by the ENewImage event must match the image sized passed
-- to logRenderSegs.
prop_ENewImage :: H.Property
prop_ENewImage = H.property $ do
  w <- H.forAll $ Gen.integral (Range.linear 0 16)
  h <- H.forAll $ Gen.integral (Range.linear 0 16)
  let
    sz = Size w h
    (_, logv) = logRenderSegs (Epsilon 1e-5) sz sampleTri

    getSize :: Event Float -> Maybe Size
    getSize event = case event of
      ENewImage sz' -> Just sz'
      _             -> Nothing

    sizes = mapMaybe getSize (DV.toList logv)

  sizes === [sz]

-- | Each segment must output an EStartSeg event.
prop_EStartSeg :: H.Property
prop_EStartSeg = H.property $ do
  w <- H.forAll $ Gen.integral (Range.linear 1 16)
  h <- H.forAll $ Gen.integral (Range.linear 1 16)
  let sz = Size w h
  segList <- H.forAll $ Gen.list (Range.linear 0 50) (genFloatSegInSize sz)
  let
    (_, logv) = logRenderSegs (Epsilon 1e-5) sz segList

    getStartSegSeg :: Event Float -> Maybe (Seg Float)
    getStartSegSeg event = case event of
      EStartSeg sseg -> Just sseg
      _              -> Nothing

    startedSegs = mapMaybe getStartSegSeg (DV.toList logv)

  startedSegs === segList

-- | For each segment and each column intersected by that segment,
-- an EClipSegToColumn event must be output.
prop_EClipSegToColumn :: H.Property
prop_EClipSegToColumn = H.property $ do
  w <- H.forAll $ Gen.integral (Range.linear 1 16)
  h <- H.forAll $ Gen.integral (Range.linear 1 16)
  let sz = Size w h
  segList <- H.forAll $ Gen.list (Range.linear 0 5) (genFloatSegInSize sz)
  let
    (_, logv) = logRenderSegs (Epsilon 1e-5) sz segList

    getClipSeg :: Event Float -> Maybe (I, Seg Float)
    getClipSeg event = case event of
      EClipSegToColumn i segm _ -> Just (i, segm)
      _                         -> Nothing

    clippedSegs = sort $ mapMaybe getClipSeg (DV.toList logv)

    expected =
      sort
      [ (i, segm)
      | segm <- segList
      , let (iMin, iMax) = pixelXRange (Epsilon 1e-5) segm
      , i <- [iMin .. iMax] ]

  expected === clippedSegs

-- | Applying the EPxAdd events to a blank image should give us the same image
-- as we get from the rendering process.
prop_EPxAdd :: H.Property
prop_EPxAdd = H.property $ do
  w <- H.forAll $ Gen.integral (Range.linear 1 16)
  h <- H.forAll $ Gen.integral (Range.linear 1 16)
  let sz = Size w h
  segList <- H.forAll $ Gen.list (Range.linear 0 5) (genFloatSegInSize sz)
  let
    (image, logv) = logRenderSegs (Epsilon 1e-5) sz segList

    getPxAdd :: Event Float -> Maybe (Ix, Float)
    getPxAdd event = case event of
      EPxAdd ix value -> Just (ix, value)
      _               -> Nothing

    pxAdds = mapMaybe getPxAdd (DV.toList logv)

    addToPixel img ix value = Image.setPixel img ix (value + value')
      where
        value' = Image.getPixel img ix

    resultImage =
      foldl
      (\img (ix, value) -> addToPixel img ix value)
      (Image.new sz 0)
      pxAdds

  resultImage === image

aeq :: (Ord a, Num a) => Epsilon a -> a -> a -> Bool
aeq (Epsilon eps) x y = x - eps <= y && x + eps >= y

sampleTri :: [Seg Float]
sampleTri
  = catMaybes
    [ seg eps (P 1 1) (P 3 0.5)
    , seg eps (P 3 0.5) (P 2.5 3)
    , seg eps (P 2.5 3) (P 1 1) ]
  where
    eps = Epsilon 1e-5

genFloatSegInSize
  :: ( H.MonadGen m, H.GenBase m ~ Identity )
  => Size
  -> m (Seg Float)
genFloatSegInSize (Size w h) = do
  let
    wf = fromIntegral w
    hf = fromIntegral h
    genX = Gen.float (Range.linearFrac 0 wf)
    genY = Gen.float (Range.linearFrac 0 hf)
    genP = P <$> genX <*> genY
  Gen.just $ seg (Epsilon 1e-5) <$> genP <*> genP
