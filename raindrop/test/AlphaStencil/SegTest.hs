{-|
Module      : AlphaStencil.SegTest
Description : Tests for the AlphaStencil.Seg module.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module AlphaStencil.SegTest
  ( tests
  )
where

import           Hedgehog                       ( (===) )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Control.Exception              ( ErrorCall
                                                  ( ErrorCallWithLocation
                                                  )
                                                , evaluate
                                                , try
                                                )
import           Data.Functor.Identity          ( Identity )
import           Data.Maybe                     ( fromMaybe )
import           Data.Ratio                     ( Rational
                                                , (%)
                                                )

import           AlphaStencil.Seg               ( Epsilon(Epsilon)
                                                , P(P, ptx)
                                                , PxDivision(PxDivision)
                                                , Seg
                                                , Sign(Minus, Plus)
                                                , clip
                                                , pixelXRange
                                                , pixelYRange
                                                , projXArea
                                                , pxDivision
                                                , pxDivisionArea
                                                , seg
                                                , segP
                                                , segPoly
                                                , segQ
                                                , segSign
                                                , unClipSeg
                                                )
import           Image                          ( I(I)
                                                , J(J)
                                                )
import           Image.Loop                     ( inc
                                                , loop
                                                )

tests :: H.Group
tests = H.Group
  "AlphaStencil.Seg"
  [ ("seg end points are correctly ordered"             , prop_seg_end_order)
  , ("pixelXRange sanity check"                         , unit_pixelXRange)
  , ("clipped segments occupy the correct pixel"        , prop_clip_pixel)
  , ("clip errors if a segment doesn't occupy the pixel", unit_clip_error)
  , ("pixelYRange sanity check"                         , unit_pixelYRange)
  , ("pxDivision sanity check (SW->NE)"                 , unit_pxDivision_swne)
  , ("pxDivision sanity check (NW->SE)"                 , unit_pxDivision_nwse)
  , ("pxDivisionArea sanity check"                      , unit_pxDivisionArea)
  , ("projXArea sanity check"                           , unit_projXArea)
  ]

-- | The ends of a created Seg should be ordered so that the P.x value is
-- less than the Q.x value. Vertical segments should be discarded. The sign
-- of the segment should be correct.
prop_seg_end_order :: H.Property
prop_seg_end_order = H.property $ do
  let pGen = genP (Gen.realFrac_ (Range.linearFrac (0 :: Rational) 16))
  a <- H.forAll pGen
  b <- H.forAll pGen
  H.cover 1 "vertical" (ptx a == ptx b)
  H.cover 10 "a.x <= b.x" (ptx a <= ptx b)
  H.cover 10 "a.x > b.x" (ptx a > ptx b)
  let eps         = Epsilon (0 :: Rational)
      resultMaybe = seg eps a b
  if ptx a == ptx b
    then resultMaybe === Nothing  -- discarding vertical segments
    else do
      result <- case resultMaybe of
        Nothing -> H.failure
        Just x  -> pure x
      H.assert (ptx (segP result) <= ptx (segQ result))
      if ptx a <= ptx b
        then do
          segP result === a
          segQ result === b
          segSign result === Minus
        else do
          segP result === b
          segQ result === a
          segSign result === Plus

-- | Examples of computing the x-range of a segment, with and without epsilon
-- effects.
unit_pixelXRange :: H.Property
unit_pixelXRange = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps0     = Epsilon (0 :: Float)
      eps03    = Epsilon (0.03 :: Float)
      seg1     = fromJust $ seg eps0 (P 2.98 1.0) (P 4.46 2.7)
      seg2     = fromJust $ seg eps0 (P 2.95 1.0) (P 4.46 2.7)
      seg3     = fromJust $ seg eps0 (P 2.46 1.0) (P 4.02 2.7)
      seg4     = fromJust $ seg eps0 (P 2.46 1.0) (P 4.05 2.7)
  pixelXRange eps0 seg1 === (I 2, I 4)
  pixelXRange eps0 seg2 === (I 2, I 4)
  pixelXRange eps0 seg3 === (I 2, I 4)
  pixelXRange eps0 seg4 === (I 2, I 4)
  pixelXRange eps03 seg1 === (I 3, I 4)
  pixelXRange eps03 seg2 === (I 2, I 4)
  pixelXRange eps03 seg3 === (I 2, I 3)
  pixelXRange eps03 seg4 === (I 2, I 4)

-- | When we clip a pixel to a column in the image, the pixel must occupy that
-- column, and have the same polynomial equation and sign as the original.
prop_clip_pixel :: H.Property
prop_clip_pixel = H.property $ do
  let scalarGen = Gen.realFrac_ (Range.linearFrac (0 :: Rational) 16)
  segm <- H.forAll $ genSeg (Epsilon 0) scalarGen
  let (minI, maxI) = pixelXRange (Epsilon 0) segm
  loop minI (<= maxI) inc $ \i -> do
    let xn      = fromIntegral i
        xp      = xn + 1
        clipSeg = clip i segm
        xMin    = ptx . segP . unClipSeg $ clipSeg
        xMax    = ptx . segQ . unClipSeg $ clipSeg
    -- check order
    H.assert (xMin <= xMax)
    -- check that we occupy the right pixel
    H.assert (xMin >= xn && xMin <= xp)
    H.assert (xMax >= xn && xMax <= xp)
    -- check that we have the same polynomial equation and sign as the original
    -- segment
    (segPoly . unClipSeg) clipSeg === segPoly segm
    (segSign . unClipSeg) clipSeg === segSign segm

-- | If we try to clip a segment in a column that it doesn't intersect then an
-- error should be produced.
unit_clip_error :: H.Property
unit_clip_error = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps      = Epsilon (0 :: Rational)
      segm     = fromJust $ seg eps (P 5 2) (P 7 3)
  result <- H.evalIO . try . evaluate $ clip (I 3) segm
  case result of
    Left (ErrorCallWithLocation message _) ->
      message === "clip: segment does not intersect pixel column"
    Right q -> do
      H.annotate "Expected an ErrorCall exception:"
      H.annotateShow q
      H.failure

-- | Computing the y-range of a segment, with and without the effect of
-- epsilon.
unit_pixelYRange :: H.Property
unit_pixelYRange = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps0     = Epsilon (0 :: Rational)
      segm1    = fromJust $ seg eps0 (P (3 % 5) 0) (P (12 % 5) 4)
      segm2    = fromJust $ seg eps0 (P (3 % 5) 4) (P (12 % 5) 0)
      clipSeg1 = clip (I 1) segm1
      clipSeg2 = clip (I 1) segm2
  (segP . unClipSeg) clipSeg1 === P 1 (8 % 9)
  (segQ . unClipSeg) clipSeg1 === P 2 (28 % 9)
  pixelYRange eps0 clipSeg1 === (J 0, J 3)
  pixelYRange (Epsilon (2 % 9)) clipSeg1 === (J 1, J 2)
  (segP . unClipSeg) clipSeg2 === P 1 (28 % 9)
  (segQ . unClipSeg) clipSeg2 === P 2 (8 % 9)
  pixelYRange eps0 clipSeg2 === (J 0, J 3)
  pixelYRange (Epsilon (2 % 9)) clipSeg2 === (J 1, J 2)

-- | Check an example of calculating pixel area subdivision for a line segment
-- that runs SW->NE.
unit_pxDivision_swne :: H.Property
unit_pxDivision_swne = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps0     = Epsilon (0 :: Rational)
      segm     = fromJust $ seg eps0 (P (1 % 5) (1 % 5)) (P (4 % 5) (14 % 5))
      clipseg  = clip (I 0) segm
      pd0      = pxDivision clipseg (J 0)
      pd1      = pxDivision clipseg (J 1)
      pd2      = pxDivision clipseg (J 2)
  pd0 === PxDivision (27 % 65) (12 % 65) (4 % 5) (1 % 5)
  pd1 === PxDivision (12 % 65) (3 % 13) 1 0
  pd2 === PxDivision 0 (12 % 65) (4 % 5) 0

-- | Check an example of calculating pixel area subdivision for a line segment
-- that runs NW->SE.
unit_pxDivision_nwse :: H.Property
unit_pxDivision_nwse = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps0     = Epsilon (0 :: Rational)
      segm     = fromJust $ seg eps0 (P (1 % 5) (14 % 5)) (P (4 % 5) (1 % 5))
      clipseg  = clip (I 0) segm
      pd0      = pxDivision clipseg (J 0)
      pd1      = pxDivision clipseg (J 1)
      pd2      = pxDivision clipseg (J 2)
  pd0 === PxDivision (27 % 65) (12 % 65) (4 % 5) (1 % 5)
  pd1 === PxDivision (12 % 65) (3 % 13) 1 0
  pd2 === PxDivision 0 (12 % 65) (4 % 5) 0

-- | Check examples of calculating the area of pixel area subdivisions.
unit_pxDivisionArea :: H.Property
unit_pxDivisionArea = H.withTests 1 $ H.property $ do
  let pd0 =
        PxDivision (27 % 65) (12 % 65) (4 % 5) (1 % 5) :: PxDivision Rational
      pd1 = PxDivision (12 % 65) (3 % 13) 1 0 :: PxDivision Rational
      pd2 = PxDivision 0 (12 % 65) (4 % 5) 0 :: PxDivision Rational
  pxDivisionArea Plus pd0 === 171 % 325
  pxDivisionArea Plus pd1 === 3 % 10
  pxDivisionArea Plus pd2 === 24 % 325
  pxDivisionArea Minus pd0 === -171 % 325
  pxDivisionArea Minus pd1 === -3 % 10
  pxDivisionArea Minus pd2 === -24 % 325

-- | Check example of projected x-area of a segment.
unit_projXArea :: H.Property
unit_projXArea = H.withTests 1 $ H.property $ do
  let fromJust = fromMaybe (error "Just expected")
      eps0     = Epsilon (0 :: Rational)
      segm1    = fromJust $ seg eps0 (P (1 % 5) (1 % 5)) (P (4 % 5) (14 % 5))
      segm2    = fromJust $ seg eps0 (P (4 % 5) (14 % 5)) (P (1 % 5) (1 % 5))
      clipseg1 = clip (I 0) segm1
      clipseg2 = clip (I 0) segm2
  projXArea clipseg1 === -3 % 5
  projXArea clipseg2 === 3 % 5

-- | Generate points given a generator for scalar components.
genP :: H.MonadGen m => m a -> m (P a)
genP g = P <$> g <*> g

-- | Generate 'Seg's given a generator for scalar components.
genSeg
  :: (H.MonadGen m, H.GenBase m ~ Identity, Ord a, Fractional a)
  => Epsilon a
  -> m a
  -> m (Seg a)
genSeg eps g = Gen.just $ seg eps <$> genP g <*> genP g
