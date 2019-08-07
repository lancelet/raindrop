{-|
Module      : AlphaStencil.SegTest
Description : Tests for the Eggshell.SegTest module.
-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
module AlphaStencil.SegTest (tests) where

import           Hedgehog         (Group (Group), MonadGen, Property, assert,
                                   classify, cover, forAll, property, success,
                                   withTests, (===))
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

import           Data.Ratio       (Rational)

import           AlphaStencil.Seg (P (P), Poly (Poly, Vertical), Seg (Seg),
                                   clip, evalPoly, evalPolyInv, oPoly, oSeg,
                                   oSegPixelXRange, oSign, orient, poly,
                                   unClipSeg)
import           Image            (I (I))

tests :: Group
tests = Group "Eggshell.SegTest"
  [ ( "prop_orient_x_values", prop_orient_x_values )
  , ( "prop_orient_sign", prop_orient_sign )
  , ( "prop_orient_poly_evals_ends", prop_orient_poly_evals_ends )
  , ( "prop_poly_evals_end", prop_poly_evals_ends )
  , ( "prop_clip_intersects_pixel_if_present"
    , prop_clip_intersects_pixel_if_present )
  , ( "prop_clip_line_equation_conserved", prop_clip_line_equation_conserved )
  ]

-- Check that orienting a line segment gets the ends ordered correctly.
prop_orient_x_values :: Property
prop_orient_x_values = property $ do
  seg <- forAll $ genSeg (Gen.float (Range.linearFracFrom 0 -10 10))
  let Seg (P px _) (P qx _) = seg
  classify "px < qx" (px < qx)
  classify "px == qx" (px == qx)
  classify "px > qx" (px > qx)
  let Seg (P px' _) (P qx' _) = oSeg (orient seg)
  assert (px' <= qx')

-- Check that orienting a line segment gets the sign correct.
prop_orient_sign :: Property
prop_orient_sign = property $ do
  seg <- forAll $ genSeg (Gen.float (Range.linearFracFrom 0 -10 10))
  let Seg (P px _) (P qx _) = seg
  let sign = oSign (orient seg)
  classify "px < qx" (px < qx)
  classify "px == qx" (px == qx)
  classify "px > qx" (px > qx)
  if px <= qx then sign === -1 else sign === 1

-- Evaluating the Poly version of an oriented line segment at one
-- coordinate of the ends of the segment produces the other coordinate.
prop_orient_poly_evals_ends :: Property
prop_orient_poly_evals_ends = property $ do
  let range = Range.constant -4096 4096 :: Range.Range Rational
  seg <- forAll $ Gen.frequency
                  [ (95, genSeg (Gen.realFrac_ range))
                  , (5, genVerticalSeg (Gen.realFrac_ range)) ]
  let Seg (P px py) (P qx qy) = seg
  let ply = oPoly (orient seg)
  cover 1 "Vertical" (ply == Vertical)
  case ply of
    Vertical -> success -- can't evaluate poly for a vertical line
    Poly _ _ -> do
      evalPoly ply px === py
      evalPoly ply qx === qy
      evalPolyInv ply py === px
      evalPolyInv ply qy === qx

-- Evaluating the Poly version of a line segment at one coordinate at the ends
-- of the segment produces the other coordinate.
prop_poly_evals_ends :: Property
prop_poly_evals_ends = property $ do
  let range = Range.constant -4096 4096 :: Range.Range Rational
  seg <- forAll $ Gen.frequency
                  [ (95, genSeg (Gen.realFrac_ range))
                  , (5, genVerticalSeg (Gen.realFrac_ range)) ]
  let (Seg (P px py) (P qx qy)) = seg
  let ply = poly seg
  cover 1 "Vertical" (ply == Vertical)
  case ply of
    Vertical -> success -- can't evaluate poly for a vertical line
    Poly _ _ -> do
      evalPoly ply px === py
      evalPoly ply qx === qy
      evalPolyInv ply py === px
      evalPolyInv ply qy === qx

-- If a clip for an oriented segment exists then it must completely intersect
-- the pixel it has been clipped for.
prop_clip_intersects_pixel_if_present :: Property
prop_clip_intersects_pixel_if_present = withTests 2000 $ property $ do
  let range = Range.constant -3 3 :: Range.Range Rational
  seg <- forAll $ Gen.frequency
                  [ (70, genSeg (Gen.realFrac_ range))
                  , (30, genVerticalSeg (Gen.realFrac_ range)) ]
  let oseg = orient seg
  let isVertical = poly (oSeg oseg) == Vertical
  cover 1 "Vertical" isVertical
  i <- forAll $ I <$> Gen.int (Range.linearFrom 0 -3 3)
  let (Seg (P px _) (P qx _)) = oSeg oseg
  let
    xn = fromIntegral i :: Rational
    xp = xn + 1
    pOnLeft  = px < xn
    qOnLeft  = qx < xn
    pOnRight = px > xp
    qOnRight = qx > xp
    pInside  = px >= xn && px <= xp
    qInside  = qx >= xn && qx <= xp
    insidePixel = pInside && qInside
    intersectsPixel = not ((pOnLeft && qOnLeft) || (pOnRight && qOnRight))
  if intersectsPixel
    then do
      let (iMin, iMax) = oSegPixelXRange oseg
      assert (i >= iMin)
      assert (i <= iMax)
    else pure ()
  cover 10 "intersects the pixel" intersectsPixel
  cover 1 "vertical and intersects the pixel" (intersectsPixel && isVertical)
  cover 10 "non-vertical and intersects the pixel" (intersectsPixel
                                                    && not isVertical)
  cover 10 "does not intersect the pixel" (not intersectsPixel)
  cover 1 "start inside pixel" pInside
  cover 1 "end inside pixel" qInside
  cover 1 "completely inside the pixel" insidePixel
  cover 1 "completely inside the pixel and not vertical" (insidePixel
                                                          && not isVertical)
  let mClipSeg = clip i oseg
  case mClipSeg of
    Nothing -> assert (not intersectsPixel)
    Just clipSeg -> do
      let Seg (P px' _) (P qx' _) = oSeg (unClipSeg clipSeg)
      assert (px' >= xn)
      assert (px' <= xp)
      assert (qx' >= xn)
      assert (qx' <= xp)

-- Clipping an oriented segment does not change its line equation, and the
-- boundaries of the new clip match the line equation.
prop_clip_line_equation_conserved :: Property
prop_clip_line_equation_conserved = property $ do
  let range = Range.constant 0 16 :: Range.Range Rational
  seg <- forAll $ genSeg (Gen.realFrac_ range)
  i   <- forAll $ I <$> Gen.int (Range.linearFrom 0 0 16)
  let Seg (P px py) (P qx qy) = seg
  let oseg = orient seg
  let ply = oPoly oseg
  let mClipSeg = clip i oseg
  case mClipSeg of
    Nothing -> success
    Just clipSeg -> do
      let Seg (P px' py') (P qx' qy') = oSeg . unClipSeg $ clipSeg
      let ply' = oPoly . unClipSeg $ clipSeg
      ply === ply'
      evalPoly ply' px === py
      evalPoly ply' qx === qy
      evalPolyInv ply' py === px
      evalPolyInv ply' qy === qx
      evalPoly ply' px' === py'
      evalPoly ply' qx' === qy'
      evalPolyInv ply' py' === px'
      evalPolyInv ply' qy' === qx'

-- | Generate a 'Seg' value given a generator for the scalar elements of
-- its points.
genSeg :: MonadGen m => m a -> m (Seg a)
genSeg g = Seg <$> genP g <*> genP g

-- | Generates a 'Seg' that is vertical, given a generator for the scalar
-- elements of its points.
genVerticalSeg :: MonadGen m => m a -> m (Seg a)
genVerticalSeg g = do
  x  <- g
  py <- g
  qy <- g
  pure (Seg (P x py) (P x qy))

-- | Generate a 'P' value given a generator for its values.
genP :: MonadGen m => m a -> m (P a)
genP g = P <$> g <*> g
