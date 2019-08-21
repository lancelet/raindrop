{-|
Module      : AlphaStencil.RenderTest
Description : Tests for the AlphaStencil.Render module
-}
{-# LANGUAGE OverloadedStrings #-}
module AlphaStencil.RenderTest (tests) where

import           Hedgehog             ((===))
import qualified Hedgehog             as H

import           Data.Maybe           (catMaybes)
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as V

import           AlphaStencil.Render  (logRenderSegs, renderSegs)
import           AlphaStencil.Seg     (Epsilon (Epsilon), P (P), Seg, seg)
import           Image                (Size (Size), rowMajor)

tests :: H.Group
tests = H.Group "AlphaStencil.Render"
  [ ("render sanity test triangle", unit_render_triangle)
  , ("log render sanity test triangle", unit_logrender_triangle)
  ]

unit_render_triangle :: H.Property
unit_render_triangle = H.withTests 1 $ H.property $ do
  let
    image = renderSegs (Epsilon 1e-5) (Size 4 4) sample_tri
    elems = V.toList . rowMajor $ image
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
    (image, logv) = logRenderSegs (Epsilon 1e-5) (Size 4 4) sample_tri
    elems = V.toList . rowMajor $ image
    expected_elems = [ 0, 0.125, 0.35, 0
                     , 0, 0.625, 0.8, 0
                     , 0, 0.0416667, 0.433333, 0
                     , 0, 0, 0, 0 ]
  H.annotate "elems:"
  H.annotateShow elems
  H.assert $ and $ zipWith (aeq (Epsilon 1e-6)) elems expected_elems
  DV.length logv === 33

aeq :: (Ord a, Num a) => Epsilon a -> a -> a -> Bool
aeq (Epsilon eps) x y = x - eps <= y && x + eps >= y

sample_tri :: [Seg Float]
sample_tri
  = catMaybes
    [ seg eps (P 1 1) (P 3 0.5)
    , seg eps (P 3 0.5) (P 2.5 3)
    , seg eps (P 2.5 3) (P 1 1) ]
  where
    eps = Epsilon 1e-5
