{-|
Module      : Raindrop.Internal.Geom.Bound
Description : Bounding geometry.
-}
module Raindrop.Internal.Geom.Bound
  ( -- * Types
    BBox
    -- * Functions
  , mkBBox
  ) where

import           Control.Lens               ((^.))
import           Linear                     (_x, _y)

import           Raindrop.Internal.Geom.Vec (P, V, mkP, mkV)


-- | Bounding box.
data BBox a
  = BBox
    {-# UNPACK #-} !(P a)  -- ^ Position of a bounding box corner.
    {-# UNPACK #-} !(V a)  -- ^ Size of the bounding box.


-- | Create a bounding box.
--
-- The position and dimensions are organized so that the dimensions are greater
-- than or equal to zero.
mkBBox :: (Ord a, Num a) => P a -> V a -> BBox a
mkBBox p v = BBox (mkP x y) (mkV w h)
  where
    (x, w) | v^._x < 0 = (p^._x-v^._x, -v^._x)
           | otherwise = (p^._x, v^._x)
    (y, h) | v^._y < 0 = (p^._y-v^._y, -v^._y)
           | otherwise = (p^._y, v^._y)
