{-|
Module      : Raindrop.Internal.Geom.LineSeg
Description : Line segment.
-}
module Raindrop.Internal.Geom.LineSeg
  ( -- * Types
    LineSeg(LineSeg)
    -- * Functions
  , eval
  , bound
  , windingNum
  , distanceTo
  ) where

import           Control.Lens                 ((^.))

import           Raindrop.Internal.Geom.Bound (BBox, mkBBox)
import           Raindrop.Internal.Geom.Vec   (P, distanceA, dot, quadrance,
                                               scalarCross, (*^), (.+^), (.-.),
                                               _y)
import           Raindrop.Internal.Interval   (Interval, clamp, inRange,
                                               mkClosedInterval)


-- | Line segment.
data LineSeg a
  = LineSeg
    !(P a)
    !(P a)


-- | Evaluate a line segment as a parametric curve.
eval :: (Num a) => LineSeg a -> a -> P a
eval (LineSeg pa pb) t = (1-t)*^pa + t*^pb
{-# INLINE eval #-}


-- | Bounding box of a line segment.
bound :: (Ord a, Num a) => LineSeg a -> BBox a
bound (LineSeg p1 p2) = mkBBox p1 (p2 .-. p1)
{-# INLINE bound #-}


-- | True if a line segment is exactly horizontal.
isHorizontal :: (Eq a) => LineSeg a -> Bool
isHorizontal (LineSeg p1 p2) = p1^._y == p2^._y
{-# INLINE isHorizontal #-}


-- | The y interval corresponding to a line segment.
--
-- An upward edge excludes its end. A downward edge excludes its start.
yInterval :: (Ord a) => LineSeg a -> Interval a
yInterval (LineSeg p1 p2) = mkClosedInterval y1 y2
  where
    y1 = p1^._y
    y2 = p2^._y
{-# INLINE yInterval #-}


-- | Horizontal winding number.
windingNum :: (Ord a, Num a) => LineSeg a -> P a -> Int
windingNum ls@(LineSeg p1 p2) p
  | isHorizontal ls                = 0
  | inRange (yInterval ls) (p^._y) = if onLeft then 1 else -1
  | otherwise                      = 0
  where
    onLeft = (p2 .-. p1) `scalarCross` (p .-. p1) > 0
{-# INLINE windingNum #-}


distanceTo :: (Floating a, Ord a) => LineSeg a -> P a -> a
distanceTo (LineSeg p1 p2) p =
  let
    v = p .-. p1
    s = p2 .-. p1
    t = clamp (mkClosedInterval 0 1) $ (v `dot` s) / (quadrance s)
    p' = p1 .+^ (t *^ s)
  in
    distanceA p p'
{-# INLINE distanceTo #-}
