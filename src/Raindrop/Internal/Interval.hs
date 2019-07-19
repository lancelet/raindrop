{-|
Module      : Raindrop.Internal.Interval
Description : Mathematics operations on intervals.
-}
module Raindrop.Internal.Interval
  ( -- * Types
    Boundary(Open, Closed)
  , Interval
    -- * Functions
  , mkInterval
  , mkClosedInterval
  , clamp
  , inRange
  ) where


-- | Interval boundary.
data Boundary
  = Open
  | Closed


-- | Interval.
data Interval a
  = Interval
    { start         :: a
    , startBoundary :: Boundary
    , end           :: a
    , endBounary    :: Boundary
    }


-- | Create an interval.
mkInterval :: (a, Boundary) -> (a, Boundary) -> Interval a
mkInterval (c, cb) (d, db) = Interval c cb d db
{-# INLINE mkInterval #-}


-- | Create a closed interval.
mkClosedInterval :: a -> a -> Interval a
mkClosedInterval c d = mkInterval (c, Closed) (d, Closed)
{-# INLINE mkClosedInterval #-}


-- | Clamp a scalar value to an interval.
--
-- The interval is always assumed to be closed in this case.
clamp
  :: (Ord a)
  => Interval a
  -> a
  -> a
clamp (Interval c _ d _) x
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise  = x
  where
    (minVal, maxVal) = if c < d then (c, d) else (d, c)
{-# INLINE clamp #-}


-- | Check if a value is in the range of an interval.
inRange
  :: (Ord a)
  => Interval a
  -> a
  -> Bool
inRange (Interval c cb d db) x = okMinBound && okMaxBound
  where
    okMinBound = case minBoundType of
      Open   -> minVal < x
      Closed -> minVal <= x
    okMaxBound = case maxBoundType of
      Open   -> maxVal > x
      Closed -> maxVal >= x
    (minVal, minBoundType, maxVal, maxBoundType) =
      if c < d then (c, cb, d, db) else (d, db, c, cb)
{-# INLINE inRange #-}
