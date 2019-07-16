{-|
Module      : Raindrop.Internal.Interval
Description : Mathematics operations on intervals.
-}
module Raindrop.Internal.Interval
  ( -- * Types
    Interval
  , -- * Functions
    mkInterval
  , clamp
  ) where


-- | Interval between two numbers.
data Interval a = Interval a a deriving (Show, Eq)


-- | Create an interval with correctly-ordered limits.
mkInterval :: (Ord a) => a -> a -> Interval a
mkInterval x y
  | x < y     = Interval x y
  | otherwise = Interval y x
{-# INLINE mkInterval #-}


-- | Clamp a scalar value to an interval.
clamp
  :: (Ord a)
  => Interval a
  -> a
  -> a
clamp (Interval minVal maxVal) x
  | x < minVal = minVal
  | x > maxVal = maxVal
  | otherwise  = x
{-# INLINE clamp #-}
