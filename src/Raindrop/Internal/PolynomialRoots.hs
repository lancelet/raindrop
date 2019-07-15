{-|
Module      : Raindrop.Internal.PolynomialRoots
Description : Finding roots of polynomials.

Some algorithms from https://pomax.github.io/bezierinfo

Point in polygon:
  http://geomalgorithms.com/a03-_inclusion.html
-}

module Raindrop.Internal.PolynomialRoots where

import Linear (Epsilon, nearZero)


-- | Solves @c*x + d = 0@ for @x@.
--
-- No root can be found if 'nearZero c'.
solveLinear
  :: (Epsilon t, Fractional t)
  => t        -- ^ @c@ coefficient
  -> t        -- ^ @d@ coefficient
  -> Maybe t  -- ^ root, if 'not (nearZero c)'
solveLinear c d
  | nearZero c = Nothing
  | otherwise  = Just (-d/c)
{-# INLINE solveLinear #-}
