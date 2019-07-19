{-|
Module      : Raindrop.Internal.PolynomialRoots
Description : Solving for the real roots of polynomials.

Some algorithms from https://pomax.github.io/bezierinfo

Point in polygon:
  http://geomalgorithms.com/a03-_inclusion.html
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NegativeLiterals #-}
module Raindrop.Internal.PolynomialRoots
  ( -- * Types
    MaybeTwo(M2None, M2One, M2Two)
  , MaybeThree(M3None, M3One, M3Two, M3Three)
    -- * Functions
    -- ** Solutions of polynomial equations
  , solveLinear
  , solveQuadratic
  , solveCubic
    -- ** Processing results
  , maybeTwoToList
  , maybeThreeToList
  -- , filterMaybeTwo
  ) where

import           Raindrop.Internal.Interval (clamp, mkClosedInterval)

-- | Solves @c*x + d = 0@ for @x@.
--
-- No root can be found if 'c' is near to zero.
solveLinear
  :: (Fractional t)
  => (t -> Bool)  -- ^ Test if 't' is near to zero
  -> t            -- ^ @c@ coefficient
  -> t            -- ^ @d@ coefficient
  -> Maybe t      -- ^ root, if 'c' is not near zero
solveLinear nearZero c d
  | nearZero c = Nothing
  | otherwise  = Just (-d/c)
{-# INLINE solveLinear #-}


-- | Zero, one or two things.
data MaybeTwo a
  = M2None
  | M2One a
  | M2Two a a
  deriving (Show, Eq, Functor)


-- | Filter the solutions of a quadratic equation.
{-
filterMaybeTwo :: (a -> Bool) -> MaybeTwo a -> MaybeTwo a
filterMaybeTwo _ M2None = M2None
filterMaybeTwo p s@(M2One x) = if p x then s else M2None
filterMaybeTwo p s@(M2Two x y)
  | p x       = if p y then s else M2One x
  | p y       = M2One y
  | otherwise = M2None
{-# INLINE filterMaybeTwo #-}
-}


-- | Convert a 'MaybeTwo' to a list.
maybeTwoToList :: MaybeTwo a -> [a]
maybeTwoToList M2None      = []
maybeTwoToList (M2One x)   = [x]
maybeTwoToList (M2Two x y) = [x, y]
{-# INLINE maybeTwoToList #-}


-- | Solves for the real roots of @b*x^2 + c*x + d = 0@ in @x@.
--
-- Zero, one or two solutions are possible.
solveQuadratic
  :: (Ord t, Floating t)
  => (t -> Bool)  -- ^ Test if 't' is near to zero
  -> t            -- ^ @b@ coefficient
  -> t            -- ^ @c@ coefficient
  -> t            -- ^ @d@ coefficient
  -> MaybeTwo t   -- ^ Real roots
solveQuadratic nearZero b c d
  | nearZero disc = M2One (-c/twoB)
  | disc < 0      = M2None
  | nearZero b    = maybe M2None M2One $ solveLinear nearZero c d
  | otherwise =
    let s = sqrt disc
    in M2Two ((-c + s)/twoB) ((-c - s)/twoB)
  where
    twoB = 2*b
    disc = c*c - 4*b*d
{-# INLINE solveQuadratic #-}


-- | Zero, one, two or three things.
data MaybeThree a
  = M3None
  | M3One a
  | M3Two a a
  | M3Three a a a
  deriving (Show, Eq, Functor)


-- | Convert a 'MaybeThree' to a list.
maybeThreeToList :: MaybeThree a -> [a]
maybeThreeToList M3None = []
maybeThreeToList (M3One x) = [x]
maybeThreeToList (M3Two x y) = [x, y]
maybeThreeToList (M3Three x y z) = [x, y, z]
{-# INLINE maybeThreeToList #-}


-- | Convert a 'MaybeTwo' to a 'MaybeThree'.
maybeTwoToMaybeThree :: MaybeTwo a -> MaybeThree a
maybeTwoToMaybeThree M2None      = M3None
maybeTwoToMaybeThree (M2One x)   = M3One x
maybeTwoToMaybeThree (M2Two x y) = M3Two x y
{-# INLINE maybeTwoToMaybeThree #-}


-- | Solves for the real roots of @a*x^3 + b*x^2 + c*x + d = 0@ in @x@.
--
-- Zero, one, two or three solutions are possible.
--
-- Cardano's Algorithm. Described here:
--   - https://pomax.github.io/bezierinfo/#extremities
solveCubic
  :: (Ord t, Floating t)
  => (t -> Bool)    -- ^ Test if 't' is near to zero
  -> t              -- ^ @a@ coefficient
  -> t              -- ^ @b@ coefficient
  -> t              -- ^ @c@ coefficient
  -> t              -- ^ @d@ coefficient
  -> MaybeThree t   -- ^ Real roots
solveCubic nearZero a' b' c' d'
  | nearZero a' = maybeTwoToMaybeThree $ solveQuadratic nearZero b' c' d'
  | otherwise =
    let
      cuberoot x
        | x < 0     = -((-x)**(1/3))
        | otherwise = x**(1/3)
      (a, b, c) = (b'/a', c'/a', d'/a')
      p    = (3*b - a*a)/3
      p3   = p/3
      q    = (2*a*a*a - 9*a*b + 27*c)/27
      q2   = q/2
      disc = q2*q2 + p3*p3*p3
      a3   = a/3
      roots
        | nearZero disc =
          let
            u1
              | q2 < 0    = cuberoot (-q2)
              | otherwise = -(cuberoot q2)
            root1 = 2*u1 - a3
            root2 = -u1 - a3
          in
            M3Two root1 root2
        | disc < 0 =
          let
            mp3   = -p/3
            mp33  = mp3*mp3*mp3
            r     = sqrt mp33
            t     = -q/(2*r)
            phi   = acos $ clamp (mkClosedInterval -1 1) t
            crtr  = cuberoot r
            t1    = 2*crtr
            root1 = t1*cos(phi/3) - a3
            root2 = t1*cos((phi + 2*pi)/3) - a3
            root3 = t1*cos((phi + 4*pi)/3) - a3
          in
            M3Three root1 root2 root3
        | otherwise =
          let
            sd    = sqrt disc
            u1    = cuberoot (sd - q2)
            v1    = cuberoot (sd + q2)
            root1 = u1 - v1 - a3
          in
            M3One root1
    in roots
{-# INLINE solveCubic #-}
