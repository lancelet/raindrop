{-|
Module      : AlphaStencil.Seg
Description : Line segments and pixel intersections.
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencil.Seg
  ( -- * Types
    P(P, ptx, pty)
  , Epsilon(Epsilon, unEpsilon)
  , Sign(Plus, Minus)
  , Gradient(Gradient, unGradient)
  , Intercept(Intercept, unIntercept)
  , Poly(Poly)
  , Seg(segSign, segP, segQ, segPoly)
  , ClipSeg(unClipSeg)
  , PxDivision(PxDivision, pdDxa, pdDxb, pdDyc)
    -- * Functions
  , seg
  , pixelXRange
  , pixelYRange
  , clip
  , pxDivision
  , pxDivisionArea
  , projXArea
  ) where

import           Image (I (I), J (J))

---- Types

-- | Point in 2D.
data P a
  = P
    { ptx :: !a  -- ^ x-coordinate.
    , pty :: !a  -- ^ y-coordinate.
    }
  deriving stock (Eq, Show)

-- | A "small value".
--
-- Epsilon values are (optionally) used in circumstances where floating-point
-- operations may lead to unreliable values. An example is when a float is
-- divided by a very small number; if the number is close enough to zero then
-- the large value produced can have large inaccuracies. To avoid these cases,
-- where appropriate, values that are encountered are compared against this
-- Epsilon value.
newtype Epsilon a = Epsilon { unEpsilon :: a }

-- | Sign of a line segment's contribution to a pixel.
data Sign = Plus | Minus
  deriving stock (Eq, Show)

-- | Gradient of a line segment.
newtype Gradient a = Gradient { unGradient :: a }
  deriving stock (Eq, Show)

-- | y-axis intercept of a line segment.
newtype Intercept a = Intercept { unIntercept :: a }
  deriving stock (Eq, Show)

-- | Polynomial representation of a line segment.
data Poly a = Poly !(Gradient a) !(Intercept a)
  deriving stock (Eq, Show)

-- | Line segment.
--
-- On construction, the x-coordinate of 'segP' is less than the x-coordinate
-- of 'segQ'. Line segments in this representation cannot be vertical.
data Seg a
  = Seg
    { segSign :: !Sign
      -- ^ Sign of contributions of the line segment to pixels below it.
    , segP    :: !(P a)
      -- ^ End of the line segment with lesser x-coordinate.
    , segQ    :: !(P a)
      -- ^ End of the line segment with greater x-coordinate.
    , segPoly :: !(Poly a)
      -- ^ Polynomial representation of the line segment.
    }
  deriving stock (Eq, Show)

-- | Clipped line segment.
--
-- This is a line segment that has been clipped to one pixel column of the
-- image being rendered.
newtype ClipSeg a = ClipSeg { unClipSeg :: Seg a }
  deriving stock (Eq, Show)

-- | Area sub-division of a pixel.
--
-- This type contains 4 scalar values which, along with a corresponding
-- 'ClipSeg', fully describe the area under a line segment as it intersects
-- with a pixel. This is difficult to describe textually; please refer to a
-- diagram.
data PxDivision a
  = PxDivision
    { pdDxa :: !a
    , pdDxb :: !a
    , pdDyb :: !a
    , pdDyc :: !a
    }
  deriving stock (Eq, Show)

-- | Create a 'Seg' from two points if they are not too close to vertical.
seg
  :: forall a.
     (Ord a, Fractional a)
  => Epsilon a      -- ^ Epsilon value to test for verticality.
  -> P a            -- ^ First coordinate of the line segment.
  -> P a            -- ^ Second coordinate of the line segment.
  -> Maybe (Seg a)  -- ^ Line segment.
seg (Epsilon eps) p'@(P px' _) q'@(P qx' _)
  | dx <= eps = Nothing
  | otherwise = Just s
  where
    p, q :: P a
    px, py, qx, qy :: a
    sign :: Sign
    (p@(P px py), q@(P qx qy), sign) | px' <= qx' = (p', q', Minus)
                                     | otherwise  = (q', p', Plus)

    dx, dy :: a
    dx = qx - px
    dy = qy - py

    m = dy/dx
    c = py - m*px

    s :: Seg a
    s = Seg sign p q (Poly (Gradient m) (Intercept c))

-- | Find the pixels occupied by a 'Seg' along the x-axis.
--
-- An epsilon value is used to avoid pixels where the 'Seg' extends less than a
-- very small amount.
pixelXRange
  :: RealFrac a
  => Epsilon a  -- ^ Used to exclude pixels with a very small coverage.
  -> Seg a      -- ^ Segment to compute coverage for.
  -> (I, I)     -- ^ Range of pixels (inclusive) along the x-axis that are
                --   touched by the segment.
pixelXRange (Epsilon eps) (Seg _ (P px _) (P qx _) _) = (minI, maxI)
  where
    pf = floor px
    qf = floor qx
    minI | 1 - px + fromIntegral pf < eps = pf + 1
         | otherwise                      = pf
    maxI | qx - fromIntegral qf < eps = qf - 1
         | otherwise                  = qf

pixelYRange :: (Ord a, RealFrac a) => Epsilon a -> ClipSeg a -> (J, J)
pixelYRange (Epsilon eps) (ClipSeg (Seg _ (P _ py) (P _ qy) _)) = (minJ, maxJ)
  where
    (s, t) | py <= qy  = (py, qy)
           | otherwise = (qy, py)
    sf = floor s
    tf = floor t
    minJ | 1 - s + fromIntegral sf < eps = sf + 1
         | otherwise                     = sf
    maxJ | t - fromIntegral tf < eps = tf - 1
         | otherwise                 = tf

-- | Evaluate a linear polynomial equation at a given x value.
evalPoly :: Num a => Poly a -> a -> a
evalPoly (Poly (Gradient m) (Intercept c)) x = m*x + c

-- | Evaluate the inverse of a linear polynomial equation at a given y value.
evalPolyInv :: Fractional a => Poly a -> a -> a
evalPolyInv (Poly (Gradient m) (Intercept c)) y = (y - c)/m

clip
  :: forall a. (Ord a, Num a)
  => I
  -> Seg a
  -> ClipSeg a
clip (I i) s@(Seg sign p@(P px _) q@(P qx _) poly)
  | pIn  && qIn  = ClipSeg s
  | pOnL && qIn  = ClipSeg (Seg sign p' q  poly)
  | pIn  && qOnR = ClipSeg (Seg sign p  q' poly)
  | pOnL && qOnR = ClipSeg (Seg sign p' q' poly)
  | otherwise = error "clip: segment does not intersect pixel column"
  where
    xn, xp :: a
    xn = fromIntegral i
    xp = xn + 1

    pOnL, pIn, qIn, qOnR :: Bool
    pOnL = px < xn
    pIn  = px >= xn && px <= xp
    qIn  = qx >= xn && qx <= xp
    qOnR = qx > xp

    p', q' :: P a
    p' = P xn (evalPoly poly xn)
    q' = P xp (evalPoly poly xp)

pxDivision
  :: forall a.
     (Ord a, Fractional a)
  => ClipSeg a
  -> J
  -> PxDivision a
pxDivision (ClipSeg (Seg _ p@(P px py) q@(P qx qy) poly)) (J j)
  | nwse      = PxDivision (px' - px) dxb (py' - qy') (qy' - yn)
  | otherwise = PxDivision (qx - qx') dxb (qy' - py') (py' - yn)
  where
    yn, yp :: a
    yn = fromIntegral j
    yp = yn + 1

    nwse, pIn, qIn :: Bool
    nwse = py >= qy
    pIn  = py >= yn && py <= yp
    qIn  = qy >= yn && qy <= yp

    P px' py' | pIn       = p
              | nwse      = P xp yp
              | otherwise = P xn yn
    P qx' qy' | qIn       = q
              | nwse      = P xn yn
              | otherwise = P xp yp

    xp, xn :: a
    xp = evalPolyInv poly yp
    xn = evalPolyInv poly yn

    dxb :: a
    dxb = qx' - px'

pxDivisionArea :: Fractional a => Sign -> PxDivision a -> a
pxDivisionArea sign (PxDivision dxa dxb dyb dyc) =
  c*(dxa + dxb*dyb/2 + dxb*dyc)
  where
    c = case sign of
          Plus  -> 1
          Minus -> -1

projXArea :: Num a => ClipSeg a -> a
projXArea (ClipSeg (Seg sign (P px _) (P qx _) _)) =
  c*(qx - px)
  where
    c = case sign of
          Plus  -> 1
          Minus -> -1
