{-|
Module      : AlphaStencil.Seg
Description : Line segments.
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencil.Seg
  ( -- * Types
    P(P)
  , Seg(Seg)
  , OSeg(oSign, oSeg, oPoly)
  , Poly(Poly, Vertical)
  , ClipSeg(unClipSeg)
  , OClipSeg(unOClipSeg)
    -- * Functions
  , orient
  , oSegPixelXRange
  , clip
  , poly
  , evalPoly
  , evalPolyInv
  , orientClipped
  , oClipSegAreaInPixel
  , oClipSegPixelYRange
  , clipSegProjX
  ) where

import           Image (I (I), J (J))

-- | 2D Point.
data P a
  = P
    !a  -- ^ @x@ coordinate of the point.
    !a  -- ^ @y@ coordinate of the point.
  deriving stock (Show)

-- | Line Segment.
data Seg a
  = Seg
    !(P a)  -- ^ First coordinate of the line segment.
    !(P a)  -- ^ Second coordinate of the line segment.
  deriving stock (Show)

-- | Oriented line segment.
--
-- An 'OSeg' differs from a regular 'Seg' in the following ways:
--
--   1. The @x@ value of the first coordinate is less than or equal to the @x@
--      value of the second coordinate.
--
--   2. The sign parameter specifies the orientation of the line segment. It
--      specifies the sign of pixel coverage contributions of the line segment.
--
--   3. The 'OSeg' has polynomial parameters (gradient and intercept)
--      calculated and stored.
data OSeg a
  = OSeg
    { oSign :: !a         -- ^ Sign parameter of the line segment.
    , oSeg  :: !(Seg a)   -- ^ Oriented line segment.
    , oPoly :: !(Poly a)  -- ^ Polynomial representation of the line.
    }
  deriving stock (Show)

-- | Polynomial representation of a line.
data Poly a
    -- | A vertical line (infinite gradient).
  = Vertical
    -- | A polynomial line equation.
  | Poly
    { pGradient  :: !a  -- ^ Gradient.
    , pIntercept :: !a  -- ^ @y@-axis intercept.
    }
  deriving stock (Show, Eq)

-- | Create a polynomial representation of a line segment.
poly :: (Eq a, Fractional a) => Seg a -> Poly a
poly (Seg (P px py) (P qx qy))
  | dx == 0   = Vertical
  | otherwise = Poly m c
  where
    c = py - m*px
    m = dy/dx
    dx = qx - px
    dy = qy - py

-- | Evaluate a linear polynomial at an @x@ value.
evalPoly :: Num a => Poly a -> a -> a
evalPoly (Poly m c) x = m*x + c
evalPoly Vertical _   = error "evalPoly: Vertical line!"

-- | Evaluate the inverse of a linear polynomial at a @y@ value.
evalPolyInv :: Fractional a => Poly a -> a -> a
evalPolyInv (Poly m c) y = (y - c)/m
evalPolyInv Vertical _   = error "evalPolyInv: Vertical line!"

-- | Orient a line segment.
orient :: (Ord a, Fractional a) => Seg a -> OSeg a
orient seg@(Seg p@(P px _) q@(P qx _)) = OSeg sign seg' (poly seg')
  where
    (sign, seg') | px <= qx  = (-1, seg)
                 | otherwise = ( 1, Seg q p)

-- | Range of pixels covered horizontally by an oriented segment.
oSegPixelXRange
  :: RealFrac a
  => OSeg a  -- ^ Oriented segment.
  -> (I, I)  -- ^ @(minI, maxI)@ values for the segment.
oSegPixelXRange (OSeg _ (Seg (P px _) (P qx _)) _) = (minI, maxI)
  where
    minI = floor px
    maxI = floor qx

-- | Clipped line segment.
--
-- A clipped line segment is a line segment that has been clipped to a single
-- column of pixels.
newtype ClipSeg a = ClipSeg { unClipSeg :: OSeg a }

-- | Clip a line segment to a column of pixels.
clip
  :: forall a. (Ord a, Num a)
  => I                  -- ^ Column of the image at which to clip.
  -> OSeg a             -- ^ Oriented segment to clip.
  -> Maybe (ClipSeg a)  -- ^ Clipped segment if it spanned the pixel.
clip (I i) oseg@(OSeg sign seg ply)
  | pInside && qInside  = Just (ClipSeg oseg)
  | pOnLeft && qInside  = Just (ClipSeg (OSeg sign (Seg p' q ) ply))
  | pInside && qOnRight = Just (ClipSeg (OSeg sign (Seg p  q') ply))
  | pOnLeft && qOnRight = Just (ClipSeg (OSeg sign (Seg p' q') ply))
  | otherwise           = Nothing
  where
    xn, xp :: a
    xn = fromIntegral i
    xp = xn + 1

    Seg p@(P px _) q@(P qx _) = seg

    pOnLeft, pInside, qInside, qOnRight :: Bool
    pOnLeft  = px < xn
    pInside  = px >= xn && px <= xp
    qInside  = qx >= xn && qx <= xp
    qOnRight = qx > xp

    p', q' :: P a
    p' = P xn (evalPoly ply xn)
    q' = P xp (evalPoly ply xp)

-- | Oriented clipped line segment.
--
-- An oriented clipped line segment is a clipped line segment within a column
-- of pixels that has been specially-oriented to run NW to SE.
newtype OClipSeg a = OClipSeg { unOClipSeg :: ClipSeg a }

-- | Orient a clipped line segment inside a pixel.
orientClipped :: (Ord a, Num a) => ClipSeg a -> OClipSeg a
orientClipped clipSeg
  | py >= qy  = OClipSeg clipSeg
  | otherwise = OClipSeg clipSeg'
  where
    ClipSeg (OSeg sign (Seg (P px py) (P qx qy)) ply) = clipSeg
    clipSeg' = ClipSeg (OSeg sign (Seg (P px qy) (P qx py)) ply')
    ply' = case ply of
      Vertical -> Vertical
      Poly m _ -> Poly m' c'
        where
          m' = -m
          c' = qy - m'*px


-- | Clipped segment area projected inside a pixel.
oClipSegAreaInPixel :: forall a. (Ord a, Fractional a) => OClipSeg a -> J -> a
oClipSegAreaInPixel clipseg (J j)
  = case ply of
      Vertical -> 0
      Poly _ _ -> sign*(aa + ba + ca)
  where
    OClipSeg (ClipSeg (OSeg sign (Seg (P px py) (P qx qy)) ply)) = clipseg

    yn, yp :: a
    yn = fromIntegral j
    yp = yn + 1

    pInPixel, qInPixel :: Bool
    pInPixel = py >= yn && py <= yp
    qInPixel = qy >= yn && qy <= yp

    (px', py') | pInPixel  = (px, py)
               | otherwise = (evalPolyInv ply yp, yp)
    (qx', qy') | qInPixel  = (qx, qy)
               | otherwise = (evalPolyInv ply yn, yn)

    aa = px' - px
    ba = (qx' - px')*(py' - qy')/2
    ca = (qx' - px')*(qy' - yn)

-- | Y range of a clipped segment.
oClipSegPixelYRange :: RealFrac a => OClipSeg a -> (J, J)
oClipSegPixelYRange (OClipSeg (ClipSeg (OSeg _ (Seg (P _ py) (P _ qy)) _)))
  = (minJ, maxJ)
  where
    minJ = floor qy
    maxJ = floor py

-- | Projection width of the segment onto the x-axis.
clipSegProjX :: Num a => ClipSeg a -> a
clipSegProjX (ClipSeg (OSeg sign (Seg (P px _) (P qx _)) _)) = sign*(qx - px)
