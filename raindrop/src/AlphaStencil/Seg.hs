{-|
Module      : AlphaStencil.Seg
Description : Line segments.
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencil.Seg
  ( -- * Types
    P(P)
  , Epsilon(Epsilon, unEpsilon)
  , Sign(Plus, Minus)
  , Gradient(Gradient, unGradient)
  , Intercept(Intercept, unIntercept)
  , Poly(Poly)
  , Seg
  , pattern Seg
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

data P a = P !a !a deriving stock (Show)

newtype Epsilon a = Epsilon { unEpsilon :: a }

data Sign = Plus | Minus
  deriving stock Show

newtype Gradient a = Gradient { unGradient :: a }
  deriving stock Show

newtype Intercept a = Intercept { unIntercept :: a }
  deriving stock Show

data Poly a = Poly !(Gradient a) !(Intercept a)
  deriving stock Show

data Seg a = SegC !Sign !(P a) !(P a) !(Poly a)
  deriving stock Show

pattern Seg :: Sign -> P a -> P a -> Poly a -> Seg a
pattern Seg sign p q poly <- SegC sign p q poly
{-# COMPLETE Seg :: Seg #-}

newtype ClipSeg a = ClipSeg { unClipSeg :: Seg a }
  deriving stock Show

data PxDivision a
  = PxDivision
    { pdDxa :: !a
    , pdDxb :: !a
    , pdDyb :: !a
    , pdDyc :: !a
    }
  deriving stock Show

---- Functions

seg
  :: forall a.
     (Ord a, Fractional a)
  => Epsilon a
  -> P a
  -> P a
  -> Maybe (Seg a)
seg (Epsilon eps) p'@(P px' _) q'@(P qx' _)
  | dy >= eps && invGradMag < eps             = Nothing
  | dy < eps  && dx < eps && invGradMag < eps = Nothing
  | otherwise                                 = Just s
  where
    p, q :: P a
    px, py, qx, qy :: a
    sign :: Sign
    (p@(P px py), q@(P qx qy), sign) | px' <= qx' = (p', q', Minus)
                                     | otherwise  = (q', p', Plus)

    dx, dy :: a
    dx = qx - px
    dy = qy - py

    invGradMag, m, c :: a
    invGradMag = abs (dx/dy)
    m = dy/dx
    c = py - m*px

    s :: Seg a
    s = SegC sign p q (Poly (Gradient m) (Intercept c))

pixelXRange :: RealFrac a => Epsilon a -> Seg a -> (I, I)
pixelXRange (Epsilon eps) (Seg _ (P px _) (P qx _) _) = (minI, maxI)
  where
    pf = floor px
    qf = floor qx
    minI | px - fromIntegral pf + 1 < eps = pf + 1
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
    minJ | s - fromIntegral sf + 1 < eps = sf + 1
         | otherwise                     = sf
    maxJ | t - fromIntegral tf < eps = tf - 1
         | otherwise                 = tf

evalPoly :: Num a => Poly a -> a -> a
evalPoly (Poly (Gradient m) (Intercept c)) x = m*x + c

evalPolyInv :: Fractional a => Poly a -> a -> a
evalPolyInv (Poly (Gradient m) (Intercept c)) y = (y - c)/m

clip
  :: forall a. (Ord a, Num a)
  => I
  -> Seg a
  -> ClipSeg a
clip (I i) s@(Seg sign p@(P px _) q@(P qx _) poly)
  | pIn  && qIn  = ClipSeg s
  | pOnL && qIn  = ClipSeg (SegC sign p' q  poly)
  | pIn  && qOnR = ClipSeg (SegC sign p  q' poly)
  | pOnL && qOnR = ClipSeg (SegC sign p' q' poly)
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

pxDivisionArea :: Fractional a => ClipSeg a -> PxDivision a -> a
pxDivisionArea clipSeg (PxDivision dxa dxb dyb dyc) =
  c*(dxa + dxb*dyb/2 + dxb*dyc)
  where
    ClipSeg (Seg sign _ _ _) = clipSeg
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
