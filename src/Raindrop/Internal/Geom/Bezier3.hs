{-|
Module      : Raindrop.Internal.Geom.Bezier3
Description : Cubic Bezier curves.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Raindrop.Internal.Geom.Bezier3
  ( -- * Types
    Bezier3(Bezier3)
    -- * Functions
  , windingNum
  , distanceTo
  ) where

import           Control.Lens                      ((^.))
import           Data.Massiv.Array                 (Array, Comp (Seq), Ix1,
                                                    Sz (Sz), U, Unbox, ifoldlS,
                                                    makeArray, (!))
import           Linear                            (Epsilon, nearZero)

import           Raindrop.Internal.Geom.Bezier2    (Bezier2 (Bezier2))
import qualified Raindrop.Internal.Geom.Bezier2    as Bezier2
import           Raindrop.Internal.Geom.Vec        (P, V, distance, normalize,
                                                    p2v, qd, scalarCross, v2p,
                                                    (*^), (^+^), (^-^), _y)
import           Raindrop.Internal.PolynomialRoots (filterMaybeThree,
                                                    solveCubic)


data Bezier3 a
  = Bezier3
    !(P a)
    !(P a)
    !(P a)
    !(P a)


eval :: (Num a) => Bezier3 a -> a -> P a
eval (Bezier3 pa pb pc pd) t =
  let
    va = p2v pa
    vb = p2v pb
    vc = p2v pc
    vd = p2v pd

    t2  = t*t
    t3  = t2*t
    mt  = 1 - t
    mt2 = mt*mt
    mt3 = mt2*mt
  in
    v2p $ mt3*^va ^+^ 3*mt2*t*^vb ^+^ 3*mt*t2*^vc ^+^ t3*^vd
{-# INLINE eval #-}


deriv :: (Num a) => Bezier3 a -> Bezier2 a
deriv (Bezier3 pa pb pc pd) = Bezier2 pa' pb' pc'
  where
    va = p2v pa
    vb = p2v pb
    vc = p2v pc
    vd = p2v pd

    pa' = v2p $ 3*^(vb ^-^ va)
    pb' = v2p $ 3*^(vc ^-^ vb)
    pc' = v2p $ 3*^(vd ^-^ vc)
{-# INLINE deriv #-}


{-
This is a bit of a hack. For this to work reliably, I will probably have to
split Beziers in advance so that they don't extend beyond a certain number
of pixels.
-}
inBezierParamRange :: (Ord a, Fractional a) => a -> Bool
inBezierParamRange t = (t >= -0.000001) && (t <= 1.000001)
{-# INLINE inBezierParamRange #-}


tangent :: (Epsilon a, Floating a) => Bezier3 a -> a -> V a
tangent b t = normalize $ p2v $ Bezier2.eval (deriv b) t
{-# INLINE tangent #-}


windingNum :: (Ord a, RealFrac a, Floating a, Epsilon a) => Bezier3 a -> P a -> Int
windingNum bez@(Bezier3 pa pb pc pd) p = sum $ fmap wn ts
  where
    v  = p2v p
    y  = p^._y
    ya = pa^._y - y
    yb = pb^._y - y
    yc = pc^._y - y
    yd = pd^._y - y

    a = -ya + 3*yb - 3*yc + yd
    b = 3*ya - 6*yb + 3*yc
    c = -3*ya + 3*yb
    d = ya

    ts' = solveCubic nearZero a b c d
    ts = filterMaybeThree inBezierParamRange ts'

    wn t = if onLeft then 1 else -1
      where
        onLeft = tangent bez t `scalarCross` (v ^-^ vx) > 0
        vx = p2v $ eval bez t
{-# INLINE windingNum #-}


data MinIdxSemi a = MinIdxSemi !Ix1 !a
instance Ord a => Semigroup (MinIdxSemi a) where
  (<>) (MinIdxSemi i1 x1) (MinIdxSemi i2 x2)
    | x1 <= x2  = MinIdxSemi i1 x1
    | otherwise = MinIdxSemi i2 x2


minIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Array U Ix1 a -> Ix1
minIndexBy cmp array = fst $ ifoldlS fn z array
  where
    z = (0, array ! 0)
    fn c@(_, currentMin) i val
      | cmp val currentMin == LT = (i, val)
      | otherwise                =  c


distanceTo :: forall a. (Unbox a, Floating a, Ord a) => Int -> a -> Bezier3 a -> P a -> a
distanceTo nTabulatedPoints err bez p =
  let
    ptTable :: Array U Ix1 (P a)
    ptTable = makeArray Seq (Sz nTabulatedPoints)
              $ \i ->
                  let
                    t = fromIntegral i / fromIntegral (nTabulatedPoints - 1)
                  in
                    eval bez t
    dt :: a
    dt = 1 / fromIntegral (nTabulatedPoints - 1)
  in
    let
      cmpDistance :: P a -> P a -> Ordering
      cmpDistance p1 p2 = compare (qd p p1) (qd p p2)

      minIx :: Ix1
      minIx = minIndexBy cmpDistance ptTable

      startBracket :: (a, a)
      startBracket
        | minIx == 0 = (0, dt)
        | minIx == (nTabulatedPoints - 1) = (1-dt, 1)
        | otherwise = (dt * fromIntegral (minIx-1), dt * fromIntegral (minIx+1))

      bisect :: (a, a) -> a
      bisect (ta, tb)
        | abs (da - db) < err = tmid
        | da < db             = bisect (ta, tmid)
        | otherwise           = bisect (tmid, tb)
        where
          tmid = (ta + tb) / 2
          da = qd p (eval bez ta)
          db = qd p (eval bez tb)
    in
      distance p (eval bez (bisect startBracket))
