{-|
Module      : Raindrop.Internal.Geom.Bezier3
Description : Cubic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier3
  ( -- * Types
    Bezier3(Bezier3)
    -- * Functions
  , windingNum
  ) where

import           Control.Lens                      ((^.))
import           Linear                            (Epsilon, nearZero)

import           Raindrop.Internal.Geom.Bezier2    (Bezier2 (Bezier2))
import qualified Raindrop.Internal.Geom.Bezier2    as Bezier2
import           Raindrop.Internal.Geom.Vec        (P, V, normalize, p2v,
                                                    scalarCross, v2p, (*^),
                                                    (^+^), (^-^), _y)
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


inBezierParamRange :: (Ord a, Num a) => a -> Bool
inBezierParamRange x = (x >= 0) && (x <= 1)
{-# INLINE inBezierParamRange #-}


tangent :: (Epsilon a, Floating a) => Bezier3 a -> a -> V a
tangent b t = normalize $ p2v $ Bezier2.eval (deriv b) t
{-# INLINE tangent #-}


windingNum :: (Ord a, Floating a, Epsilon a) => Bezier3 a -> P a -> Int
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
        onLeft = (tangent bez t) `scalarCross` (v ^-^ vx) > 0
        vx = p2v $ eval bez t
{-# INLINE windingNum #-}
