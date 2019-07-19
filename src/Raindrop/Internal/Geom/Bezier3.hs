{-|
Module      : Raindrop.Internal.Geom.Bezier3
Description : Cubic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier3 where

import Raindrop.Internal.Geom.Vec (P, p2v, v2p, (*^), (^+^))


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
