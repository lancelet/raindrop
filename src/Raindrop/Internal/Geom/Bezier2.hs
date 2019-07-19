{-|
Module      : Raindrop.Internal.Geom.Bezier2
Description : Quadratic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier2 where

import Raindrop.Internal.Geom.Vec (P, p2v, v2p, (*^), (^+^))


data Bezier2 a
  = Bezier2
    !(P a)
    !(P a)
    !(P a)


eval :: (Num a) => Bezier2 a -> a -> P a
eval (Bezier2 pa pb pc) t =
  let
    va = p2v pa
    vb = p2v pb
    vc = p2v pc

    t2  = t*t
    mt  = 1 - t
    mt2 = mt*mt
  in
    v2p $ mt2*^va ^+^ 2*mt*t*^vb ^+^ t2*^vc
