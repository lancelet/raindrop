{-|
Module      : Raindrop.Internal.Geom.Bezier2
Description : Quadratic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier2 where

import           Raindrop.Internal.Geom.LineSeg (LineSeg (LineSeg))
import           Raindrop.Internal.Geom.Vec     (P, (*^))


data Bezier2 a
  = Bezier2
    !(P a)
    !(P a)
    !(P a)


eval :: (Num a) => Bezier2 a -> a -> P a
eval (Bezier2 pa pb pc) t =
  let
    t2  = t*t
    mt  = 1 - t
    mt2 = mt*mt
  in
    mt2*^pa + 2*mt*t*^pb + t2*^pc
{-# INLINE eval #-}


deriv :: (Num a) => Bezier2 a -> LineSeg a
deriv (Bezier2 pa pb pc) = LineSeg (2*(pb - pa)) (2*(pc - pb))
{-# INLINE deriv #-}
