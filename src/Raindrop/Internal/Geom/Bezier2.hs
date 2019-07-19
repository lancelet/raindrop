{-|
Module      : Raindrop.Internal.Geom.Bezier2
Description : Quadratic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier2 where

import Raindrop.Internal.Geom.Vec (P)

data Bezier2 a
  = Bezier2
    !(P a)
    !(P a)
    !(P a)
