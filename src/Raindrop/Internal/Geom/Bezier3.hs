{-|
Module      : Raindrop.Internal.Geom.Bezier3
Description : Cubic Bezier curves.
-}
module Raindrop.Internal.Geom.Bezier3 where

import Raindrop.Internal.Geom.Vec (P)

data Bezier3 a
  = Bezier3
    !(P a)
    !(P a)
    !(P a)
    !(P a)
