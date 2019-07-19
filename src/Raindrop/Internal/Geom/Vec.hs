{-|
Module      : Raindrop.Internal.Geom.Vec
Description : Point and vector types.
-}
module Raindrop.Internal.Geom.Vec
  ( -- * Types
    V
  , P
    -- * Functions
  , mkV
  , mkP
  , scalarCross
    -- * Lenses
  , _x
  , _y
    -- * Operators
  , (.-.)
  ) where

import           Control.Lens  ((^.))
import           Linear        (_x, _y)
import qualified Linear
import           Linear.Affine ((.-.))
import qualified Linear.Affine

type V a = Linear.V2 a
type P a = Linear.Affine.Point Linear.V2 a

mkV :: a -> a -> V a
mkV = Linear.V2

mkP :: a -> a -> P a
mkP x y = Linear.Affine.P $ mkV x y

scalarCross :: (Num a) => V a -> V a -> a
scalarCross v q = v^._x*q^._y - v^._y*q^._x
