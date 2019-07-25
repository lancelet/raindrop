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
  , p2v
  , v2p
  , scalarCross
  , normalize
  , qd
  , dot
  , distance
  , distanceA
  , quadrance
  , norm
    -- * Lenses
  , _x
  , _y
    -- * Operators
  , (*^)
  , (.+^)
  , (.-.)
  ) where

import           Control.Lens  ((^.))
import           Linear        (distance, dot, norm, normalize, qd, quadrance,
                                (*^), _x, _y)
import qualified Linear
import           Linear.Affine (distanceA, (.+^), (.-.))
import qualified Linear.Affine

type V a = Linear.V2 a
type P a = Linear.Affine.Point Linear.V2 a

mkV :: a -> a -> V a
mkV = Linear.V2
{-# INLINE mkV #-}

mkP :: a -> a -> P a
mkP x y = Linear.Affine.P $ mkV x y
{-# INLINE mkP #-}

p2v :: P a -> V a
p2v (Linear.Affine.P v) = v
{-# INLINE p2v #-}

v2p :: V a -> P a
v2p = Linear.Affine.P
{-# INLINE v2p #-}

scalarCross :: (Num a) => V a -> V a -> a
scalarCross v q = v^._x*q^._y - v^._y*q^._x
{-# INLINE scalarCross #-}
