{-|
Module      : Raindrop.Path1
Description : Initial path description language.
-}
module Raindrop.Path1 where

import           Data.Massiv.Array              (Unbox)
import           Linear.Epsilon                 (Epsilon)

import           Raindrop.Internal.Geom.Bezier3 (Bezier3 (Bezier3))
import qualified Raindrop.Internal.Geom.Bezier3 as Bezier3
import           Raindrop.Internal.Geom.LineSeg (LineSeg (LineSeg))
import qualified Raindrop.Internal.Geom.LineSeg as LineSeg
import           Raindrop.Internal.Geom.Vec     (P)


data Path a
  = Path
    (P a)
    [PathCommand a]


data PathCommand a
  = LineTo (P a)
  | CurveTo (P a) (P a) (P a)


data SDFComponent a
  = SDFComponent
    { windingNum :: P a -> Int
    , distanceTo :: P a -> a }


pathToSDF :: (Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> [SDFComponent a]
pathToSDF (Path _ []) = []
pathToSDF (Path o (c:cs)) = s : pathToSDF (Path o' cs)
  where
    (s, o') = case c of
      LineTo p ->
        (SDFComponent
         { windingNum = LineSeg.windingNum (LineSeg o p)
         , distanceTo = LineSeg.distanceTo (LineSeg o p)
         }, p)
      CurveTo pb pc pd ->
        (SDFComponent
         { windingNum = Bezier3.windingNum (Bezier3 o pb pc pd)
         , distanceTo = Bezier3.distanceTo 16 0.1 (Bezier3 o pb pc pd)
         }, pd)


inPath :: (Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> Bool
inPath path p = sum ((`windingNum` p) <$> pathToSDF path) /= 0


pathSDF :: (Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> a
pathSDF path p = sdfSign * minimum ((`distanceTo` p) <$> pathToSDF path)
  where
    sdfSign = if inPath path p then -1 else 1
