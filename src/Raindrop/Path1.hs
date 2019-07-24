{-|
Module      : Raindrop.Path1
Description : Initial path description language.
-}
module Raindrop.Path1 where

import           Data.Massiv.Array              (Unbox)
import           Graphics.ColorSpace.RGB        (Pixel, RGBA)
import           Linear.Epsilon                 (Epsilon)

import           Raindrop.Internal.Geom.Bezier3 (Bezier3 (Bezier3))
import qualified Raindrop.Internal.Geom.Bezier3 as Bezier3
import           Raindrop.Internal.Geom.LineSeg (LineSeg (LineSeg))
import qualified Raindrop.Internal.Geom.LineSeg as LineSeg
import           Raindrop.Internal.Geom.Vec     (P)


data Elem a
  = Flood (Fill a)
  | FillPath (Fill a) (Path a)


data Path a
  = Path
    (P a)
    [PathCommand a]


data Fill a
  = SolidFill (Pixel RGBA a)


data PathCommand a
  = LineTo (P a)
  | CurveTo (P a) (P a) (P a)


data SDFComponent a
  = SDFComponent
    { windingNum :: P a -> Int
    , distanceTo :: P a -> a }


pathToSDF :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> [SDFComponent a]
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
         , distanceTo = Bezier3.distanceTo 32 0.001 (Bezier3 o pb pc pd)
         }, pd)


inPath :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> Bool
inPath path p = sum ((`windingNum` p) <$> pathToSDF path) /= 0


pathSDF :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> a
pathSDF path p = sdfSign * minimum ((`distanceTo` p) <$> pathToSDF path)
  where
    sdfSign = if inPath path p then -1 else 1


smoothStep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothStep minVal maxVal x | x < minVal = 0
                           | x > maxVal = 1
                           | otherwise  = 6*x'^(5 :: Int)
                                          - 15*x'^(4 :: Int)
                                          + 10*x'^(3 :: Int)
  where
    x' = (x - minVal) / (maxVal - minVal)


aaPath :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => a -> Path a -> P a -> a
aaPath aaWidth path p = 1 - smoothStep (-a2) a2 (pathSDF path p)
  where
    a2 = aaWidth / 2


{-
drawElem :: (Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => a -> Elem a -> P a -> Pixel RGBA a
drawElem _ (Flood (SolidFill fill)) _ = fill
drawElem aaWidth (FillPath (SolidFill fill) path) p = aaPath path p
-}
