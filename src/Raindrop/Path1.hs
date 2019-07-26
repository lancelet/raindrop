{-|
Module      : Raindrop.Path1
Description : Initial path description language.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Raindrop.Path1 where

import           Data.Massiv.Array              (Unbox)
import           Graphics.ColorSpace.RGB        (Pixel(PixelRGBA), RGBA)
import           Linear.Epsilon                 (Epsilon)

import           Raindrop.Internal.Geom.Bezier3 (Bezier3 (Bezier3))
import qualified Raindrop.Internal.Geom.Bezier3 as Bezier3
import           Raindrop.Internal.Geom.LineSeg (LineSeg (LineSeg))
import qualified Raindrop.Internal.Geom.LineSeg as LineSeg
import           Raindrop.Internal.Geom.Vec     (P)


data Elem px a
  = Flood (Fill px)
  | FillPath (Fill px) (Path a)


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
         , distanceTo = Bezier3.distanceTo 8 0.2 0.2 (Bezier3 o pb pc pd)
         }, pd)


inPath :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> Bool
inPath path p =
  let
    sdfComponents = pathToSDF path
  in
    sum ((`windingNum` p) <$> sdfComponents) /= 0


pathSDF :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => Path a -> P a -> a
pathSDF path p =
  let
    sdfComponents = pathToSDF path
    ip = inPath path
    sdfSign = if ip p then -1 else 1
  in
    sdfSign * minimum ((`distanceTo` p) <$> sdfComponents)


smoothStep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothStep minVal maxVal x | x' <= 0.01 = 0
                           | x' >= 0.99 = 1
                           | otherwise  = 6*x'^(5 :: Int)
                                          - 15*x'^(4 :: Int)
                                          + 10*x'^(3 :: Int)
  where
    x' = (x - minVal) / (maxVal - minVal)


alphaOver :: forall px. (Num px, Integral px, Bounded px) => Pixel RGBA px -> Pixel RGBA px -> Pixel RGBA px
alphaOver (PixelRGBA ra' ga' ba' aa') (PixelRGBA rb' gb' bb' ab') = PixelRGBA r g b a
  where
    pxtof x = fromIntegral x / fromIntegral (maxBound :: px)
    ftopx x = floor $ fromIntegral (maxBound :: px) * x
    -- premul alpha
    faa, fab :: Float
    faa = pxtof aa'
    fab = pxtof ab'
    ra = pxtof ra' * faa
    ga = pxtof ga' * faa
    ba = pxtof ba' * faa
    rb = pxtof rb' * fab
    gb = pxtof gb' * fab
    bb = pxtof bb' * fab
    r = ftopx $ ra + rb*(1 - faa)
    g = ftopx $ ga + gb*(1 - faa)
    b = ftopx $ ba + bb*(1 - faa)
    a = ftopx $ faa + fab*(1 - faa)


aaPath :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a) => a -> Path a -> P a -> a
aaPath aaWidth path p =
  let
    psdf = pathSDF path
    a2 = aaWidth / 2
  in
    1 - smoothStep (-a2) a2 (psdf p)


drawElem :: (Show a, Ord a, RealFrac a, Floating a, Unbox a, Epsilon a, Integral px) => a -> Elem px a -> P a -> Pixel RGBA px
drawElem _ (Flood (SolidFill fill)) _ = fill
drawElem aaWidth (FillPath (SolidFill (PixelRGBA r g b a)) path) p =
  let
    aap = aaPath aaWidth path
    a' = floor $ fromIntegral a * aap p
  in
    PixelRGBA r g b a'

drawElems
  :: forall px a.
     ( Bounded px
     , Num px
     , Show a
     , RealFrac a
     , Floating a
     , Unbox a
     , Epsilon a
     , Integral px )
  => a
  -> [Elem px a]
  -> P a
  -> Pixel RGBA px
drawElems aaWidth elemList =
  let
    draws :: [P a -> Pixel RGBA px]
    draws = drawElem aaWidth <$> elemList

    drawElems' :: [P a -> Pixel RGBA px] -> P a -> Pixel RGBA px
    drawElems' [] _ = PixelRGBA 0 0 0 0
    drawElems' (e:es) p = if a == (maxBound :: px) then c else alphaOver c (drawElems' es p)
      where
        c@(PixelRGBA _ _ _ a) = e p
  in
    drawElems' draws

{-
drawElems :: forall px a. (Bounded px, Num px, Show a, RealFrac a, Floating a, Unbox a, Epsilon a, Integral px) => a -> [Elem px a] -> P a -> Pixel RGBA px
drawElems _ [] _ = PixelRGBA 0 0 0 0
drawElems aaWidth [e] p = drawElem aaWidth e p
drawElems aaWidth (e:es) p =
  if a == (maxBound :: px) then c else alphaOver c (drawElems aaWidth es p)
  where
    c@(PixelRGBA _ _ _ a) = drawElem aaWidth e p
-}
