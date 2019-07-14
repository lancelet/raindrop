{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Massiv.Array     (Comp (Seq), Ix2 ((:.)), S, Sz (Sz))
import qualified Data.Massiv.Array     as Massiv
import           Data.Massiv.Array.IO  (Image)
import qualified Data.Massiv.Array.IO  as Massiv
import           Data.Word             (Word8)
import           Graphics.ColorSpace.Y (Pixel(PixelY), Y)
import           Lens.Micro            ((^.))
import           Linear                (_x, _y, dot, quadrance, (*^))
import qualified Linear
import           Linear.Affine         ((.-.), (.+^), distanceA)
import qualified Linear.Affine

type V a = Linear.V2 a
type Pt a = Linear.Affine.Point Linear.V2 a

mkPt :: a -> a -> Pt a
mkPt x y = Linear.Affine.P (Linear.V2 x y)

main :: IO ()
main = do
  putStrLn "raindrop - early dev"
  test

data PathComponent a
  = PCLine {-# UNPACK #-} !(Line a)

data Line a = Line {-# UNPACK #-} !(Pt a) {-# UNPACK #-} !(Pt a)

scalarCross :: (Num a) => V a -> V a -> a
scalarCross v1 v2 = v1^._x * v2^._y - v1^._y * v2^._x

clamp :: (Ord a) => a -> a -> a -> a
clamp minVal maxVal x | x < minVal = minVal
                      | x > maxVal = maxVal
                      | otherwise  = x

smoothStep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothStep minVal maxVal x | x < minVal = 0
                           | x > maxVal = 1
                           | otherwise  = 6*x'^(5 :: Int)
                                          - 15*x'^(4 :: Int)
                                          + 10*x'^(3 :: Int)
  where
    x' = (x - minVal) / (maxVal - minVal)

inRange :: (Ord a) => a -> a -> a -> Bool
inRange a1 a2 a | a2 > a1   = a >= a1 && a <= a2
                | otherwise = a >= a2 && a <= a1

pathComponentSDFSeg
  :: (Num a, Ord a, Floating a)
  => PathComponent a
  -> SDFSeg a
pathComponentSDFSeg pathComponent =
  case pathComponent of
    PCLine line -> lineSDFSeg line

lineSDFSeg :: forall a. (Num a, Ord a, Floating a) => Line a -> SDFSeg a
lineSDFSeg (Line p1 p2) = SDFSeg { windingNumber, distanceTo }
  where
    windingNumber :: Pt a -> Int
    windingNumber p = if inYRange then (if onLeft then 1 else -1) else 0
      where
        v = p .-. p1
        s = p2 .-. p1
        onLeft = v `scalarCross` s > 0
        inYRange = inRange (p1^._y) (p2^._y) (p^._y)

    distanceTo :: Pt a -> a
    distanceTo p = distanceA p q
      where
        v = p .-. p1
        s = p2 .-. p1
        t = clamp 0 1 $ (v `dot` s) / (quadrance s)
        q = p1 .+^ (t *^ s)

data SDFSeg a
  = SDFSeg
    { windingNumber :: Pt a -> Int
    , distanceTo    :: Pt a -> a
    }

sdfPolygon :: (Num a, Ord a) => [SDFSeg a] -> Pt a -> a
sdfPolygon segs pt = sdfSign * minDistance
  where
    minDistance = minimum $ fmap (`distanceTo` pt) segs
    sdfSign = if inPolygon segs pt then -1 else 1

aaPolygon :: (Fractional a, Ord a) => a -> [SDFSeg a] -> Pt a -> a
aaPolygon aaWidth segs pt = smoothStep (-a2) a2 sdf
  where
    a2 = aaWidth / 2
    sdf = sdfPolygon segs pt

inPolygon :: [SDFSeg a] -> Pt a -> Bool
inPolygon segs pt = totalWindingNumber > 0
  where
    totalWindingNumber = sum $ fmap (`windingNumber` pt) segs

-- | Example polygon; looks like a capital letter 'Y'.
ySDFSegs :: forall a. (Floating a, Ord a) => [SDFSeg a]
ySDFSegs = fmap pathComponentSDFSeg yPolygon

-- | Example polygon; looks like a capital letter 'Y'.
yPolygon :: forall a. (Fractional a) => [PathComponent a]
yPolygon = fmap PCLine lineSegs
  where
    lineSegs :: [Line a]
    lineSegs
      = [ Line (mkPt 221.8 459.4) (mkPt 290.2 459.4)
        , Line (mkPt 290.2 459.4) (mkPt 290.2 229.0)
        , Line (mkPt 290.2 229.0) (mkPt 416.2  54.5)
        , Line (mkPt 416.2  54.4) (mkPt 328.5  54.4)
        , Line (mkPt 328.5  54.4) (mkPt 256.0 163.7)
        , Line (mkPt 256.0 163.7) (mkPt 183.5  54.4)
        , Line (mkPt 183.5  54.4) (mkPt  95.8  54.4)
        , Line (mkPt  95.8  54.4) (mkPt 221.8 229.0)
        , Line (mkPt 221.8 229.0) (mkPt 221.8 459.4)
        ]

test :: IO ()
test = do
  let
    gen :: Ix2 -> Pixel Y Word8
    gen (j :. i) = if inPolygon segs pt then 255 else 0
      where
        segs :: [SDFSeg Float]
        segs = ySDFSegs

        pt :: Pt Float
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testInPolygonImg :: Image S Y Word8
    testInPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen

    gen2 :: Ix2 -> Pixel Y Word8
    gen2 (j :. i) = PixelY . floor $ sdfPolygon segs pt
      where
        segs = ySDFSegs
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testSDFPolygonImg :: Image S Y Word8
    testSDFPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen2

    gen3 :: Ix2 -> Pixel Y Word8
    gen3 (j :. i) = PixelY . floor $ 255 * aaPolygon 1.44 segs pt
      where
        segs = ySDFSegs
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testAAPolygonImg :: Image S Y Word8
    testAAPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen3

    gen4 :: Ix2 -> Pixel Y Word8
    gen4 (j :. i) = PixelY . floor $ 255 * aaPolygon 10 segs pt
      where
        segs = ySDFSegs
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testWideAAPolygonImg :: Image S Y Word8
    testWideAAPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen4

  Massiv.writeImage "test-in-polygon.png" testInPolygonImg
  Massiv.writeImage "test-sdf.png" testSDFPolygonImg
  Massiv.writeImage "test-aa-polygon.png" testAAPolygonImg
  Massiv.writeImage "test-wide-aa-polygon.png" testWideAAPolygonImg
  pure ()
