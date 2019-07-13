{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Massiv.Array     (Comp (Seq), Ix2 ((:.)), S, Sz (Sz))
import qualified Data.Massiv.Array     as Massiv
import           Data.Massiv.Array.IO  (Image)
import qualified Data.Massiv.Array.IO  as Massiv
import           Data.Word             (Word8)
import           Graphics.ColorSpace.Y (Pixel, Y)
import           Lens.Micro            ((^.))
import           Linear                (_x, _y)
import qualified Linear
import           Linear.Affine         ((.-.))
import qualified Linear.Affine
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

inRange :: (Ord a) => a -> a -> a -> Bool
inRange a1 a2 a | a2 > a1   = a >= a1 && a <= a2
                | otherwise = a >= a2 && a <= a1

pathComponentSDFSeg :: (Num a, Ord a) => PathComponent a -> SDFSeg a
pathComponentSDFSeg pathComponent =
  case pathComponent of
    PCLine line -> lineSDFSeg line

lineSDFSeg :: forall a. (Num a, Ord a) => Line a -> SDFSeg a
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
    distanceTo = undefined

data SDFSeg a
  = SDFSeg
    { windingNumber :: Pt a -> Int
    , distanceTo    :: Pt a -> a
    }

inPolygon :: [SDFSeg a] -> Pt a -> Bool
inPolygon segs pt = totalWindingNumber > 0
  where
    totalWindingNumber = sum $ fmap (`windingNumber` pt) segs

-- | Example polygon; looks like a capital letter 'Y'.
ySDFSegs :: forall a. (Fractional a, Ord a) => [SDFSeg a]
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
  Massiv.writeImage "test-in-polygon.png" testInPolygonImg
  pure ()
