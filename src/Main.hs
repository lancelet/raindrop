{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Massiv.Array          (Comp (Seq), Ix2 ((:.)), S, Sz (Sz))
import           Data.Massiv.Array          as Massiv
import           Data.Massiv.Array.IO       (Image)
import           Data.Massiv.Array.IO       as Massiv
import           Data.Word                  (Word8)
import           Graphics.ColorSpace.Y      (Pixel (PixelY), Y)

import           Raindrop.Internal.Geom.Vec (mkP)
import           Raindrop.Path1             (Path (Path),
                                             PathCommand (CurveTo, LineTo),
                                             inPath)

capitalY :: Path Float
capitalY
  = Path (mkP 221.8 459.4)
    [ LineTo (mkP 290.2 459.4)
    , LineTo (mkP 290.2 229.0)
    , LineTo (mkP 419.2  54.5)
    , LineTo (mkP 328.5  54.5)
    , LineTo (mkP 256.0 163.7)
    , LineTo (mkP 183.5  54.4)
    , LineTo (mkP  95.8  54.4)
    , LineTo (mkP 221.8 229.0)
    , LineTo (mkP 221.8 459.4)
    ]


bezierThingo :: Path Float
bezierThingo
  = Path (mkP 286.7 395.9)
    [ CurveTo (mkP 316.2 23.0) (mkP 372.9 481.1) (mkP 403.7 169.9)
    , LineTo (mkP 460.4 169.9)
    , LineTo (mkP 460.4  41.4)
    , LineTo (mkP  78.9  41.4)
    , LineTo (mkP  78.9 449.7)
    , LineTo (mkP 403.7 449.7)
    , LineTo (mkP 403.7 364.4)
    , LineTo (mkP 329.0 364.4)
    , LineTo (mkP 329.0 395.9)
    , LineTo (mkP 286.7 395.9)
    ]


inCapitalY :: Image S Y Word8
inCapitalY = Massiv.makeArray Seq (Sz (512 :. 512)) gen
  where
    gen (j :. i) = if inPath capitalY pt then 255 else 0
      where
        pt = mkP (fromIntegral i) (fromIntegral j)


inBezierThingo :: Image S Y Word8
inBezierThingo = Massiv.makeArray Seq (Sz (512 :. 512)) gen
  where
    gen (j :. i) = if inPath bezierThingo pt then 255 else 0
      where
        pt = mkP (fromIntegral i) (fromIntegral j)


main :: IO ()
main = do
  Massiv.writeImage "test-in-polygon.png" inCapitalY
  Massiv.writeImage "test-in-bezier-thingo.png" inBezierThingo
