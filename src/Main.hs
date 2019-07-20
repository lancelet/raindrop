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


batSymbol :: Path Float
batSymbol
  = Path (mkP 290 319)
    [ CurveTo (mkP 277 248) (mkP 232 197) (mkP 201 257)
    , CurveTo (mkP 172 185) (mkP 96 218) (mkP 162 291)
    , CurveTo (mkP 22 244) (mkP 11 98) (mkP 194 66)
    , CurveTo (mkP 134 136) (mkP 264 207) (mkP 261 52)
    , LineTo (mkP 273 73)
    , LineTo (mkP 308 73)
    , LineTo (mkP 320 52)
    , CurveTo (mkP 317 207) (mkP 447 136) (mkP 387 66)
    , CurveTo (mkP 570 98) (mkP 559 244) (mkP 419 291)
    , CurveTo (mkP 485 218) (mkP 409 185) (mkP 380 257)
    , CurveTo (mkP 349 197) (mkP 304 248) (mkP 290 319)
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


inBatSymbol :: Image S Y Word8
inBatSymbol = Massiv.makeArray Seq (Sz (365 :. 581)) gen
  where
    gen (j :. i) = if inPath batSymbol pt then 255 else 0
      where
        pt = mkP (0.5 + fromIntegral i) (0.5 + fromIntegral j)


main :: IO ()
main = do
  Massiv.writeImage "test-in-polygon.png" inCapitalY
  Massiv.writeImage "test-in-bezier-thingo.png" inBezierThingo
  Massiv.writeImage "test-in-bat-symbol.png" inBatSymbol
