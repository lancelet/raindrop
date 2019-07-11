{-|
Module      : Main
Description : Main application entry point (testing for now)
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Codec.Picture       as JP
import           Codec.Picture.Extra (flipVertically)
import           Data.Maybe          (catMaybes)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Foreign.Storable    (Storable)

import           AlphaStencil.Log    (Event)
import           AlphaStencil.Render (logRenderSegs)
import           AlphaStencil.Seg    (Epsilon (Epsilon), P (P), Seg, seg)
import           Image               (Image, Size (Size), rowMajor)

main :: IO ()
main = renderHashSymbol

renderHashSymbol :: IO ()
renderHashSymbol = do
  let
    (img, events) = renderPath (Size 16 16) (Epsilon 1e-5) hashSymbol

    jpImg :: JP.Image JP.PixelF
    jpImg = flipVertically $ JP.Image 16 16 (rowMajor img)

    dynImg :: JP.DynamicImage
    dynImg = JP.ImageYF jpImg

  mapM_ (putStrLn . show) (V.toList events)
  JP.savePngImage "hash-symbol.png" dynImg

renderPath
  :: (RealFrac a, Fractional a, Ord a, Storable a)
  => Size
  -> Epsilon a
  -> Path a
  -> (Image a, Vector(Event a))
renderPath sz eps path = logRenderSegs eps sz (pathToSegs eps path)

data Path a = Path [Loop a]
data Loop a = Loop [P a]

pathToSegs :: (Ord a, Fractional a) => Epsilon a -> Path a -> [Seg a]
pathToSegs eps (Path ls) = concat $ loopToSegs eps <$> ls

loopToSegs :: forall a. (Ord a, Fractional a) => Epsilon a -> Loop a -> [Seg a]
loopToSegs _ (Loop [])        = []
loopToSegs _ (Loop [_,_])     = []
loopToSegs eps (Loop (pf:ps)) = catMaybes (followTrail pf (pf:ps))
  where
    followTrail :: P a -> [P a] -> [Maybe (Seg a)]
    followTrail _ []        = []
    followTrail firstPt [p] = [seg eps p firstPt]
    followTrail firstPt (p : q : ss) =
      seg eps p q : followTrail firstPt (q : ss)

hashSymbol :: Path Float
hashSymbol =
  Path
  [ Loop
    [ P  3.70  1.00  -- bottom right corner; outer loop
    , P  5.00  1.00
    , P  5.78  4.90
    , P  8.78  4.90
    , P  8.00  1.00
    , P  9.30  1.00
    , P 10.08  4.90
    , P 13.00  4.90
    , P 13.00  6.30
    , P 10.36  6.30
    , P 11.02  9.60
    , P 13.00  9.60
    , P 13.00 11.00
    , P 11.30 11.00
    , P 12.10 15.00
    , P 10.80 15.00
    , P 10.00 11.00
    , P  7.00 11.00
    , P  7.80 15.00
    , P  6.50 15.00
    , P  5.70 11.00
    , P  3.00 11.00
    , P  3.00  9.60
    , P  5.42  9.60
    , P  4.76  6.30
    , P  3.00  6.30
    , P  3.00  4.90
    , P  4.48  4.90
    ]
  , Loop
    [ P  6.72  9.60 -- top left corner; inner loop
    , P  9.72  9.60
    , P  9.06  6.30
    , P  6.06  6.30
    ]
  ]
