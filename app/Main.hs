{-|
Module      : Main
Description : Main application entry point (testing for now)
-}
module Main (main) where

import qualified Codec.Picture       as JP
import           Foreign.Storable    (Storable)

import           AlphaStencil.Render (renderSegs)
import           AlphaStencil.Seg    (P (P), Seg (Seg))
import           Image               (Image, Size (Size), rowMajor)

main :: IO ()
main = renderY

renderY :: IO ()
renderY = do
  let
    img = renderPath (Size 512 512) capitalY

    jpImg :: JP.Image JP.PixelF
    jpImg = JP.Image 512 512 (rowMajor img)

    dynImg :: JP.DynamicImage
    dynImg = JP.ImageYF jpImg

  JP.savePngImage "ytest.png" dynImg

renderPath :: (RealFrac a, Storable a) => Size -> Path a -> Image a
renderPath sz path = renderSegs sz (pathToSegs path)

pathToSegs :: Path a -> [Seg a]
pathToSegs (Path _ [])              = []
pathToSegs (Path p (LineTo q : ss)) = Seg p q : pathToSegs (Path q ss)

data Path a = Path (P a) [PathComponent a]
data PathComponent a = LineTo (P a)

capitalY :: Path Float
capitalY
  = Path (P 221.8 459.4)
    [ LineTo (P 290.2 459.4)
    , LineTo (P 290.2 229.0)
    , LineTo (P 419.2  54.5)
    , LineTo (P 328.5  54.5)
    , LineTo (P 256.0 163.7)
    , LineTo (P 183.5  54.4)
    , LineTo (P  95.8  54.4)
    , LineTo (P 221.8 229.0)
    , LineTo (P 221.8 459.4)
    ]
