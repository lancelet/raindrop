module Main where

import qualified AlphaStencilDiagrams.Diagrams as ASD
import           Diagrams.Backend.Rasterific   (B, renderRasterific)
import qualified Diagrams.Prelude              as D

main :: IO ()
main = do
  let
    testDia =
      D.bg D.white
      $ D.frame 0.1
      $ ASD.grid ASD.defaultGridStyle 5 5
  renderRasterific "test.png" (D.dims (D.V2 1024 1024)) testDia
