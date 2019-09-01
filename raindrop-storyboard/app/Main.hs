module Main where

import AlphaStencilDiagrams (animateRenderingHashSymbol)

main :: IO ()
main =
  animateRenderingHashSymbol "test.gif"
