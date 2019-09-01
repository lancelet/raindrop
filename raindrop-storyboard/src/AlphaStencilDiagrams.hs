{-|
Module      : AlphaStencilDiagrams
Description : Diagrams showing how alpha stencil rendering works.
-}
{-# LANGUAGE FlexibleContexts #-}
module AlphaStencilDiagrams where

import qualified Data.Vector                      as V
import           Diagrams.Backend.Rasterific      (GifLooping (LoopingForever),
                                                   animatedGif)
import qualified Diagrams.Prelude                 as D

import           AlphaStencil                     (Epsilon (Epsilon),
                                                   logRenderSegs)
import           AlphaStencilDiagrams.Diagrams    (defaultStyle,
                                                   renderMultipleSteps)
import           AlphaStencilDiagrams.SamplePaths (hashSymbol)
import           AlphaStencilDiagrams.State       (interpretEvents)
import           Image                            (Size (Size))

animateRenderingHashSymbol :: FilePath -> IO ()
animateRenderingHashSymbol filePath = do
  let
    (_, events) = logRenderSegs (Epsilon 1e-5) (Size 16 16) hashSymbol
    renderSteps = V.take 5 $ interpretEvents events
    addBG d = D.bg D.white $ D.frame 0.1 $ d
    diagrams = V.toList
               $ V.map addBG
               $ renderMultipleSteps defaultStyle renderSteps
    diaSize = D.dims (D.V2 (1024 :: Float) 1024)
  animatedGif filePath diaSize LoopingForever 10 diagrams
