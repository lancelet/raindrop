{-|
Module      : AlphaStencilDiagrams
Description : Diagrams showing how alpha stencil rendering works.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencilDiagrams where

import           Control.Monad                    (forM_)
import qualified Data.Vector                      as V
import           Diagrams.Backend.Rasterific      (GifLooping (LoopingForever),
                                                   animatedGif,
                                                   renderRasterific)
import qualified Diagrams.Prelude                 as D
import           Text.Printf                      (printf)

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
    renderSteps = V.take 20 $ interpretEvents events
    addBG d = D.bg D.white $ D.frame 0.1 $ d
    diagrams = V.toList
               $ V.map addBG
               $ renderMultipleSteps defaultStyle renderSteps
    diaSize = D.dims (D.V2 (1024 :: Float) 1024)

  --animatedGif filePath diaSize LoopingForever 10 diagrams

  forM_ (zip [0..] diagrams) $ \(i :: Int, dia) -> do
    let fileName = printf "%.3d.png" i
    putStrLn fileName
    renderRasterific fileName diaSize dia
