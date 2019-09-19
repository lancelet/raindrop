{-|
Module      : AlphaStencilDiagrams
Description : Diagrams showing how alpha stencil rendering works.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencilDiagrams where

import           Control.Monad                    (forM_)
import qualified Data.Vector                      as V
import           Diagrams.Backend.Rasterific      (renderRasterific)
import qualified Diagrams.Prelude                 as D
import           Text.Printf                      (printf)

import           AlphaStencil                     (Epsilon (Epsilon),
                                                   logRenderSegs)
import           AlphaStencilDiagrams.Diagrams    (defaultStyle,
                                                   renderMultipleSteps)
import           AlphaStencilDiagrams.SamplePaths (hashSymbol, pathToSegs)
import           AlphaStencilDiagrams.State       (interpretEvents)
import           Image                            (Size (Size))

animateRenderingHashSymbol :: FilePath -> IO ()
animateRenderingHashSymbol filePath = do
  let
    path = hashSymbol
    eps = Epsilon 1e-5
    (_, events) = logRenderSegs
                  eps
                  (Size 16 16)
                  (pathToSegs eps path)
    renderSteps = interpretEvents events
    addBG d = D.bg D.white $ D.frame 0.1 d
    diagrams = V.toList
               $ V.map addBG
               $ renderMultipleSteps defaultStyle path renderSteps
    diaSize = D.dims (D.V2 (1024 :: Float) 1024)

  forM_ (zip [0..] diagrams) $ \(i :: Int, dia) -> do
    let fileName = printf "%s.%.3d.png" filePath i
    putStrLn fileName
    renderRasterific fileName diaSize dia
