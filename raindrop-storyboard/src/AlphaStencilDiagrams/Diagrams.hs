{-|
Module      : AlphaStencilDiagrams.Diagrams
Description : Diagrams for AlphaStencil state.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module AlphaStencilDiagrams.Diagrams where

import           Data.Colour                (Colour)
import           Data.Colour.SRGB           (sRGB24)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Diagrams.Prelude           (HasStyle, InSpace, TrailLike,
                                             Transformable, V2 (V2), ( # ))
import qualified Diagrams.Prelude           as D

import           AlphaStencilDiagrams.State (AnimAction (OutputFrame, SkipToNext),
                                             RenderState, RenderStep,
                                             rsAnimAction, rsImage, rsState)
import           Image                      (Size (Size))
import qualified Image

data Style
  = Style
    { styGrid :: GridStyle
    }

defaultStyle :: Style
defaultStyle
  = Style
    { styGrid = defaultGridStyle
    }

renderMultipleSteps
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t )
  => Style
  -> Vector (RenderStep a)
  -> Vector t
renderMultipleSteps style = V.mapMaybe (renderSingleStep style)

renderSingleStep
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t )
  => Style
  -> RenderStep a
  -> Maybe t
renderSingleStep style step =
  case rsAnimAction step of
    OutputFrame -> Just $ renderState style (rsState step)
    SkipToNext  -> Nothing

renderState
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t )
  => Style
  -> RenderState a
  -> t
renderState style state =
  pixelGrid
  where
    pixelGrid = case Image.size <$> rsImage state of
      Nothing         -> mempty
      Just (Size w h) -> grid (styGrid style) (fromIntegral w) (fromIntegral h)

---- Pixel grid

-- | Style for a pixel grid.
data GridStyle
  = GridStyle
    { gsBorderLineOWidth :: Float
    , gsBorderLineColor  :: Colour Double
    , gsGridLineOWidth   :: Float
    , gsGridLineColor    :: Colour Double
    }

-- | Default style for a pixel grid.
defaultGridStyle :: GridStyle
defaultGridStyle
  = GridStyle
    { gsBorderLineOWidth = 2.0
    , gsBorderLineColor  = sRGB24 0x00 0x00 0x00
    , gsGridLineOWidth   = 2.0
    , gsGridLineColor    = sRGB24 0x00 0x00 0x00
    }

-- | Pixel grid diagram.
grid
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t )
  => GridStyle
  -> Int
  -> Int
  -> t
grid style nWide nHigh = border <> verticalLines <> horizontalLines
  where
    width = fromIntegral nWide
    height = fromIntegral nHigh

    border =
      D.rect width height
      # D.translate (V2 (width/2) (height/2))
      # D.lc (gsBorderLineColor style)
      # D.lwO (gsBorderLineOWidth style)
      # D.lineJoin D.LineJoinRound

    verticalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | x <- fromIntegral <$> [ 1 .. (nWide - 1) ]
      , let p1 = D.mkP2 x 0
      , let p2 = D.mkP2 x height
      ]
      # D.lc (gsGridLineColor style)
      # D.lwO (gsGridLineOWidth style)

    horizontalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | y <- fromIntegral <$> [ 1 .. (nHigh - 1) ]
      , let p1 = D.mkP2 0 y
      , let p2 = D.mkP2 width y
      ]
      # D.lc (gsGridLineColor style)
      # D.lwO (gsGridLineOWidth style)
