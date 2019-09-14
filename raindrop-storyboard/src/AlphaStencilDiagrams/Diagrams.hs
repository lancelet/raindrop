{-|
Module      : AlphaStencilDiagrams.Diagrams
Description : Diagrams for AlphaStencil state.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
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
import           Optics                     ((^.))
import qualified Optics.TH

---- Styles -------------------------------------------------------------------

-- | Style for a pixel grid.
data GridStyle
  = GridStyle
    { _borderLineOutputWidth :: Float
    , _borderLineColor       :: Colour Double
    , _gridLineOutputWidth   :: Float
    , _gridLineColor         :: Colour Double
    }
Optics.TH.makeLenses ''GridStyle

data Style
  = Style
    { _gridStyle :: GridStyle
    }
Optics.TH.makeLenses ''Style

defaultStyle :: Style
defaultStyle
  = Style
    { _gridStyle = defaultGridStyle
    }

---- Rendering Functions ------------------------------------------------------

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
      Just (Size w h) -> grid (style^.gridStyle)
                              (fromIntegral w)
                              (fromIntegral h)

---- Pixel grid

-- | Default style for a pixel grid.
defaultGridStyle :: GridStyle
defaultGridStyle
  = GridStyle
    { _borderLineOutputWidth = 2.0
    , _borderLineColor       = sRGB24 0x00 0x00 0x00
    , _gridLineOutputWidth   = 2.0
    , _gridLineColor         = sRGB24 0x00 0x00 0x00
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
      # D.lc (style^.borderLineColor)
      # D.lwO (style^.borderLineOutputWidth)
      # D.lineJoin D.LineJoinRound

    verticalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | x <- fromIntegral <$> [ 1 .. (nWide - 1) ]
      , let p1 = D.mkP2 x 0
      , let p2 = D.mkP2 x height
      ]
      # D.lc (style^.gridLineColor)
      # D.lwO (style^.gridLineOutputWidth)

    horizontalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | y <- fromIntegral <$> [ 1 .. (nHigh - 1) ]
      , let p1 = D.mkP2 0 y
      , let p2 = D.mkP2 width y
      ]
      # D.lc (style^.gridLineColor)
      # D.lwO (style^.gridLineOutputWidth)
