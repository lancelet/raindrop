{-|
Module      : AlphaStencilDiagrams.Diagrams
Description : Diagrams for AlphaStencil state.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
module AlphaStencilDiagrams.Diagrams where

import           Control.Lens     (Lens', lens, (^.))
import           Data.Colour      (Colour)
import           Data.Colour.SRGB (sRGB24)
import           Diagrams.Prelude (HasStyle, InSpace, TrailLike, Transformable,
                                   V2 (V2), ( # ))
import qualified Diagrams.Prelude as D

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

class HasGridStyle style where
  gridStyle :: Lens' style GridStyle
instance HasGridStyle GridStyle where
  gridStyle = lens id (const id)

-- | Pixel grid diagram.
grid
  :: ( HasGridStyle style
     , InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t )
  => style
  -> Int
  -> Int
  -> t
grid style nWide nHigh = border <> verticalLines <> horizontalLines
  where
    gs :: GridStyle
    gs = style ^. gridStyle

    width = fromIntegral nWide
    height = fromIntegral nHigh

    border =
      D.rect width height
      # D.translate (V2 (width/2) (height/2))
      # D.lc (gsBorderLineColor gs)
      # D.lwO (gsBorderLineOWidth gs)
      # D.lineJoin D.LineJoinRound

    verticalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | x <- fromIntegral <$> [ 1 .. (nWide - 1) ]
      , let p1 = D.mkP2 x 0
      , let p2 = D.mkP2 x height
      ]
      # D.lc (gsGridLineColor gs)
      # D.lwO (gsGridLineOWidth gs)

    horizontalLines =
      mconcat
      [ D.fromVertices [ p1, p2 ]
      | y <- fromIntegral <$> [ 1 .. (nHigh - 1) ]
      , let p1 = D.mkP2 0 y
      , let p2 = D.mkP2 width y
      ]
      # D.lc (gsGridLineColor gs)
      # D.lwO (gsGridLineOWidth gs)
