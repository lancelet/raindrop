{-|
Module      : AlphaStencilDiagrams.Diagrams
Description : Diagrams for AlphaStencil state.
-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module AlphaStencilDiagrams.Diagrams where

import           Data.Colour                    ( Colour
                                                , blend
                                                )
import           Data.Colour.SRGB               ( sRGB24 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Diagrams.Prelude               ( HasStyle
                                                , InSpace
                                                , TrailLike
                                                , Transformable
                                                , V2(V2)
                                                , (#)
                                                )
import qualified Diagrams.Prelude              as D
import           Foreign.Storable               ( Storable )

import           AlphaStencil                   ( P(P) )
import           AlphaStencilDiagrams.SamplePaths
                                                ( Loop(Loop)
                                                , Path(Path)
                                                )
import           AlphaStencilDiagrams.State     ( AnimAction
                                                  ( OutputFrame
                                                  , SkipToNext
                                                  )
                                                , RenderState
                                                , RenderStep
                                                , rsAnimAction
                                                , rsImage
                                                , rsState
                                                )
import           GHC.Float                      ( float2Double )
import           Image                          ( I(I)
                                                , Image
                                                , Ix(Ix)
                                                , J(J)
                                                , Size(Size)
                                                )
import qualified Image
import           Optics                         ( (^.) )
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

-- | Style for an image.
newtype ImageStyle
  = ImageStyle
    { _pixelColorFn :: Float -> Colour Double
    }
Optics.TH.makeLenses ''ImageStyle

-- | Style for a path.
data PathStyle
  = PathStyle
    { _pathLineOutputWidth :: Float
    , _pathLineColor       :: Colour Double
    }
Optics.TH.makeLenses ''PathStyle

data Style
  = Style
    { _gridStyle  :: GridStyle
    , _imageStyle :: ImageStyle
    , _pathStyle  :: PathStyle
    }
Optics.TH.makeLenses ''Style

defaultStyle :: Style
defaultStyle = Style { _gridStyle  = defaultGridStyle
                     , _imageStyle = defaultImageStyle
                     , _pathStyle  = defaultPathStyle
                     }

---- Rendering Functions ------------------------------------------------------

renderMultipleSteps
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t
     , Real a
     , Storable a
     )
  => Style
  -> Path a
  -> Vector (RenderStep a)
  -> Vector t
renderMultipleSteps style rpath = V.mapMaybe (renderSingleStep style rpath)

renderSingleStep
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t
     , Real a
     , Storable a
     )
  => Style
  -> Path a
  -> RenderStep a
  -> Maybe t
renderSingleStep style rpath step = case rsAnimAction step of
  OutputFrame -> Just $ renderState style rpath (rsState step)
  SkipToNext  -> Nothing

renderState
  :: ( InSpace V2 Float t
     , TrailLike t
     , HasStyle t
     , Transformable t
     , Monoid t
     , Real a
     , Storable a
     )
  => Style
  -> Path a
  -> RenderState a
  -> t
renderState style rpath state = pathOutlines <> pixelGrid <> imageRects
 where
  pathOutlines = path (style ^. pathStyle) rpath
  pixelGrid    = case Image.size <$> rsImage state of
    Nothing -> mempty
    Just (Size w h) ->
      grid (style ^. gridStyle) (fromIntegral w) (fromIntegral h)
  imageRects = case rsImage state of
    Nothing  -> mempty
    Just img -> image (style ^. imageStyle) img

---- Pixel grid

-- | Default style for a pixel grid.
defaultGridStyle :: GridStyle
defaultGridStyle = GridStyle { _borderLineOutputWidth = 2.0
                             , _borderLineColor       = sRGB24 0x00 0x00 0x00
                             , _gridLineOutputWidth   = 0.6
                             , _gridLineColor         = sRGB24 0x00 0x00 0x00
                             }

-- | Pixel grid diagram.
grid
  :: (InSpace V2 Float t, TrailLike t, HasStyle t, Transformable t, Monoid t)
  => GridStyle
  -> Int
  -> Int
  -> t
grid style nWide nHigh = border <> verticalLines <> horizontalLines
 where
  width  = fromIntegral nWide
  height = fromIntegral nHigh

  border =
    D.rect width height
      # D.translate (V2 (width / 2) (height / 2))
      # D.lc (style ^. borderLineColor)
      # D.lwO (style ^. borderLineOutputWidth)
      # D.lineJoin D.LineJoinRound

  verticalLines =
    mconcat
        [ D.fromVertices [p1, p2]
        | x <- fromIntegral <$> [1 .. (nWide - 1)]
        , let p1 = D.mkP2 x 0
        , let p2 = D.mkP2 x height
        ]
      # D.lc (style ^. gridLineColor)
      # D.lwO (style ^. gridLineOutputWidth)

  horizontalLines =
    mconcat
        [ D.fromVertices [p1, p2]
        | y <- fromIntegral <$> [1 .. (nHigh - 1)]
        , let p1 = D.mkP2 0 y
        , let p2 = D.mkP2 width y
        ]
      # D.lc (style ^. gridLineColor)
      # D.lwO (style ^. gridLineOutputWidth)

---- Image

-- | Bi-colour image function.
--
-- This function uses three linearly-interpolated colours to produce a function
-- from a magnitude to a pixel colour:
--   - Zero colour
--   - Negative colour
--   - Positive colour
-- Additionally, it takes a magnitude, which is the maximum expected magnitude
-- of the floating-point magnitude.
biColor
  :: Colour Double
  -> Colour Double
  -> Colour Double
  -> Float
  -> Float
  -> Colour Double
biColor zeroColour negColour posColour maxMag = colorFn
 where
  colorFn :: Float -> Colour Double
  colorFn x | x >= 0    = blend frac posColour zeroColour
            | otherwise = blend frac negColour zeroColour
    where frac = float2Double $ abs x / abs maxMag

-- | Default style for images.
defaultImageStyle :: ImageStyle
defaultImageStyle = ImageStyle (biColor zc nc pc 1.0)
 where
  zc = sRGB24 0xFF 0xFF 0xFF
  nc = sRGB24 0xE8 0x7A 0xBC
  pc = sRGB24 0x9F 0xE8 0x7A

-- | Render an image with unit-square pixels.
image
  :: forall t a
   . ( InSpace V2 Float t
     , TrailLike t
     , Transformable t
     , HasStyle t
     , Monoid t
     , Real a
     , Storable a
     )
  => ImageStyle
  -> Image a
  -> t
image style img =
  mconcat
    $   renderCell
    <$> [ Ix i j | i <- I <$> [0 .. width - 1], j <- J <$> [0 .. height - 1] ]
 where
  sz     = Image.size img
  width  = fromIntegral . Image.width $ sz
  height = fromIntegral . Image.height $ sz
  pxColor ix =
    style ^. pixelColorFn $ fromRational . toRational $ Image.getPixel img ix

  renderCell :: Ix -> t
  renderCell ix@(Ix (I i) (J j)) =
    D.rect 1 1
      # D.translateX (fromIntegral i + 0.5)
      # D.translateY (fromIntegral j + 0.5)
      # D.lwO 0
      # D.fc (pxColor ix)

---- Path outline

defaultPathStyle :: PathStyle
defaultPathStyle = PathStyle { _pathLineOutputWidth = 4.5
                             , _pathLineColor       = sRGB24 0xEF 0xC6 0xA4
                             }

-- | Path outline diagram.
path
  :: (Monoid t, InSpace V2 Float t, TrailLike t, HasStyle t, Real a)
  => PathStyle
  -> Path a
  -> t
path style (Path loops) = mconcat $ loop style <$> loops

-- | Path loop diagram.
loop
  :: forall t a
   . (InSpace V2 Float t, TrailLike t, HasStyle t, Real a)
  => PathStyle
  -> Loop a
  -> t
loop style (Loop points) =
  D.fromVertices (p2v <$> (points ++ [head points]))
    # D.lc (style ^. pathLineColor)
    # D.lwO (style ^. pathLineOutputWidth)
 where
  p2v :: P a -> D.P2 Float
  p2v (P x y) = D.p2 (a2f x, a2f y)

  a2f :: a -> Float
  a2f = realToFrac
