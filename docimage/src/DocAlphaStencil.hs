{-|
Module      : DocAlphaStencil
Description : Graphical documentation of the alpha stencil rendering.
-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
module DocAlphaStencil
  ( renderDoc
  ) where

import           Control.Monad               (forM_)
import           Data.Colour.SRGB            (sRGB24read)
import           Data.List                   (mapAccumL)
import           Data.Maybe                  (catMaybes)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           Diagrams                    (Diagram, ( # ))
import qualified Diagrams                    as D
import           Diagrams.Backend.Rasterific (B, renderRasterific)
import qualified Diagrams.Prelude            as D
import           Foreign.Storable            (Storable)
import           Text.Printf                 (printf)

import           AlphaStencil.Log            (Event (EventAddToPixel, EventPixelAreaDivision, EventPixelLineClip, EventPixelProjectX, EventStartSeg))
import           AlphaStencil.Render         (logRenderSegs)
import           AlphaStencil.Seg            (ClipSeg, Epsilon (Epsilon), P (P),
                                              PxDivision (PxDivision), Seg,
                                              pattern Seg, seg, unClipSeg)
import           Image                       (I (I), Image, Ix (Ix), J (J),
                                              Size (Size), getPixel, getSize,
                                              newImage, setPixel)

renderDoc :: IO ()
renderDoc = do
  let
    sz = Size 16 16
    eps = Epsilon 1e-5
    initState = State True hashSymbol (newImage sz 0) Nothing Nothing Nothing Nothing
    (_, events) = logRenderSegs eps sz (pathToSegs eps hashSymbol)

  let
    mapf state event = (s', s')
      where
        s' = acceptEvent event state

    states :: Vector (State Float)
    states = V.filter stateDraw $ snd $ mapAccumL mapf initState events

    diagrams :: Vector (Diagram B)
    diagrams = renderState <$> states

  let layoutDia dia =
        D.bg D.white
        $ D.frame 0.1
        $ dia

  forM_ (V.indexed diagrams) $ \(i, dia) -> do
    let
      name = "test." <> printf "%.3d" i <> ".png"
    renderRasterific name (D.dims (D.V2 1024 1024)) (layoutDia dia)

acceptEvent :: (Storable a, Num a) => Event a -> State a -> State a
acceptEvent (EventStartSeg (segnt)) state =
  state
  { stateDraw = True
  , stateMSeg = Just segnt
  , stateMClipSeg = Nothing
  , stateMProject = Nothing
  , stateMPxDivision = Nothing }
acceptEvent (EventPixelLineClip cSeg) state =
  state
  { stateMClipSeg = Just cSeg
  , stateDraw = True
  , stateMProject = Nothing
  , stateMPxDivision = Nothing
  }
acceptEvent (EventPixelAreaDivision ix cSeg pxDivision) state =
  state
  { stateDraw = True
  , stateMPxDivision = Just (ix, cSeg, pxDivision)
  }
acceptEvent (EventPixelProjectX ix cSeg) state =
  state
  { stateDraw = True
  , stateMProject = Just (ix, cSeg)
  , stateMPxDivision = Nothing
  }
acceptEvent (EventAddToPixel ix x) state =
  state
  { stateDraw = False
  , stateImage =
      let
        img = stateImage state
        newValue = getPixel img ix + x
      in
        setPixel img ix newValue
  }

data State a
  = State
    { stateDraw        :: Bool
    , _statePath       :: Path a
    , stateImage       :: Image a
    , stateMSeg        :: Maybe (Seg a)
    , stateMClipSeg    :: Maybe (ClipSeg a)
    , stateMProject    :: Maybe (Ix, ClipSeg a)
    , stateMPxDivision :: Maybe (Ix, ClipSeg a, PxDivision a)
    }

renderState :: (Real a, Storable a) => State a -> Diagram B
renderState (State _ path img mSeg mClipSeg mProject mPxDivision) =
  mconcat
  [ maybe mempty clipSeg mClipSeg
  , maybe mempty segment mSeg
  , maybe mempty areaDivision mPxDivision
  , maybe mempty pxProject mProject
  , pathOutline path
  , pixelGridLines (getSize img)
  , renderImage img
  ]

renderImage :: (Real a, Storable a) => Image a -> Diagram B
renderImage img =
  mconcat
  [ renderCell i j
  | j <- [ 0 .. (h - 1) ]
  , i <- [ 0 .. (w - 1) ]
  ]
  where
    Size w h = getSize img
    renderCell i j =
      D.rect 1 1
      # D.translateX (fromIntegral i + 0.5)
      # D.translateY (fromIntegral j + 0.5)
      # D.lwO 0
      # D.fc (c (getPixel img (Ix (I i) (J j))))
      # D.opacity (abs (realToFrac (getPixel img (Ix (I i) (J j)))))
    c f
      | f < 0     = sRGB24read "#E87ABC"
      | otherwise = sRGB24read "#9FE87A"

pxProject :: Real a => (Ix, ClipSeg a) -> Diagram B
pxProject (Ix _ (J j), cSeg) =
  D.rect (qx - px) 1
  # D.translateX (px + (qx - px)/2)
  # D.translateY (yn + 0.5)
  # D.lwO 0
  # D.fc D.green
  where
    Seg _ (P px' _) (P qx' _) _ = unClipSeg cSeg

    px = realToFrac px'
    qx = realToFrac qx'

    yn = fromIntegral j

areaDivision :: Real a => (Ix, ClipSeg a, PxDivision a) -> Diagram B
areaDivision (Ix _ (J j), cSeg, pxDivision) =
  mconcat
  [ aRect
  , bTri
  , cRect
  ]
  # D.lwO 0
  # D.fc D.green
  where
    PxDivision dxa' dxb' dyb' dyc' = pxDivision
    Seg _ (P px' py') (P qx' qy') _ = unClipSeg cSeg

    px = realToFrac px'
    qx = realToFrac qx'
    dxa = realToFrac dxa'
    dxb = realToFrac dxb'
    dyb = realToFrac dyb'
    dyc = realToFrac dyc'
    yn = fromIntegral j

    aRect :: Diagram B
    aRect | dxa < 1e-3 = mempty
          | nwse = D.rect dxa 1
                   # D.translateX (px + dxa/2)
                   # D.translateY (yn + 1/2)
          | otherwise = D.rect dxa 1
                        # D.translateX (qx - dxa/2)
                        # D.translateY (yn + 1/2)

    bTri :: Diagram B
    bTri | nwse = D.fromVertices
                  [ D.mkP2 0 0
                  , D.mkP2 dxb 0
                  , D.mkP2 0 dyb
                  ]
                  # D.closeLine
                  # D.strokeLoop
                  # D.translateX (px + dxa)
                  # D.translateY (yn + dyc)
         | otherwise = D.fromVertices
                       [ D.mkP2 0 0
                       , D.mkP2 dxb 0
                       , D.mkP2 dxb dyb
                       ]
                       # D.closeLine
                       # D.strokeLoop
                       # D.translateX (qx - dxa - dxb)
                       # D.translateY (yn + dyc)

    cRect :: Diagram B
    cRect | dyc < 1e-3 = mempty
          | nwse = D.rect dxb dyc
                   # D.translateX (px + dxa + dxb/2)
                   # D.translateY (yn + dyc/2)
          | otherwise = D.rect dxb dyc
                        # D.translateX (qx - dxa - dxb/2)
                        # D.translateY (yn + dyc/2)

    nwse :: Bool
    nwse = py' >= qy'

clipSeg :: Real a => ClipSeg a -> Diagram B
clipSeg cSeg =
  D.fromVertices
  [ D.mkP2 (realToFrac px) (realToFrac py)
  , D.mkP2 (realToFrac qx) (realToFrac qy) ]
  # D.lwO 10
  # D.lc (sRGB24read "#C62591")
  where
    Seg _ (P px py) (P qx qy) _ = unClipSeg cSeg

segment :: Real a => Seg a -> Diagram B
segment (Seg _ (P px py) (P qx qy) _) =
  D.fromVertices
  [ D.mkP2 (realToFrac px) (realToFrac py)
  , D.mkP2 (realToFrac qx) (realToFrac qy) ]
  # D.lwO 5
  # D.lc (sRGB24read "#C62509")

pixelGridLines :: Size -> Diagram B
pixelGridLines (Size w h) =
  mconcat (vertLines <> horizLines)
  # D.lwO 2
  # D.fc D.black
  # D.opacity 0.5
  where
    vertLines =
      [ D.fromVertices [ D.mkP2 x 0, D.mkP2 x (fromIntegral h) ]
      | x <- fromIntegral <$> [0 .. w] ]
    horizLines =
      [ D.fromVertices [ D.mkP2 0 y, D.mkP2 (fromIntegral w) y ]
      | y <- fromIntegral <$> [0 .. h] ]

pathOutline :: forall a. Real a => Path a -> Diagram B
pathOutline (Path loops) =
  mconcat (loopToPath <$> loops)
  # D.lwO 10
  # D.lc (sRGB24read "#EFC6A4")
  # D.lineCap D.LineCapRound
  # D.lineJoin D.LineJoinRound
  where
    loopToPath :: Loop a -> Diagram B
    loopToPath (Loop []) = D.fromVertices []
    loopToPath (Loop pts@(p:_)) =
      D.fromVertices (toP2 <$> (pts <> [p]))

    toP2 :: P a -> D.P2 Double
    toP2 (P x y) = D.mkP2 (realToFrac x) (realToFrac y)

data Path a = Path [Loop a]
data Loop a = Loop [P a]

pathToSegs :: (Ord a, Fractional a) => Epsilon a -> Path a -> [Seg a]
pathToSegs eps (Path ls) = concat $ loopToSegs eps <$> ls

loopToSegs :: forall a. (Ord a, Fractional a) => Epsilon a -> Loop a -> [Seg a]
loopToSegs _ (Loop [])        = []
loopToSegs _ (Loop [_,_])     = []
loopToSegs eps (Loop (pf:ps)) = catMaybes (followTrail pf (pf:ps))
  where
    followTrail :: P a -> [P a] -> [Maybe (Seg a)]
    followTrail _ []        = []
    followTrail firstPt [p] = [seg eps p firstPt]
    followTrail firstPt (p : q : ss) =
      seg eps p q : followTrail firstPt (q : ss)

hashSymbol :: Path Float
hashSymbol =
  Path
  [ Loop
    [ P  3.70  1.00  -- bottom right corner; outer loop
    , P  5.00  1.00
    , P  5.78  4.90
    , P  8.78  4.90
    , P  8.00  1.00
    , P  9.30  1.00
    , P 10.08  4.90
    , P 13.00  4.90
    , P 13.00  6.30
    , P 10.36  6.30
    , P 11.02  9.60
    , P 13.00  9.60
    , P 13.00 11.00
    , P 11.30 11.00
    , P 12.10 15.00
    , P 10.80 15.00
    , P 10.00 11.00
    , P  7.00 11.00
    , P  7.80 15.00
    , P  6.50 15.00
    , P  5.70 11.00
    , P  3.00 11.00
    , P  3.00  9.60
    , P  5.42  9.60
    , P  4.76  6.30
    , P  3.00  6.30
    , P  3.00  4.90
    , P  4.48  4.90
    ]
  , Loop
    [ P  6.72  9.60 -- top left corner; inner loop
    , P  9.72  9.60
    , P  9.06  6.30
    , P  6.06  6.30
    ]
  ]
