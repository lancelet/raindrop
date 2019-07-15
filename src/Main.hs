{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Debug.Trace (trace)
import           Data.Massiv.Array     (Comp (Seq), Ix2 ((:.)), S, Sz (Sz))
import qualified Data.Massiv.Array     as Massiv
import           Data.Massiv.Array.IO  (Image)
import qualified Data.Massiv.Array.IO  as Massiv
import           Data.Word             (Word8)
import           Graphics.ColorSpace.Y (Pixel(PixelY), Y)
import           Lens.Micro            ((^.))
import           Linear                (_x, _y, dot, quadrance, (*^), (^+^), normalize)
import qualified Linear
import           Linear.Affine         ((.-.), (.+^), distanceA)
import qualified Linear.Affine

type V a = Linear.V2 a
type Pt a = Linear.Affine.Point Linear.V2 a

mkV :: a -> a -> V a
mkV x y = Linear.V2 x y

mkPt :: a -> a -> Pt a
mkPt x y = Linear.Affine.P (Linear.V2 x y)

p2v :: Pt a -> V a
p2v (Linear.Affine.P v) = v

v2p :: V a -> Pt a
v2p v = Linear.Affine.P v

main :: IO ()
main = do
  putStrLn "raindrop - early dev"
  test

data PathComponent a
  = PCLine {-# UNPACK #-} !(Line a)
  | PCBezier3 {-# UNPACK #-} !(Bezier3 a)

data Line a
  = Line
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)

data Bezier2 a
  = Bezier2
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)

data Bezier3 a
  = Bezier3
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)
    {-# UNPACK #-} !(Pt a)

defApproxZero :: (Fractional a, Ord a) => a -> Bool
defApproxZero x = abs x < 1e-3

solveQuadratic :: (Floating a, Ord a) => (a -> Bool) -> a -> a -> a -> [a]
solveQuadratic approxZero a b c
  | approxZero den = []
  | qd < 0         = []
  | otherwise      = [(-b+sqd)/2*a, (-b-sqd)/2*a]
  where
    sqd = sqrt qd
    qd = b^(2 :: Int) - 4*a*c
    den = 2*a

-- | TODO: May not cover all edge cases.
solveCubic :: (Floating a, Ord a) => (a -> Bool) -> a -> a -> a -> a -> [a]
solveCubic approxZero a b c d
  | approxZero a   = solveQuadratic approxZero b c d
  | w > 0 && v < 0 = [ cr ((w-v)/2) - (u/3) * cr (2/(w-v)) - p/3 ]
  | w > 0          = [ cr ((w+v)/2) - (u/3) * cr (2/(w+v)) - p/3 ]
  | otherwise      = [ s*(cos k) - p/3
                     , s*(-(cos k) + (sqrt 3)*(sin k))/2 - p/3
                     , s*(-(cos k) - (sqrt 3)*(sin k))/2 - p/3 ]
  where
    u = q - p
    v = r - p*q/3 + 2*p*p*p/27
    w = 4 * (u/3)^(3 :: Int) + v*v
    p = b / a
    q = c / a
    r = d / a
    s = sqrt(-u/3)
    t = -v/(2*s*s*s)
    k = 1/3 * acos t
    cr x = x ** (1/3)  -- cube root

solveCubic2 :: (Floating a, Ord a, Show a) => (a -> Bool) -> a -> a -> a -> a -> [a]
solveCubic2 approxZero a' b' c' d'
  | approxZero a      = solveQuadratic approxZero b' c' d'
  | discriminant <= 0 = filter tInRange [ root1, root2, root3 ]
  | otherwise         = filter tInRange [ singleRoot ]
  where
    a = a' / d'
    b = b' / d'
    c = c' / d'

    p = (3*b - a*a)/3
    p3 = p/3
    q = (2*a*a*a - 9*a*b + 27*c)/27
    q2 = q/2
    discriminant = q2*q2 + p3*p3*p3

    tInRange t = t >= 0 && t <= 1

    mp3 = -p/3
    mp33 = mp3*mp3*mp3
    r = sqrt mp33
    t = -q / (2*r)
    cosphi = clamp (-1) 1 t
    phi = acos(cosphi)
    crtr = r ** (1/3)
    t1 = 2*crtr
    root1 = t1 * cos (phi/3) - a/3
    root2 = t1 * cos ((phi + 2*pi)/3) - a/3
    root3 = t1 * cos ((phi + 4*pi)/3) - a/3

    cuberoot x | x < 0     = -((-x)**(1/3))
               | otherwise = x**(1/3)

    sd = sqrt discriminant
    u1 = cuberoot (sd - q2)
    v1 = cuberoot (sd + q2)
    singleRoot' = u1 - v1 - a/3
    singleRoot = trace ("singleRoot; t = " <> show singleRoot'
                       <> ", discriminant = " <> show discriminant
                       <> ", sd = " <> show sd
                       <> ", u1 = " <> show u1
                       <> ", v1 = " <> show v1) singleRoot'


{-
bezier3DerivRoots :: (Floating a, Ord a) => a -> a -> a -> a -> [a]
bezier3DerivRoots p1 p2 p3 p4 = solveQuadratic defApproxZero a b c
  where
    a = 3*(-p1 + 3*p2 - 3*p3 + p4)
    b = 6*(p1 - 2*p2 + p3)
    c = 3*(p2 - p1)
-}

bezier3Roots :: (Floating a, Ord a, Show a) => a -> a -> a -> a -> [a]
bezier3Roots p1 p2 p3 p4 = solveCubic2 defApproxZero a b c d
  where
    a = 3*p1 - 6*p2 + 3*p3
    b = -3*p1 + 3*p2
    c = p1
    d = -p1 + 3*p2 - 3*p3 + p4

{-
bezier3TransY :: (Num a) => a -> Bezier3 a -> Bezier3 a
bezier3TransY dy (Bezier3 p1 p2 p3 p4) = Bezier3 p1' p2' p3' p4'
  where
    v = mkV 0 dy
    p1' = p1 .+^ v
    p2' = p2 .+^ v
    p3' = p3 .+^ v
    p4' = p4 .+^ v
-}

derivBezier3 :: (Num a) => Bezier3 a -> Bezier2 a
derivBezier3 (Bezier3 a b c d) = Bezier2 (3*(b - a)) (3*(c - b)) (3*(d-c))

evalBezier2 :: (Num a) => Bezier2 a -> a -> Pt a
evalBezier2 (Bezier2 p1 p2 p3) t = v2p $ c1*^v1 ^+^ c2*^v2 ^+^ c3*^v3
  where
    t2 = t*t
    mt = 1 - t
    mt2 = mt*mt
    c1 = mt2
    c2 = 2*mt*t
    c3 = t2
    v1 = p2v p1
    v2 = p2v p2
    v3 = p2v p3

evalBezier3 :: (Num a) => Bezier3 a -> a -> Pt a
evalBezier3 (Bezier3 p1 p2 p3 p4) t = v2p $ c1*^v1 ^+^ c2*^v2 ^+^ c3*^v3 ^+^ c4*^v4
  where
    t2 = t*t
    t3 = t2*t
    mt = 1-t
    mt2 = mt*mt
    mt3 = mt2*mt
    c1 = mt3
    c2 = 3*mt2*t
    c3 = 3*mt*t2
    c4 = t3
    v1 = p2v p1
    v2 = p2v p2
    v3 = p2v p3
    v4 = p2v p4

bezier3Tangent :: (Floating a, Linear.Epsilon a) => Bezier3 a -> a -> V a
bezier3Tangent b t = v
  where
    v = normalize $ p2v $ evalBezier2 (derivBezier3 b) t

{-
bezier3YRange :: (Floating a, Ord a) => Bezier3 a -> (a, a)
bezier3YRange (Bezier3 p1 p2 p3 p4) = (minimum cand, maximum cand)
  where
    cand = [p1^._y, p4^._y] ++ bezier3DerivRoots (p1^._y) (p2^._y) (p3^._y) (p4^._y)
-}

scalarCross :: (Num a) => V a -> V a -> a
scalarCross v1 v2 = v1^._x * v2^._y - v1^._y * v2^._x

clamp :: (Ord a) => a -> a -> a -> a
clamp minVal maxVal x | x < minVal = minVal
                      | x > maxVal = maxVal
                      | otherwise  = x

smoothStep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothStep minVal maxVal x | x < minVal = 0
                           | x > maxVal = 1
                           | otherwise  = 6*x'^(5 :: Int)
                                          - 15*x'^(4 :: Int)
                                          + 10*x'^(3 :: Int)
  where
    x' = (x - minVal) / (maxVal - minVal)

inRange :: (Ord a) => a -> a -> a -> Bool
inRange a1 a2 a | a2 > a1   = a >= a1 && a <= a2
                | otherwise = a >= a2 && a <= a1

pathComponentSDFSeg
  :: (Num a, Ord a, Floating a, Linear.Epsilon a, Show a)
  => PathComponent a
  -> SDFSeg a
pathComponentSDFSeg pathComponent =
  case pathComponent of
    PCLine line       -> lineSDFSeg line
    PCBezier3 bezier3 -> bezier3SDFSeg bezier3

lineSDFSeg :: forall a. (Num a, Ord a, Floating a) => Line a -> SDFSeg a
lineSDFSeg (Line p1 p2) = SDFSeg { windingNumber, distanceTo }
  where
    windingNumber :: Pt a -> Int
    windingNumber p = if inYRange then (if onLeft then 1 else -1) else 0
      where
        v = p .-. p1
        s = p2 .-. p1
        onLeft = v `scalarCross` s > 0  -- seems to be onRight, not onLeft
        inYRange = inRange (p1^._y) (p2^._y) (p^._y)

    distanceTo :: Pt a -> a
    distanceTo p = distanceA p q
      where
        v = p .-. p1
        s = p2 .-. p1
        t = clamp 0 1 $ (v `dot` s) / (quadrance s)
        q = p1 .+^ (t *^ s)

bezier3SDFSeg :: forall a. (Floating a, Ord a, Linear.Epsilon a, Show a) => Bezier3 a -> SDFSeg a
bezier3SDFSeg b@(Bezier3 p1 p2 p3 p4) = SDFSeg { windingNumber, distanceTo }
  where
    windingNumber :: Pt a -> Int
    windingNumber pt = sum $ map wn intersections
      where
        y = pt^._y
        -- wn t = if (pt .-. evalBezier3 b t) `scalarCross` (bezier3Tangent b t) > 0 then 1 else -1
        wn t = if (bezier3Tangent b t)^._y > 0 then -1 else 1
        tInRange t = (t >= 0) && (t <= 1)
        rightOfX t = (evalBezier3 b t)^._x > pt^._x
        intersections = {-filter rightOfX
                        $-} filter tInRange
                        $ bezier3Roots (p1^._y-y) (p2^._y-y) (p3^._y-y) (p4^._y-y)

    distanceTo :: Pt a -> a
    distanceTo pt = undefined


data SDFSeg a
  = SDFSeg
    { windingNumber :: Pt a -> Int
    , distanceTo    :: Pt a -> a
    }

sdfPolygon :: (Num a, Ord a) => [SDFSeg a] -> Pt a -> a
sdfPolygon segs pt = sdfSign * minDistance
  where
    minDistance = minimum $ fmap (`distanceTo` pt) segs
    sdfSign = if inPolygon segs pt then -1 else 1

aaPolygon :: (Fractional a, Ord a) => a -> [SDFSeg a] -> Pt a -> a
aaPolygon aaWidth segs pt = smoothStep (-a2) a2 sdf
  where
    a2 = aaWidth / 2
    sdf = sdfPolygon segs pt

inPolygon :: [SDFSeg a] -> Pt a -> Bool
inPolygon segs pt = totalWindingNumber > 0
  where
    totalWindingNumber = sum $ fmap (`windingNumber` pt) segs

-- | Example polygon; looks like a capital letter 'Y'.
ySDFSegs :: forall a. (Floating a, Ord a, Linear.Epsilon a, Show a) => [SDFSeg a]
ySDFSegs = fmap pathComponentSDFSeg yPolygon

-- | Example polygon; looks like a capital letter 'Y'.
yPolygon :: forall a. (Fractional a) => [PathComponent a]
yPolygon = fmap PCLine lineSegs
  where
    lineSegs :: [Line a]
    lineSegs
      = [ Line (mkPt 221.8 459.4) (mkPt 290.2 459.4)
        , Line (mkPt 290.2 459.4) (mkPt 290.2 229.0)
        , Line (mkPt 290.2 229.0) (mkPt 416.2  54.5)
        , Line (mkPt 416.2  54.4) (mkPt 328.5  54.4)
        , Line (mkPt 328.5  54.4) (mkPt 256.0 163.7)
        , Line (mkPt 256.0 163.7) (mkPt 183.5  54.4)
        , Line (mkPt 183.5  54.4) (mkPt  95.8  54.4)
        , Line (mkPt  95.8  54.4) (mkPt 221.8 229.0)
        , Line (mkPt 221.8 229.0) (mkPt 221.8 459.4)
        ]

bezSDFSegs :: forall a. (Floating a, Ord a, Linear.Epsilon a, Show a) => [SDFSeg a]
bezSDFSegs = fmap pathComponentSDFSeg bezPolygon

bezPolygon :: forall a. (Fractional a) => [PathComponent a]
bezPolygon
  = [ PCBezier3 $ Bezier3 (mkPt 286.7 395.9) (mkPt 316.2 23.0) (mkPt 372.9 468.1) (mkPt 403.7 169.9)
      --PCLine $ Line (mkPt 286.7 395.9) (mkPt 403.7 169.9)
    , PCLine $ Line (mkPt 403.7 169.9) (mkPt 460.4 169.9)
    , PCLine $ Line (mkPt 460.4 169.9) (mkPt 460.4  41.4)
    , PCLine $ Line (mkPt 460.4  41.4) (mkPt  78.9  41.4)
    , PCLine $ Line (mkPt  78.9  41.4) (mkPt  78.9 449.7)
    , PCLine $ Line (mkPt  78.9 449.7) (mkPt 403.7 449.7)
    , PCLine $ Line (mkPt 403.7 449.7) (mkPt 403.7 364.4)
    , PCLine $ Line (mkPt 403.7 364.4) (mkPt 329.0 364.4)
    , PCLine $ Line (mkPt 329.0 364.4) (mkPt 329.0 395.9)
    , PCLine $ Line (mkPt 329.0 395.9) (mkPt 286.7 395.9)
    ]

test :: IO ()
test = do
  let
    gen :: Ix2 -> Pixel Y Word8
    gen (j :. i) = if inPolygon segs pt then 255 else 0
      where
        segs :: [SDFSeg Float]
        segs = ySDFSegs

        pt :: Pt Float
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testInPolygonImg :: Image S Y Word8
    testInPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen

    gen2 :: Ix2 -> Pixel Y Word8
    gen2 (j :. i) = PixelY . floor $ sdfPolygon segs pt
      where
        segs = ySDFSegs :: [SDFSeg Float]
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testSDFPolygonImg :: Image S Y Word8
    testSDFPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen2

    gen3 :: Ix2 -> Pixel Y Word8
    gen3 (j :. i) = PixelY . floor $ 255 * aaPolygon 1.44 segs pt
      where
        segs = ySDFSegs :: [SDFSeg Float]
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testAAPolygonImg :: Image S Y Word8
    testAAPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen3

    gen4 :: Ix2 -> Pixel Y Word8
    gen4 (j :. i) = PixelY . floor $ 255 * aaPolygon 10 segs pt
      where
        segs = ySDFSegs :: [SDFSeg Float]
        pt = mkPt (fromIntegral i) (fromIntegral j)

    testWideAAPolygonImg :: Image S Y Word8
    testWideAAPolygonImg = Massiv.makeArray Seq (Sz (512 :. 512)) gen4


    gen5 :: Ix2 -> Pixel Y Word8
    gen5 (j :. i) = if inPolygon segs pt then 255 else 0
      where
        segs :: [SDFSeg Float]
        segs = bezSDFSegs

        pt :: Pt Float
        pt = mkPt (fromIntegral i) (fromIntegral j)
    testInBezImage :: Image S Y Word8
    testInBezImage = Massiv.makeArray Seq (Sz (512 :. 512)) gen5


  Massiv.writeImage "test-in-polygon.png" testInPolygonImg
  Massiv.writeImage "test-sdf.png" testSDFPolygonImg
  Massiv.writeImage "test-aa-polygon.png" testAAPolygonImg
  Massiv.writeImage "test-wide-aa-polygon.png" testWideAAPolygonImg

  Massiv.writeImage "test-in-bezier.png" testInBezImage
