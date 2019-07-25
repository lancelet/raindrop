{-|
Module      : Raindrop.Internal.Geom.Bezier3
Description : Cubic Bezier curves.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Raindrop.Internal.Geom.Bezier3
  ( -- * Types
    Bezier3(Bezier3)
    -- * Functions
  , windingNum
  , distanceTo
  ) where

import           Control.Lens                      ((^.))
import           Control.Monad.Loops               (untilM_)
import           Control.Monad.ST                  (runST)
import           Data.Massiv.Array                 (Array, Comp (Seq), Ix1,
                                                    Sz (Sz), U, Unbox, ifoldlS,
                                                    makeArray, (!))
import           Data.STRef                        (newSTRef, readSTRef,
                                                    writeSTRef)
import           Debug.Trace                       (traceM)
import           Linear                            (Epsilon, nearZero)

import           Raindrop.Internal.Geom.Bezier2    (Bezier2 (Bezier2))
import qualified Raindrop.Internal.Geom.Bezier2    as Bezier2
import qualified Raindrop.Internal.Geom.LineSeg    as LineSeg
import           Raindrop.Internal.Geom.Vec        (P, V, distance, dot, norm,
                                                    normalize, p2v, qd,
                                                    scalarCross, (*^), _y)
import           Raindrop.Internal.Interval        (clamp, mkClosedInterval)
import           Raindrop.Internal.PolynomialRoots (filterMaybeThree,
                                                    solveCubic)


data Bezier3 a
  = Bezier3
    !(P a)
    !(P a)
    !(P a)
    !(P a)


eval :: (Num a) => Bezier3 a -> a -> P a
eval (Bezier3 pa pb pc pd) t =
  let
    t2  = t*t
    t3  = t2*t
    mt  = 1 - t
    mt2 = mt*mt
    mt3 = mt2*mt
  in
    mt3*^pa + 3*mt2*t*^pb + 3*mt*t2*^pc + t3*^pd
{-# INLINE eval #-}


deriv :: (Num a) => Bezier3 a -> Bezier2 a
deriv (Bezier3 pa pb pc pd) = Bezier2 (3*(pb - pa)) (3*(pc - pb)) (3*(pd - pc))
{-# INLINE deriv #-}


{-
This is a bit of a hack. For this to work reliably, I will probably have to
split Beziers in advance so that they don't extend beyond a certain number
of pixels.
-}
inBezierParamRange :: (Ord a, Fractional a) => a -> Bool
inBezierParamRange t = (t >= -0.000001) && (t <= 1.000001)
{-# INLINE inBezierParamRange #-}


tangent :: (Epsilon a, Floating a) => Bezier3 a -> a -> V a
tangent b t = normalize $ p2v $ Bezier2.eval (deriv b) t
{-# INLINE tangent #-}


windingNum :: (Ord a, RealFrac a, Floating a, Epsilon a) => Bezier3 a -> P a -> Int
windingNum bez@(Bezier3 pa pb pc pd) p = sum $ fmap wn ts
  where
    y  = p^._y
    ya = pa^._y - y
    yb = pb^._y - y
    yc = pc^._y - y
    yd = pd^._y - y

    a = -ya + 3*yb - 3*yc + yd
    b = 3*ya - 6*yb + 3*yc
    c = -3*ya + 3*yb
    d = ya

    ts' = solveCubic nearZero a b c d
    ts = filterMaybeThree inBezierParamRange ts'

    wn t = if onLeft then 1 else -1
      where
        onLeft = tangent bez t `scalarCross` p2v (p - px) > 0
        px = eval bez t
{-# INLINE windingNum #-}


minIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Array U Ix1 a -> Ix1
minIndexBy cmp array = fst $ ifoldlS fn z array
  where
    z = (0, array ! 0)
    fn c@(_, currentMin) i val
      | cmp val currentMin == LT = (i, val)
      | otherwise                =  c
{-# INLINE minIndexBy #-}


distanceTo :: forall a. (Show a, Unbox a, Floating a, Ord a, Epsilon a) => Int -> a -> a -> Bezier3 a -> P a -> a
distanceTo nTabulatedPoints eps1 eps2 bez p =
  let
    ptTable :: Array U Ix1 (P a)
    ptTable = makeArray Seq (Sz nTabulatedPoints)
              $ \i ->
                  let
                    t = fromIntegral i / fromIntegral (nTabulatedPoints - 1)
                  in
                    eval bez t
  in
    let
      cmpDistance :: P a -> P a -> Ordering
      cmpDistance p1 p2 = compare (qd p p1) (qd p p2)

      minIx :: Ix1
      minIx = minIndexBy cmpDistance ptTable

      initT :: a
      initT = fromIntegral minIx / fromIntegral (nTabulatedPoints - 1)

      finalT :: a
      finalT = clamp (mkClosedInterval 0 1) $ nrMin eps1 eps2 bez p initT
    in
      distance p (eval bez finalT)
{-# INLINE distanceTo #-}


{-
eps1 = a measure of Euclidean distance
eps2 = a zero cosine measure
-}
nrMin :: forall a. (Floating a, Ord a, Show a, Epsilon a) => a -> a -> Bezier3 a -> P a -> a -> a
nrMin eps1 eps2 bez p tInit = runST $ do

  let maxIterations = 100
  nIterationsRef <- newSTRef 0

  tRef <- newSTRef tInit
  terminateRef <- newSTRef False
  let

    b2 = deriv bez
    b1 = Bezier2.deriv b2

    iteration = do
      t <- readSTRef tRef
      let
        r   = eval bez t
        r'  = Bezier2.eval b2 t
        r'' = LineSeg.eval b1 t

        pr = r - p
        g  = r' `dot` pr
        g' = (r'' `dot` pr) + (r' `dot` r')

        t' = t - g / g'

        ptCoincidence = norm pr <= eps1
        zeroCosine = abs g / norm r' / norm pr <= eps2
        noProgress = norm ((t' - t)*^r') <= eps1
        outOfRange = t < 0 || t > 1

        terminate = ptCoincidence || zeroCosine || noProgress || outOfRange

      nIterations <- readSTRef nIterationsRef
      if nIterations >= maxIterations
        then error
             $ "Maximum number of iterations exceeded! "
             <> "ptCoincidence = " <> show ptCoincidence
             <> ", zeroCosine = " <> show zeroCosine
             <> ", noProgress = " <> show noProgress
             <> ", pr = " <> show pr
             <> ", g = " <> show g
             <> ", g' = " <> show g'
             <> ", t = " <> show t
             <> ", t' = " <> show t'
        else pure ()
      writeSTRef nIterationsRef (nIterations + 1)

      writeSTRef tRef t'
      writeSTRef terminateRef terminate

  untilM_ iteration (readSTRef terminateRef)
  clamp (mkClosedInterval 0 1) <$> readSTRef tRef
{-# INLINE nrMin #-}
