{-|
Module      : Raindrop.Internal.PolynomialRootsTest
Description : Tests for the PolynomialRoots package.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
module Raindrop.Internal.PolynomialRootsTest
  ( tests
  ) where

import           GHC.Stack                         (HasCallStack)
import           Hedgehog                          ((===))
import qualified Hedgehog                          as HH
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import qualified Test.Tasty                        as Tasty
import           Test.Tasty.Hedgehog               (testProperty)

import           Data.Ratio                        ((%))
import           Linear.Epsilon                    (nearZero)

import           Raindrop.Internal.PolynomialRoots (MaybeTwo (M2None, M2One, M2Two),
                                                    maybeTwoToList, solveLinear,
                                                    solveQuadratic, maybeThreeToList,
                                                    solveCubic)


-- | All module tests.
tests :: Tasty.TestTree
tests = Tasty.testGroup "Raindrop.Internal.PolynomialRoots"
  [ testsLinear
  , testsQuadratic
  , testsCubic
  ]


-- | Tests for solving linear polynomials.
testsLinear :: Tasty.TestTree
testsLinear = Tasty.testGroup "linear equation roots"
  [ testProperty
    "solving a linear equation"
    prop_solveLinear
  ]


-- | Solve some linear equations, using exact rational arithmetic.
prop_solveLinear :: HH.Property
prop_solveLinear = HH.property $ do
  let
    genRational = do
      num <- Gen.integral $ Range.linear -64000 64000
      den <- Gen.integral $ Range.linear -64000 64000
      pure (num % den :: Rational)
  r <- HH.forAll genRational  -- root
  c <- HH.forAll genRational  -- slope
  let
    i = -r * c  -- intercept
    expected = if c == 0 then Nothing else Just r
  solveLinear (== 0) c i === expected


-- | Tests for solving quadratic polynomials.
testsQuadratic :: Tasty.TestTree
testsQuadratic = Tasty.testGroup "quadratic equation roots"
  [ testProperty
    "solutions of a quadratic are actually roots"
    prop_quadraticSolutions
  , testProperty
    "negative discriminants result in no solutions"
    prop_quadraticNegDiscriminant
  , testProperty
    "zero discriminant results in a single solution"
    prop_quadraticZeroDiscriminant
  ]


-- | Check that solutions returned for a quadratic are actually solutions.
prop_quadraticSolutions :: HH.Property
prop_quadraticSolutions = HH.property $ do
  b <- HH.forAll $ Gen.float (Range.linearFrac -100 100)
  c <- HH.forAll $ Gen.float (Range.linearFrac -100 100)
  d <- HH.forAll $ Gen.float (Range.linearFrac -100 100)
  let
    q t = b*t*t + c*t + d
    approxOk t = approxEq' 1e-2 0 (q t)
  HH.annotate $ "On failure: "
    <> "Roots: " <> show (solveQuadratic nearZero b c d) <> ". "
    <> "Quadratic evaluated at roots: "
    <> show (q <$> solveQuadratic nearZero b c d) <> "."
  HH.assert
    $ (all approxOk . maybeTwoToList)
    $ solveQuadratic nearZero b c d


-- | Check that negative discriminants of a quadratic result in no solutions.
prop_quadraticNegDiscriminant :: HH.Property
prop_quadraticNegDiscriminant = HH.property $ do
  b <- HH.forAll $ Gen.float (Range.linearFrac 1 100)
  d <- HH.forAll $ Gen.float (Range.linearFrac 1 100)
  let cMax = 0.8 * sqrt (4*b*d)
  c <- HH.forAll $ Gen.float (Range.linearFrac (-cMax) cMax)
  solveQuadratic nearZero b c d === M2None


-- | Check that a zero discriminant results in one solution.
prop_quadraticZeroDiscriminant :: HH.Property
prop_quadraticZeroDiscriminant = HH.property $ do
  b <- HH.forAll $ Gen.float (Range.linearFrac 1 100)
  d <- HH.forAll $ Gen.float (Range.linearFrac 1 100)
  let
    c = sqrt (4*b*d)
    q t = b*t*t + c*t + d
    looseNearZero t = abs t < 0.1
  case solveQuadratic looseNearZero b c d of
    M2One x -> approxEq 1e-4 0 (q x)
    r -> do
      HH.annotate $ "Expected a single root: " <> show r
      HH.failure


-- | Tests for solving cubic polynomials.
testsCubic :: Tasty.TestTree
testsCubic = Tasty.testGroup "cubic equation roots"
  [ testProperty
    "solutions of a cubic are actually roots"
    prop_cubicSolutions
  ]


-- | Check that solutions returned for a cubic are actually solutions.
prop_cubicSolutions :: HH.Property
prop_cubicSolutions = HH.property $ do
  let gen = Gen.float (Range.linearFrac -100 100)
  a <- HH.forAll gen
  b <- HH.forAll gen
  c <- HH.forAll gen
  d <- HH.forAll gen
  let
    q t = a*t*t*t + b*t*t + c*t + d
    approxOk t = approxEq' 20.0 0 (q t)
  HH.annotate $ "On failure: "
    <> "Roots: " <> show (solveCubic nearZero a b c d) <> ". "
    <> "Cubic evaluated at roots: "
    <> show (q <$> solveCubic nearZero a b c d) <> "."
  HH.assert
    $ (all approxOk . maybeThreeToList)
    $ solveCubic nearZero a b c d


-- | Check for approximate equality.
approxEq'
  :: (Floating a, Ord a)
  => a     -- ^ Epsilon value.
  -> a     -- ^ Expected.
  -> a     -- ^ Actual.
  -> Bool  -- ^ Is approximately equal?
approxEq' eps expected x = x >= minVal && x <= maxVal
  where
    eps' = abs eps
    minVal = expected - eps'
    maxVal = expected + eps'


-- | Check for approximate equality, failing in 'MonadTest'.
approxEq
  :: (HH.MonadTest m, Floating a, Ord a, Show a, HasCallStack)
  => a    -- ^ Epsilon value.
  -> a    -- ^ Expected.
  -> a    -- ^ Actual.
  -> m () -- ^ Monadic result.
approxEq eps expected x = HH.diff x (approxEq' eps) expected
