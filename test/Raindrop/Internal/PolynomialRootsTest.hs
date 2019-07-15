{-|
Module      : Raindrop.Internal.PolynomialRootsTest
Description : Tests for the PolynomialRoots package.
-}
{-# LANGUAGE FlexibleContexts #-}
module Raindrop.Internal.PolynomialRootsTest
  ( tests
  ) where

import           GHC.Stack                         (HasCallStack)
import qualified Hedgehog                          as HH
import qualified Hedgehog.Gen                      as Gen
import qualified Hedgehog.Range                    as Range
import qualified Test.Tasty                        as Tasty
import           Test.Tasty.Hedgehog               (testProperty)

import           Linear                            (nearZero)

import           Raindrop.Internal.PolynomialRoots (solveLinear)


tests :: Tasty.TestTree
tests = Tasty.testGroup "Raindrop.Internal.PolynomialRoots"
  [ testProperty
    "solving a linear equation"
    prop_solveLinear
  ]


axisLim :: Float
axisLim = 64000.0


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


prop_solveLinear :: HH.Property
prop_solveLinear = HH.property $ do
  root  <- HH.forAll $ Gen.float (Range.linearFrac (-axisLim) axisLim)
  slope <- HH.forAll $ Gen.float (Range.linearFrac (-1e10) 1e10)
  let intercept = -root * slope
  let solvedRoot = solveLinear slope intercept
  case solvedRoot of
    Just r  -> approxEq 1e-2 root r
    Nothing -> HH.assert (nearZero slope)
