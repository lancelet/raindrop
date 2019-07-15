{-|
Module      : Main
Description : Tests entry point.
-}
module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Raindrop.Internal.PolynomialRootsTest (tests)

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests"
  [ Raindrop.Internal.PolynomialRootsTest.tests
  ]
