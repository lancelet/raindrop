{-|
Module      : Main
Description : Main entry point for tests.
-}
module Main where

-- import qualified Test.DocTest  as DocTest
import           Hedgehog             (Group, checkParallel)

import qualified AlphaStencil.SegTest as Seg (tests)

main :: IO ()
main = do
  runHedgehogTests
  runDocTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ checkParallel hedgehogTests
  putStrLn "---- Finished Hedgehog Tests ----"

hedgehogTests :: [Group]
hedgehogTests =
  [ Seg.tests
  ]

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running DocTests ----"
  docTests
  putStrLn "---- Finished DocTests ----"

docTests :: IO ()
docTests = pure ()
  {-
  DocTest.doctest
  [ "-isrc"
  , "src/Eggshell.hs"
  ]
  -}
