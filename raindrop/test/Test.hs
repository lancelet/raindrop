{-|
Module      : Main
Description : Main entry point for tests.
-}
module Main where

import           Hedgehog                (Group, checkParallel)
import qualified Test.DocTest            as DocTest

import qualified AlphaStencil.LogTest    as AlphaStencil.Log (tests)
import qualified AlphaStencil.RenderTest as AlphaStencil.Render (tests)
import qualified AlphaStencil.SegTest    as AlphaStencil.Seg (tests)
import qualified Image.LoopTest          as Image.Loop (tests)
import qualified Image.MutableTest       as Image.Mutable (tests)
import qualified Image.TypesTest         as Image.Types (tests)
import qualified ImageTest               as Image (tests)

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
  [ AlphaStencil.Log.tests
  , AlphaStencil.Render.tests
  , AlphaStencil.Seg.tests
  , Image.tests
  , Image.Loop.tests
  , Image.Mutable.tests
  , Image.Types.tests
  ]

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running DocTests ----"
  docTests
  putStrLn "---- Finished DocTests ----"

docTests :: IO ()
docTests =
  DocTest.doctest
  [ "-isrc"
  , "src/AlphaStencil/Log.hs"
  , "src/AlphaStencil/Render.hs"
  , "src/AlphaStencil/Seg.hs"
  , "src/Image.hs"
  , "src/Image/Loop.hs"
  , "src/Image/Mutable.hs"
  , "src/Image/Types.hs"
  ]
