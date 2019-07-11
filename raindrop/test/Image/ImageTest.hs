{-|
Module      : Image.ImageTest
Description : Tests for the Eggshell.Image module.
-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Image.ImageTest (tests) where

import           Hedgehog                     (Group (Group), Property, forAll,
                                               property, (===))
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range

import           Control.Monad.ST             (runST)
import           Data.Foldable                (traverse_)
import           Data.Mutable                 (asDLList, asSRef, modifyRef,
                                               newColl, newRef, popFront,
                                               pushBack, readRef)
import           Data.Vector.Storable         (Storable)
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as M

import qualified Image
import           Image.Loop                   (loop)

tests :: Group
tests = Group "Eggshell.Image"
  [ ( "prop_numbered", prop_numbered )
  , ( "prop_freeze_conserves_size", prop_freeze_conserves_size )
  , ( "prop_initialised_value", prop_initialised_value )
  , ( "prop_loopI_Up", prop_loopI_Up )
  , ( "prop_loopI_Down", prop_loopI_Down )
  , ( "prop_loopI_UpTo", prop_loopI_UpTo )
  , ( "prop_loopI_DownTo", prop_loopI_DownTo )
  , ( "prop_loopJ_Up", prop_loopJ_Up )
  , ( "prop_loopJ_Down", prop_loopJ_Down )
  , ( "prop_loopJ_UpTo", prop_loopJ_UpTo )
  , ( "prop_loopJ_DownTo", prop_loopJ_DownTo )
  ]

-- Creating an image with row-major numbered cells and then outputting it in
-- row-major format results in the correct numbering.
prop_numbered :: Property
prop_numbered = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  let sz = Image.Size width height
  let img = runST $ do
        -- create a mutable image
        mimg <- Image.newMutable sz (0 :: Int)
        -- set all the values to their row major index
        Image.loopI sz 0 Image.Up $ \i ->
          Image.loopJ sz 0 Image.Up $ \j -> do
            let val = Image.unI i + width * Image.unJ j
            Image.unsafeModify mimg (+ val) (Image.Ix i j)
        -- freeze the image
        Image.unsafeFreeze mimg
  let vec = Image.rowMajor img
  vec === V.fromList [ 0 .. (width*height - 1) ]

-- Freezing an image conserves its size.
prop_freeze_conserves_size :: Property
prop_freeze_conserves_size = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  let sz = Image.Size width height
  let img = runST $ Image.newMutable sz (0 :: Int) >>= Image.unsafeFreeze
  Image.getSize img === sz

-- The initialised value of an image is retained.
prop_initialised_value :: Property
prop_initialised_value = property $ do
  width   <- forAll $ Gen.int (Range.linear 0 16)
  height  <- forAll $ Gen.int (Range.linear 0 16)
  initVal <- forAll $ Gen.int (Range.linear -1024 1024)
  let sz = Image.Size width height
  let img = runST $ Image.newMutable sz initVal >>= Image.unsafeFreeze
  traverse_ (=== initVal) (V.toList (Image.rowMajor img))

---- Test image loops
-- These tests involve looping over either the i or j coordinates of the image
-- in various ways and checking that the loop actually hit the correct indices
-- in the correct order.

prop_loopI_Up :: Property
prop_loopI_Up = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (width - 1))
  let sz = Image.Size width height
  let xs = Image.unI <$> createM (Image.loopI sz (Image.I start) Image.Up)
  xs === [start .. width - 1]

prop_loopI_Down :: Property
prop_loopI_Down = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (width - 1))
  let sz = Image.Size width height
  let xs = Image.unI <$> createM (Image.loopI sz (Image.I start) Image.Down)
  xs === reverse [0 .. start]

prop_loopI_UpTo :: Property
prop_loopI_UpTo = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (width - 1))
  end    <- forAll $ Gen.int (Range.linear start (width - 1))
  let sz = Image.Size width height
  let xs = Image.unI
        <$> createM (Image.loopI sz (Image.I start) (Image.UpTo (Image.I end)))
  xs === [start .. end]

prop_loopI_DownTo :: Property
prop_loopI_DownTo = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (width - 1))
  end    <- forAll $ Gen.int (Range.linear 0 start)
  let sz = Image.Size width height
  let xs = Image.unI
        <$> createM (Image.loopI sz (Image.I start) (Image.DownTo (Image.I end)))
  xs === reverse [end .. start]

prop_loopJ_Up :: Property
prop_loopJ_Up = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (height - 1))
  let sz = Image.Size width height
  let xs = Image.unJ <$> createM (Image.loopJ sz (Image.J start) Image.Up)
  xs === [start .. height - 1]

prop_loopJ_Down :: Property
prop_loopJ_Down = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (height - 1))
  let sz = Image.Size width height
  let xs = Image.unJ <$> createM (Image.loopJ sz (Image.J start) Image.Down)
  xs === reverse [0 .. start]

prop_loopJ_UpTo :: Property
prop_loopJ_UpTo = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (height - 1))
  end    <- forAll $ Gen.int (Range.linear start (height - 1))
  let sz = Image.Size width height
  let xs = Image.unJ
        <$> createM (Image.loopJ sz (Image.J start) (Image.UpTo (Image.J end)))
  xs === [start .. end]

prop_loopJ_DownTo :: Property
prop_loopJ_DownTo = property $ do
  width  <- forAll $ Gen.int (Range.linear 0 16)
  height <- forAll $ Gen.int (Range.linear 0 16)
  start  <- forAll $ Gen.int (Range.linear 0 (height - 1))
  end    <- forAll $ Gen.int (Range.linear 0 start)
  let sz = Image.Size width height
  let xs = Image.unJ
        <$> createM (Image.loopJ sz (Image.J start) (Image.DownTo (Image.J end)))
  xs === reverse [end .. start]

createM
  :: Storable a
  => (forall m. Monad m => (a -> m ()) -> m ())
  -> [a]
createM run = runST $ do
  countRef <- asSRef <$> newRef (0 :: Int)
  dl       <- asDLList <$> newColl
  run $ \x -> do
    pushBack dl x
    modifyRef countRef (+ 1)
  count <- readRef countRef
  mvec <- M.new count
  loop 0 (< count) (+1) $ \i -> do
    mval <- popFront dl
    case mval of
      Nothing  -> pure ()
      Just val -> M.write mvec i val
  V.toList <$> V.freeze mvec
