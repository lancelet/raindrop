{-|
Module      : Image.MutableTest
Description : Tests for the Image.Mutable module.
-}
{-# LANGUAGE OverloadedStrings #-}
module Image.MutableTest (tests) where

import           Hedgehog             ((===))
import qualified Hedgehog             as H
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Control.Monad.ST     (runST)
import qualified Data.Vector.Storable as V
import           Data.Word            (Word16, Word8)

import qualified Image
import           Image.Loop           (inc, loop)
import           Image.Mutable        (lindexUnsafe, new, unsafeModify)
import           Image.Types          (I (I), Ix (Ix), J (J), Size (Size),
                                       inRangeI, inRangeJ)

tests :: H.Group
tests = H.Group "Image.Mutable"
  [ ("mutable image creation sanity test", unit_mutable_image_creation)
  , ("mutable image creation; row major indices", prop_row_major_indices)
  ]

-- | Unit test for creating a mutable image and modifying its values.
unit_mutable_image_creation :: H.Property
unit_mutable_image_creation = H.withTests 1 $ H.property $ do
  let image = runST $ do
        mImage <- new (Size 3 2) (1 :: Word8)
        unsafeModify mImage (+2) (Ix (I 1) (J 0))
        unsafeModify mImage (+4) (Ix (I 0) (J 1))
        unsafeModify mImage (+6) (Ix (I 2) (J 1))
        unsafeModify mImage (+8) (Ix (I 1) (J 0))
        unsafeModify mImage (+1) (Ix (I 1) (J 1))
        unsafeModify mImage (+3) (Ix (I 0) (J 0))
        Image.unsafeFreeze mImage
  Image.size image === Size 3 2
  Image.rowMajor image === V.fromList [ 4, 11, 1, 5, 2, 7 ]

-- | Create randomly-sized images filled with their row-major indices.
prop_row_major_indices :: H.Property
prop_row_major_indices = H.property $ do
  width  <- H.forAll $ Gen.integral (Range.linear 0 64)
  height <- H.forAll $ Gen.integral (Range.linear 0 64)
  let size = Size width height
  let nElems = fromIntegral $ width * height
  let image = runST $ do
        mImage <- new size (0 :: Word16)
        loop 0 (inRangeI size) inc $ \i -> do
          loop 0 (inRangeJ size) inc $ \j -> do
            let rowMajIndex = fromIntegral $ lindexUnsafe size (Ix i j)
            unsafeModify mImage (+ rowMajIndex) (Ix i j)
        Image.unsafeFreeze mImage
  Image.size image === size
  if nElems == 0
    then Image.rowMajor image === V.empty
    else Image.rowMajor image === V.fromList [ 0 .. (nElems - 1) ]
