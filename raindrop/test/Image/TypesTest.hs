{-|
Module      : Image.TypesTest
Description : Tests for the Image.Types module.
-}
{-# LANGUAGE OverloadedStrings #-}
module Image.TypesTest
  ( tests
  )
where

import           Hedgehog                       ( (===) )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Image.Types                    ( I(I)
                                                , J(J)
                                                , Size(Size)
                                                , inRangeI
                                                , inRangeJ
                                                )

tests :: H.Group
tests = H.Group
  "Image.Types"
  [ ("inRangeI is in range", prop_inRangeI)
  , ("inRangeJ is in range", prop_inRangeJ)
  ]

prop_inRangeI :: H.Property
prop_inRangeI = H.withTests 25 $ H.property $ do
  width  <- H.forAll $ Gen.integral (Range.linear 0 128)
  height <- H.forAll $ Gen.integral (Range.linear 0 128)
  i      <- H.forAll $ I <$> Gen.integral (Range.linear 0 256)
  let size = Size width height
  inRangeI size i === (fromIntegral i < width)

prop_inRangeJ :: H.Property
prop_inRangeJ = H.withTests 25 $ H.property $ do
  width  <- H.forAll $ Gen.integral (Range.linear 0 128)
  height <- H.forAll $ Gen.integral (Range.linear 0 128)
  j      <- H.forAll $ J <$> Gen.integral (Range.linear 0 256)
  let size = Size width height
  inRangeJ size j === (fromIntegral j < height)
