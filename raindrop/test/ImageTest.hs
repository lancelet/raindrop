{-|
Module      : ImageTest
Description : Tests for the Image module.
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ImageTest
  ( tests
  )
where

import           Hedgehog                       ( (===) )
import qualified Hedgehog                      as H

import           Control.Exception              ( ErrorCall
                                                  ( ErrorCallWithLocation
                                                  )
                                                , evaluate
                                                , try
                                                )
import qualified Data.Vector.Storable          as V
import           Data.Word                      ( Word8 )

import           Image                          ( I(I)
                                                , Ix(Ix)
                                                , J(J)
                                                , Size(Size)
                                                )
import qualified Image

tests :: H.Group
tests = H.Group
  "Image"
  [ ("set/get pixel sanity test", unit_set_get_pixel)
  , ("lindex error sanity test" , unit_lindex_error)
  ]

-- | Unit test for creating an image and modifying its value via get/set pixel.
unit_set_get_pixel :: H.Property
unit_set_get_pixel = H.withTests 1 $ H.property $ do
  let modify action i j img = Image.setPixel img ix value'
       where
        ix     = Ix (I i) (J j)
        value' = action value
        value  = Image.getPixel img ix
  let image =
        modify (+ 3) 0 0
          . modify (+ 1) 1 1
          . modify (+ 8) 1 0
          . modify (+ 6) 2 1
          . modify (+ 4) 0 1
          . modify (+ 2) 1 0
          $ Image.new (Size 3 2) (1 :: Word8)
  Image.size image === Size 3 2
  Image.rowMajor image === V.fromList [4, 11, 1, 5, 2, 7]

-- | lindex should produce an error if an index is out of range.
unit_lindex_error :: H.Property
unit_lindex_error = H.withTests 1 $ H.property $ do
  let size = Size 7 3
  let ix   = Ix (I 7) (J 2)
  result <- H.evalIO . try . evaluate $ Image.lindex size ix
  case result of
    Left (ErrorCallWithLocation message _) ->
      message
        === "Index is out of range: Ix (I {unI = 7}) (J {unJ = 2}). "
        <>  "Image size: Size {width = 7, height = 3}"
    Right _ -> do
      H.annotate "Expected an ErrorCall exception"
      H.failure
