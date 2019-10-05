{-|
Module      : Image.LoopTest
Description : Tests for the Image.Loop module.
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Image.LoopTest
  ( tests
  )
where

import           Hedgehog                       ( (===) )
import qualified Hedgehog                      as H
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Control.Monad.State.Strict     ( evalState
                                                , get
                                                , modify
                                                )

import           Image.Loop                     ( dec
                                                , inc
                                                , loop
                                                )

tests :: H.Group
tests = H.Group
  "Image.Loop"
  [ ("increment in a loop", prop_increment_loop)
  , ("decrement in a loop", prop_decrement_loop)
  ]

-- | Incrementing a value in a loop should produce the same output as a list
-- comprehension that performs the same operation.
prop_increment_loop :: H.Property
prop_increment_loop = H.property $ do
  start <- H.forAll $ Gen.int (Range.linear 0 100)
  end   <- H.forAll $ Gen.int (Range.linear 0 100)
  H.cover 10 "start >= end" (start >= end)
  H.cover 1 "end < start" (end < start)
  let action i = modify (i :)
      result = reverse $ evalState (loop start (<= end) inc action >> get) []
  result === [start .. end]

-- | Decrementing a value in a loop should produce the same output as a list
-- comprehension that performs the same operation.
prop_decrement_loop :: H.Property
prop_decrement_loop = H.property $ do
  start <- H.forAll $ Gen.int (Range.linear 0 100)
  end   <- H.forAll $ Gen.int (Range.linear 0 100)
  H.cover 10 "end <= start" (end <= start)
  H.cover 1 "start > end" (start > end)
  let action i = modify (i :)
      result = reverse $ evalState (loop start (>= end) dec action >> get) []
  result === reverse [end .. start]
