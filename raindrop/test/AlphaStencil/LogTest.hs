{-|
Module      : AlphaStencil.LogTest
Description : Tests for the AlphaStencil.Log module.
-}
{-# LANGUAGE OverloadedStrings #-}
module AlphaStencil.LogTest (tests) where

import           Hedgehog         ((===))
import qualified Hedgehog         as H
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

import           Control.Monad.ST (runST)
import           Data.Foldable    (traverse_)
import qualified Data.Vector      as V

import           AlphaStencil.Log (Logger (Record), logMessage,
                                   mkRecordingLogger, retrieveLog)

tests :: H.Group
tests = H.Group "AlphaStencil.Log"
  [ ("sending and retrieving log messages", prop_send_retrieve_log)
  ]

prop_send_retrieve_log :: H.Property
prop_send_retrieve_log = H.property $ do
  let listElemGen = Gen.int (Range.linear 0 2048)
  items <- H.forAll $ Gen.list (Range.linear 0 50) listElemGen
  let result = runST $ do
        recordingLogger <- mkRecordingLogger
        let logger = Record recordingLogger
        traverse_ (logMessage logger) items
        retrieveLog recordingLogger
  V.toList result === items
