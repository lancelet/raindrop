{-|
Module      : AlphaStencil.Log
Description : Logging for primitive drawing operations.

Primitive drawing operations in the alpha stencil code can be logged. This is
useful for both debugging and visualisation purposes, because the alpha stencil
code is purely procedural in nature.

Logging is disabled by passing the `NoOp` constructor to the `logMessage`
function. This is the typical use-case and is intended for zero overhead.

To record logging events, the 'Record' logger value is created inside a
'PrimMonad' instance using the 'mkRecordingLogger' function. Messages are then
sent to it during the course of 'PrimMonad' execution. Finally, after operations
have been completed, messages can be retrieved.

An example in the 'Control.Monad.ST.ST' monad:

>>> import Control.Monad.ST (runST)
>>> :{
  runST $ do
    recordingLogger <- mkRecordingLogger
    let logger = Record recordingLogger
    logMessage logger "Hello"
    logMessage logger "World"
    retrieveLog recordingLogger
:}
["Hello","World"]
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencil.Log
  ( -- * Types
    Event
    ( ENewImage
    , EStartSeg
    , EClipSegToColumn
    , EPxDivision
    , EPxAdd
    , EProjArea
    )
  , Logger(NoOp, Record)
  , RecordingLogger
    -- * Functions
  , logMessage
  , mkRecordingLogger
  , retrieveLog
  )
where

import           Control.Monad.Primitive        ( PrimMonad )
import           Data.Maybe                     ( fromMaybe )
import           Data.Mutable                   ( asDLList
                                                , asSRef
                                                , modifyRef
                                                , newColl
                                                , newRef
                                                , popFront
                                                , pushBack
                                                , readRef
                                                , writeRef
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           AlphaStencil.Seg               ( ClipSeg
                                                , PxDivision
                                                , Seg
                                                )
import           Image                          ( I
                                                , J
                                                , Ix
                                                , Size
                                                )

-- | Events produced during primitive drawing operations.
data Event a
  = ENewImage !Size
    -- ^ Image has been created. This event is produced at the start of
    --   rendering and indicates the size of the alpha stencil image that is
    --   about to be rendered.
  | EStartSeg !(Seg a)
    -- ^ Processing a segment has started.
  | EClipSegToColumn !I !(Seg a) !(ClipSeg a)
    -- ^ A segment has been clipped to a column of pixels in the image.
  | EPxDivision !Ix !(PxDivision a)
    -- ^ Area sub-divisions of a pixel have been computed.
  | EPxAdd !Ix !a
    -- ^ A value has been added to a pixel.
  | EProjArea !I !J !a
    -- ^ A projected area has been computed for a column of pixels below the
    --   line segment.
  deriving stock (Eq, Show)

-- | Log a message.
logMessage
  :: Applicative m
  => Logger msg m  -- ^ Logger to use.
  -> msg           -- ^ Message to log.
  -> m ()
logMessage NoOp        _       = pure ()
logMessage (Record rl) message = rlLogger rl message

-- | Type of logger.
data Logger msg m
  = NoOp
    -- ^ A no-op logger which does nothing.
  | Record !(RecordingLogger msg m)
    -- ^ A logger which records events.

-- | A recording logger.
data RecordingLogger msg m
  = RecordingLogger
    { rlLogger      :: msg -> m ()
    , rlRetrieveLog :: m (Vector msg)
    }

-- | Create a recording logger.
mkRecordingLogger :: forall  m msg . PrimMonad m => m (RecordingLogger msg m)
mkRecordingLogger = do
  countRef <- asSRef <$> newRef (0 :: Int)
  coll     <- asDLList <$> newColl

  let fromJust :: Maybe t -> t
      fromJust = fromMaybe (error "Logger: mismatch in number of events!")

      logAction :: msg -> m ()
      logAction msg = modifyRef countRef (+ 1) >> pushBack coll msg

      retrLog :: m (Vector msg)
      retrLog = do
        count <- readRef countRef
        writeRef countRef 0
        V.replicateM count (fromJust <$> popFront coll)

  pure $ RecordingLogger logAction retrLog

-- | Retrieve events from a recording logger.
retrieveLog :: RecordingLogger msg m -> m (Vector msg)
retrieveLog = rlRetrieveLog
