{-|
Module      : AlphaStencil.Log
Description : Log primitive drawing calculations.
-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AlphaStencil.Log
  ( -- * Types
    Logger
  , Event(EventPixelLineClip, EventPixelAreaDivision,
          EventPixelProjectX, EventAddToPixel, EventStartSeg)
    -- * Functions
  , noOpLogger
  , mkLogger
  ) where

import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Mutable            (DLList, asDLList, asSRef, modifyRef,
                                          newColl, newRef, popFront, pushBack,
                                          readRef)
import           Data.Vector             (Vector)
import qualified Data.Vector             as V

import           AlphaStencil.Seg        (ClipSeg, PxDivision, Seg)
import           Image                   (Ix)

type Logger m a = Event a -> m ()

noOpLogger :: Applicative m => Logger m a
noOpLogger = const (pure ())

mkLogger :: forall m a. (PrimMonad m) => m (Logger m a, m (Vector (Event a)))
mkLogger = do
  counter <- asSRef <$> newRef (0 :: Int)
  collection <- asDLList <$> newColl :: m (DLList (PrimState m) (Event a))
  let
    fromJust :: Maybe t -> t
    fromJust (Just x) = x
    fromJust Nothing  = error "mkLogger: wrong number of log events!"

    logEvent :: Event a -> m ()
    logEvent event = do
      modifyRef counter (+ 1)
      pushBack collection event

    retrieveLog :: m (Vector (Event a))
    retrieveLog = do
      count <- readRef counter
      V.replicateM count (fromJust <$> popFront collection)

  pure (logEvent, retrieveLog)

data Event a
  = EventStartSeg !(Seg a)
  | EventPixelLineClip !(ClipSeg a)
  | EventPixelAreaDivision !Ix !(ClipSeg a) !(PxDivision a)
  | EventPixelProjectX !Ix !(ClipSeg a)
  | EventAddToPixel !Ix !a
  deriving stock (Show)
