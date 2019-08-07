{-|
Module      : Image.Loop
Description : Monadic loop.
-}
{-# LANGUAGE BangPatterns #-}
module Image.Loop
  ( loop
  ) where

-- | Basic loop.
loop
    :: (Monad m)
    => a            -- ^ Starting value of the loop.
    -> (a -> Bool)  -- ^ Terminate when this function returns False.
    -> (a -> a)     -- ^ State transition / step function.
    -> (a -> m ())  -- ^ Body of the loop.
    -> m ()
loop start while step body = go start
  where
    go !i | while i   = body i >> go (step i)
          | otherwise = return ()
{-# INLINE loop #-}
