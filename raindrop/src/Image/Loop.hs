{-|
Module      : Image.Loop
Description : Monadic loop.

Monadic loop. This module contains a monadic-style 'loop' function, which
simulates the kind of procedural loop you'd use in C.
-}
{-# LANGUAGE BangPatterns #-}
module Image.Loop
  ( -- * Functions
    loop
  , inc
  , dec
  )
where

-- | Basic loop.
--
-- This simulates monadic-style @for@ / @while@ loops like you'd find in C.
--
-- Examples:
--
-- A loop can count:
--
-- >>> loop (0 :: Int) (< 3) inc (putStrLn . show) >> putStrLn "Done"
-- 0
-- 1
-- 2
-- Done
--
-- A loop can be degenerate (ie. a no-op):
--
-- >>> loop (0 :: Int) (const False) inc (putStrLn . show) >> putStrLn "Done"
-- Done
--
loop
  :: (Monad m)
  => a            -- ^ Starting value of the loop.
  -> (a -> Bool)  -- ^ Terminate when this function returns False.
  -> (a -> a)     -- ^ State transition / step function.
  -> (a -> m ())  -- ^ Body of the loop.
  -> m ()         -- ^ Loop action.
loop start while step body = go start
 where
  go !i | while i   = body i >> go (step i)
        | otherwise = return ()
{-# INLINE loop #-}

-- | Increment a value.
--
-- >>> inc (5 :: Int)
-- 6
inc :: Num a => a -> a
inc x = x + 1

-- | Decrement a value.
--
-- >>> dec (5 :: Int)
-- 4
dec :: Num a => a -> a
dec x = x - 1
