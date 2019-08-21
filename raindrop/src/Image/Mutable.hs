{-|
Module      : Image.Mutable
Description : Mutable Storable images.

Mutable images. These are suitable for use in tight monadic loops (typically
either the @ST@ or @IO@ monad).

Example usage:

>>> import Data.Word (Word32)
>>> import Control.Monad.ST (runST)
>>> import Data.Vector.Storable (freeze)
>>> import Image.Types (inRangeI, inRangeJ)
>>> import Image.Loop (loop, inc, dec)
>>> :{
  -- let's run in the ST monad
  runST $ do
    -- first, create a mutable image of size 3 pixels wide, 2 pixels high
    let sz = Size 3 2
    mImage <- new sz (0 :: Word32)
    -- now, loop over all pixels in the image...
    loop 0 (inRangeI sz) inc $ \i -> do
      loop 0 (inRangeJ sz) inc $ \j -> do
        -- add the value of the row-major index to the default (zero) value
        let value = fromIntegral i + (fromIntegral j)*3
        unsafeModify mImage (+ value) (Ix i j)
    -- return the frozen row-major image
    freeze (rowMajor mImage)
:}
[0,1,2,3,4,5]
-}
module Image.Mutable
  ( -- * Types
    MImage
    -- * Functions
  , new
  , size
  , rowMajor
  , unsafeModify
  , lindexUnsafe
  ) where

import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as M
import           Foreign.Storable             (Storable)

import           Image.Types                  (I (I), Ix (Ix), J (J),
                                               Size (Size))

-- | Mutable image.
--
-- Mutable images store their size, and contain a vector of 'Storable' elements
-- in row-major layout. The @width x height@ of the 'size' should always match
-- the length of the internal 'rowMajor' vector.
--
-- Mutable images should be created using the 'new' function.
data MImage s a
  = MImage
    { size     :: !Size
      -- ^ Size of a mutable image.
    , rowMajor :: !(MVector s a)
      -- ^ Vector of image pixels / elements from a mutable image in row-major
      -- order.
    }

-- | Create a mutable image.
new
  :: (PrimMonad m, Storable a)
  => Size                        -- ^ Size of the image.
  -> a                           -- ^ Value for all pixels.
  -> m (MImage (PrimState m) a)  -- ^ Created image.
new sz@(Size w h) x = MImage sz <$> M.replicate vecLen x
  where
    vecLen :: Int
    vecLen = fromIntegral (w*h)

-- | Modify a single pixel of a mutable image.
--
-- No bounds checks are performed.
unsafeModify
  :: (PrimMonad m, Storable a)
  => MImage (PrimState m) a  -- ^ Mutable image.
  -> (a -> a)                -- ^ Pure modification function.
  -> Ix                      -- ^ Index.
  -> m ()                    -- ^ Action.
unsafeModify (MImage sz vec) modFn ix
  = M.unsafeModify vec modFn (lindexUnsafe sz ix)

-- | Linear index into an image.
--
-- The index is row-major.
--
-- No bounds checks are performed.
lindexUnsafe :: Size -> Ix -> Int
lindexUnsafe (Size w _) (Ix (I i) (J j)) = i' + j'*w'
  where
    i', j', w' :: Int
    i' = fromIntegral i
    j' = fromIntegral j
    w' = fromIntegral w
