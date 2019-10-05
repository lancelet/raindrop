{-|
Module      : Image
Description : Storable images.

Immutable images. Images are built on top of the "Data.Vector.Storable" module.
They are 2D arrays that are indexed in row-major layout. Two different types of
images are provided:

  * The immutable 'Image' type, found in this module, and
  * The mutable 'Image.Mutable.MImage' type, which can be mutated in a
    'Control.Monad.Primitive.PrimMonad'.

NOTE: It would probably be better to use @massiv@ for images in general, but
      @massiv@ does not seem to allow operations without bounds-checking.
-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Image
  ( -- * Types (re-exports)
    Size(Size, width, height)
  , I(I, unI)
  , J(J, unJ)
  , Ix(Ix)
  , Image
    ( size
    , rowMajor
    -- * Functions
    -- ** Creation and Freezing
    )
  , new
  , unsafeFreeze
    -- ** Checking ranges
  , inRangeI
  , inRangeJ
    -- ** Getting and setting pixels
  , setPixel
  , getPixel
  , lindex
  )
where

import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )
import           Data.Vector.Storable           ( Vector )
import qualified Data.Vector.Storable          as V
import           Foreign.Storable               ( Storable )

import qualified Image.Mutable                 as M
import           Image.Types                    ( I(I, unI)
                                                , Ix(Ix)
                                                , J(J, unJ)
                                                , Size(Size, height, width)
                                                , inRangeI
                                                , inRangeJ
                                                )

-- | Image, with pixels of type 'a'.
data Image a
  = Image
    { size     :: !Size
    -- ^ Size of an image.
    , rowMajor :: !(Vector a)
    -- ^ Vector of image pixels / elements in row-major order.
    }
  deriving stock (Eq, Show)

-- | Set a pixel in an immutable image.
--
-- This function is ludicrously inefficient, since it copies the entire image
-- each time it is run. However, this can still be useful for documentation and
-- testing purposes.
--
-- Example:
--
-- >>> image = new (Size 3 2) (0 :: Int)
-- >>> V.toList $ rowMajor $ setPixel image (Ix (I 1) (J 1)) 3
-- [0,0,0,0,3,0]
setPixel :: Storable a => Image a -> Ix -> a -> Image a
setPixel (Image sz vec) ix value = Image sz (vec V.// [(lindex sz ix, value)])

-- | Get a pixel from an immutable image.
--
-- Example:
--
-- >>> ix = Ix (I 1) (J 1)
-- >>> image0 = new (Size 3 2) (0 :: Int)
-- >>> image1 = setPixel image0 ix 42
-- >>> getPixel image0 ix
-- 0
-- >>> getPixel image1 ix
-- 42
getPixel :: Storable a => Image a -> Ix -> a
getPixel (Image sz vec) ix = vec V.! lindex sz ix

-- | Create a new image.
new
  :: Storable a
  => Size     -- ^ Size of the image.
  -> a        -- ^ Element to fill all pixels of the image.
  -> Image a  -- ^ Produced image.
new sz@(Size w h) x = Image sz (V.replicate vecLen x)
 where
  vecLen :: Int
  vecLen = fromIntegral (w * h)

-- | Freeze a mutable image to an immutable one without copying.
unsafeFreeze
  :: (PrimMonad m, Storable a)
  => M.MImage (PrimState m) a  -- ^ Image to freeze.
  -> m (Image a)               -- ^ Frozen image.
unsafeFreeze mImage =
  Image (M.size mImage) <$> V.unsafeFreeze (M.rowMajor mImage)

-- | Create a row-major linear index.
lindex :: Size -> Ix -> Int
lindex sz ix@(Ix i j)
  | inRange
  = M.lindexUnsafe sz ix
  | otherwise
  = error $ "Index is out of range: " <> show ix <> ". Image size: " <> show sz
  where inRange = inRangeI sz i && inRangeJ sz j
