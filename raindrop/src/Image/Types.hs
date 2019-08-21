{-|
Module      : Image.Types
Description : Common types for images.

Image types. These describe the 'Size' of an image and coordinates used within
the image ('I', 'J' and 'Ix').
-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Image.Types
  ( -- * Types
    Size(Size, width, height)
  , I(I, unI)
  , J(J, unJ)
  , Ix(Ix)
    -- * Functions
  , inRangeI
  , inRangeJ
  ) where

import           Data.Word        (Word32)
import           Foreign.Storable (Storable)

-- | Size of an image.
data Size
  = Size
    { width  :: {-# UNPACK #-} !Word32
    , height :: {-# UNPACK #-} !Word32
    }
  deriving stock (Eq, Show)

-- | Horizontal coordinate.
--
-- The coordinate is zero based.
newtype I = I { unI :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Storable, Real, Enum, Integral)

-- | Vertical coordinate.
--
-- The coordinate is zero based.
newtype J = J { unJ :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Storable, Real, Enum, Integral)

-- | Coordinate in an 'Image'.
data Ix = Ix {-# UNPACK #-} !I {-# UNPACK #-} !J
  deriving stock (Eq, Show)

-- | Check if an 'I' coordinate is in range for a given 'Size'.
--
-- Example:
--
-- >>> inRangeI (Size 4 3) (I 4)
-- False
-- >>> inRangeI (Size 4 3) (I 3)
-- True
inRangeI :: Size -> I -> Bool
inRangeI (Size w _) (I i) = i >= 0 && i < fromIntegral w

-- | Check if a 'J' coordinate is in range for a given 'Size'.
--
-- Example:
--
-- >>> inRangeJ (Size 4 3) (J 0)
-- True
-- >>> inRangeJ (Size 4 3) (J 3)
-- False
inRangeJ :: Size -> J -> Bool
inRangeJ (Size _ h) (J j) = j >= 0 && j < (fromIntegral h)
