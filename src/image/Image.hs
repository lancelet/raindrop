{-|
Module      : Image
Description : Storable images for Eggshell.

Eggshell requires alpha channel images. 'MImage's are mutable images used
inside tight imperative loops, while 'Image's are immutable images returned
generally as the result of high-level processing.

NOTE: It would probably be better to use @massiv@ for this, but massiv does not
      have unsafe (no bounds-checking) operations. We need these operations to
      be extremely fast, so no bounds-checking is highly desirable.
-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Image
  ( -- * Types
    Size(Size, width, height)
  , I(I, unI)
  , J(J, unJ)
  , DirLimit(Up, Down, UpTo, DownTo)
  , Ix(Ix)
  , Image
  , MImage
    -- * Functions
    -- ** Creation and Freezing
  , newMutable
  , unsafeFreeze
    -- ** Conversion to Vector
  , rowMajor
  , getSize
  , getMSize
    -- ** Looping and Mutation
  , loopI
  , loopJ
  , unsafeModify
  ) where

import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as V
import           Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as M
import           Foreign.Storable             (Storable)

import           Image.Loop                (loop)

-- | Size of an image.
data Size
  = Size
    { width  :: {-# UNPACK #-} !Int
    , height :: {-# UNPACK #-} !Int
    }
  deriving stock (Eq, Show)

-- | Horizontal coordinate.
newtype I = I { unI :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Storable, Real, Enum, Integral)

-- | Vertical coordinate.
newtype J = J { unJ :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Storable, Real, Enum, Integral)

-- | Direction and limit of for loops over coordinates.
--
-- Used with 'loopI' and 'loopJ' functions.
data DirLimit a
  = Up        -- ^ Count up through an index until the end.
  | Down      -- ^ Count down through an index until the start.
  | UpTo a    -- ^ Count up to this value inclusive.
  | DownTo a  -- ^ Count down to this value inclusive.

-- | Loop monadically over an 'I' coordinate (horizontally in the image).
--
-- The 'Size' parameter is not referenced in 'UpTo' or 'DownTo' cases.
loopI
  :: Monad m
  => Size         -- ^ Overall size to use for bounds.
  -> I            -- ^ Starting index value ('I' coordinate).
  -> DirLimit I   -- ^ Direction and optional limiting value.
  -> (I -> m ())  -- ^ Monadic action to perform for each 'I' value.
  -> m ()         -- ^ Overall action.
loopI (Size w _) start lim body = loop start while step body
  where
    (while, step) = case lim of
      Up          -> ((< I w),   (+ 1))
      Down        -> ((>= 0),    \i -> i - 1)
      UpTo iLim   -> ((<= iLim), (+ 1))
      DownTo iLim -> ((>= iLim), \i -> i - 1)

-- | Loop monadically over a 'J' coordinate (vertically in the image).
--
-- The 'Size' parameter is not references in 'UpTo' or 'DownTo' cases.
loopJ
  :: Monad m
  => Size         -- ^ Overall size used for bounds.
  -> J            -- ^ Starting index value ('J' coordinate).
  -> DirLimit J   -- ^ Direction and optional limiting value.
  -> (J -> m ())  -- ^ Monadic action to perform for each 'J' value.
  -> m ()         -- ^ Overall action.
loopJ (Size _ h) start lim body = loop start while step body
  where
    (while, step) = case lim of
      Up          -> ((< J h),   (+ 1))
      Down        -> ((>= 0),    \j -> j - 1)
      UpTo jLim   -> ((<= jLim), (+ 1))
      DownTo jLim -> ((>= jLim), \j -> j - 1)

-- | Pair of coordinates.
data Ix = Ix {-# UNPACK #-} !I {-# UNPACK #-} !J

-- | Image.
data Image a = Image Size (Vector a)

-- | Mutable image.
data MImage s a = MImage Size (MVector s a)

-- | Get the size of an image.
getSize :: Image a -> Size
getSize (Image sz _) = sz

-- | Get the size of a mutable image.
getMSize :: MImage s a -> Size
getMSize (MImage sz _) = sz

-- | Return the image in row-major order.
rowMajor :: Storable a => Image a -> Vector a
rowMajor (Image _ vec) = vec

-- | Create a mutable image.
newMutable
  :: (PrimMonad m, Storable a)
  => Size                        -- ^ Size of the image.
  -> a                           -- ^ Value for all pixels.
  -> m (MImage (PrimState m) a)  -- ^ Created image.
newMutable sz@(Size w h) x = MImage sz <$> M.replicate (w*h) x

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

-- | Freeze a mutable image to an immutable one without copying.
unsafeFreeze
  :: (PrimMonad m, Storable a)
  => MImage (PrimState m) a  -- ^ Image to freeze.
  -> m (Image a)             -- ^ Frozen image.
unsafeFreeze (MImage sz vec) = Image sz <$> V.unsafeFreeze vec

-- | Linear index into an image.
--
-- No bounds checks are performed.
lindexUnsafe :: Size -> Ix -> Int
lindexUnsafe (Size w _) (Ix (I i) (J j)) = i + j*w
