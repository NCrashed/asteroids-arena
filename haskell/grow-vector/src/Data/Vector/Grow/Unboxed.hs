{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Grow.Unboxed(
    GrowVector(..)
  , IOGrowVector
  -- * Quering info about vector
  , length
  , null
  , capacity
  -- * Creation
  , new
  , newSized
  ) where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable (MVector, Unbox)
import GHC.Generics
import Prelude hiding (length, null)

import qualified Data.Vector.Unboxed.Mutable as M

-- | Grow vector that is wrap around mutable vector. We allocate partially filled
-- vector and grow it when there is no more space for new element.
data GrowVector s a = GrowVector {
  growVector         :: !(MVector s a)
, growVectorLength   :: !(MutVar s Int)
} deriving (Generic)

type IOGrowVector a = GrowVector RealWorld a

-- | Return current capacity of the vector (amount of elements that it can fit without realloc)
capacity :: Unbox a => GrowVector s a -> Int
capacity = M.length . growVector
{-# INLINE capacity #-}

-- | Return current amount of elements in the vector
length :: PrimMonad m => GrowVector (PrimState m) a -> m Int
length = readMutVar . growVectorLength
{-# INLINE length #-}

-- | Return 'True' if there is no elements inside the vector
null :: PrimMonad m => GrowVector (PrimState m) a -> m Bool
null = fmap (== 0) . length
{-# INLINE null #-}

-- | Allocation of new growable vector with given capacity.
new :: (Unbox a, PrimMonad m) => Int -> m (GrowVector (PrimState m) a)
new = newSized 0
{-# INLINE new #-}

-- | Allocation of new growable vector with given filled size and capacity.
-- Elements is not initialized.
newSized :: (Unbox a, PrimMonad m) => Int -> Int -> m (GrowVector (PrimState m) a)
newSized n cap = GrowVector <$> M.new cap <*> newMutVar n
{-# INLINABLE newSized #-}
