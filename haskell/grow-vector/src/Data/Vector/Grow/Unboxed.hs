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
  -- * Quering subvectors
  , slice
  -- * Converting to immutable
  , thaw
  , freeze
  -- * Capacity maninuplation
  , ensure
  , ensureAppend
  -- * Accessing individual elements
  , read
  , write
  , unsafeRead
  , unsafeWrite
  -- * Appending to vector
  , pushBack
  , unsafePushBack
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Unboxed.Mutable (MVector, Unbox)
import GHC.Generics
import Prelude hiding (length, null, read)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

-- | Grow vector that is wrap around mutable vector. We allocate partially filled
-- vector and grow it when there is no more space for new element.
data GrowVector s a = GrowVector {
  growVector         :: !(MutVar s (MVector s a))
, growVectorLength   :: !(MutVar s Int)
} deriving (Generic)

type IOGrowVector a = GrowVector RealWorld a

-- | Return current capacity of the vector (amount of elements that it can fit without realloc)
capacity :: (Unbox a, PrimMonad m) => GrowVector (PrimState m) a -> m Int
capacity v = do
  mv <- readMutVar . growVector $! v
  pure $ M.length mv
{-# INLINE capacity #-}

-- | Return current amount of elements in the vector
length :: (Unbox a, PrimMonad m) => GrowVector (PrimState m) a -> m Int
length = readMutVar . growVectorLength
{-# INLINE length #-}

-- | Return 'True' if there is no elements inside the vector
null :: (Unbox a, PrimMonad m) => GrowVector (PrimState m) a -> m Bool
null = fmap (== 0) . length
{-# INLINE null #-}

-- | Allocation of new growable vector with given capacity.
new :: (Unbox a, PrimMonad m) => Int -> m (GrowVector (PrimState m) a)
new = newSized 0
{-# INLINE new #-}

-- | Allocation of new growable vector with given filled size and capacity.
-- Elements is not initialized. Capacity must be greater than filled size.
newSized :: (Unbox a, PrimMonad m) => Int -> Int -> m (GrowVector (PrimState m) a)
newSized n cap = GrowVector <$> (newMutVar =<< M.new cap) <*> newMutVar n
{-# INLINABLE newSized #-}

-- | Yield a part of mutable vector without copying it. The vector must contain at least i+n elements.
slice :: (Unbox a, PrimMonad m)
  => Int -- ^ i starting index
  -> Int -- ^ n number of elements
  -> GrowVector (PrimState m) a
  -> m (GrowVector (PrimState m) a)
slice i n v = do
  newSize <- newMutVar n
  mv <- readMutVar . growVector $! v
  newVec <- newMutVar $! M.slice i n mv
  pure $! GrowVector newVec newSize
{-# INLINABLE slice #-}

-- | Convert immutable vector to grow mutable version. Doesn't allocate additonal memory for appending,
-- use 'ensure' to add capacity to the vector.
thaw :: (Unbox a, PrimMonad m)
  => U.Vector a
  -> m (GrowVector (PrimState m) a)
thaw u = do
  mv <- newMutVar =<< U.thaw u
  lv <- newMutVar (U.length u)
  pure $ GrowVector mv lv
{-# INLINABLE thaw #-}

-- | Freezing growable vector. It will contain only actual elements of the vector not including capacity
-- space, but you should call 'U.force' on resulting vector to not hold the allocated capacity of original
-- vector in memory.
freeze :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> m (U.Vector a)
freeze v = do
  n <- length v
  mv <- readMutVar . growVector $! v
  U.freeze . M.take n $! mv
{-# INLINABLE freeze #-}

-- | Ensure that grow vector has at least given capacity possibly with reallocation.
ensure :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int
  -> m ()
ensure v n = do
  c <- capacity v
  unless (c >= n) $ do
    mv <- readMutVar . growVector $! v
    writeMutVar (growVector v) =<< M.grow mv (n - c)
{-# INLINABLE ensure #-}

-- | Ensure that grow vector has enough space for additonal n elements.
-- We grow vector by 1.5 factor or by
ensureAppend :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Additional n elements
  -> m ()
ensureAppend v i = do
  n <- readMutVar . growVectorLength $! v
  mv <- readMutVar . growVector $! v
  let c = M.length mv
  unless (c >= n + i) $ do
    let growFactor = 1.5
        newCap = ceiling $ max (growFactor * fromIntegral c) (fromIntegral c + growFactor * fromIntegral (n + i - c))
    writeMutVar (growVector v) =<< M.grow mv (newCap - c)
{-# INLINABLE ensureAppend #-}

-- | Read element from vector at given index.
read :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> m a
read v i = do
  n <- readMutVar . growVectorLength $! v
  when (i < 0 || i >= n) $ error $ "GrowVector.read: index " <> show i <> " is out bounds " <> show n
  mv <- readMutVar . growVector $! v
  M.unsafeRead mv i
{-# INLINABLE read #-}

-- | Read element from vector at given index.
unsafeRead :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> m a
unsafeRead v i = do
  mv <- readMutVar . growVector $! v
  M.unsafeRead mv i
{-# INLINABLE unsafeRead #-}

-- | Write down element in the vector at given index.
write :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> a
  -> m ()
write v i a = do
  n <- readMutVar . growVectorLength $! v
  when (i < 0 || i >= n) $ error $ "GrowVector.write: index " <> show i <> " is out bounds " <> show n
  mv <- readMutVar . growVector $! v
  M.unsafeWrite mv i a
{-# INLINABLE write #-}

-- | Write down element in the vector at given index.
unsafeWrite :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> Int -- ^ Index of element. Must be in [0 .. length) range
  -> a
  -> m ()
unsafeWrite v i a = do
  mv <- readMutVar . growVector $! v
  M.unsafeWrite mv i a
{-# INLINABLE unsafeWrite #-}

-- | O(1) amortized appending to vector
pushBack :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> a
  -> m ()
pushBack v a = do
  ensureAppend v 1
  unsafePushBack v a
{-# INLINABLE pushBack #-}

-- | O(1) amortized appending to vector. Doesn't reallocate vector, so
-- there must by capacity - length >= 1.
unsafePushBack :: (Unbox a, PrimMonad m)
  => GrowVector (PrimState m) a
  -> a
  -> m ()
unsafePushBack v a = do
  n <- readMutVar . growVectorLength $! v
  mv <- readMutVar . growVector $! v
  M.write mv n a
  writeMutVar (growVectorLength v) (n+1)
{-# INLINABLE unsafePushBack #-}
