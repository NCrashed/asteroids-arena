module Kecsik.Storage.Entity(
    ImmutableEnt
  , EntityStorage(..)
  ) where

import Control.Monad.Primitive
import Data.Default
import Data.Mutable
import Data.Mutable.Instances
import Data.Proxy
import Data.Typeable
import GHC.Generics
import Kecsik.Core

import qualified Data.Vector.Unboxed as U

-- | Type Synonym for 'EntityStorage' iteration, use it to specify type for
-- entity element in 'cmap'.
type ImmutableEnt m = Immutable (PrimState m) Entity

-- | Special pseudo storage that allows you to get entity id of components that
-- you iterate at the moment. Don't use it as first component in tuples as it
-- will terminate iteration immediately (either this will require full scan of all entities).
data EntityStorage s = EntityStorage
  deriving (Generic)

instance Mutable s (EntityStorage s) where
  type Ref s (EntityStorage s) = ImmutableRef s (EntityStorage s)

type instance Elem (EntityStorage s) = Immutable s Entity

instance Typeable s => Component s (Immutable s Entity) where
  type Storage s (Immutable s Entity) = EntityStorage s

instance (PrimMonad m, PrimState m ~ s) => IsStorage m (EntityStorage s) where
  storeInit _ = pure EntityStorage
  {-# INLINE storeInit #-}

  storeRef _ e = pure . Just . ImmutableRef . Immutable $! e
  {-# INLINE storeRef #-}

  storeAlloc _ e _ = pure . ImmutableRef . Immutable $! e
  {-# INLINE storeAlloc #-}

  storeDestroy _ _ = pure ()
  {-# INLINE storeDestroy #-}

  storeMembers _ = pure $ U.empty -- TODO: split IsStorage to two classes
  {-# INLINE storeMembers #-}

  storeSet _ _ _ = pure ()
  {-# INLINE storeSet #-}

  storeExists _ _ = pure True
  {-# INLINE storeExists #-}

instance (PrimMonad m, s ~ PrimState m, Typeable s, Mutable s w) => HasStore w m (Immutable s Entity) where
  getStore = pure EntityStorage
  {-# INLINE getStore #-}
