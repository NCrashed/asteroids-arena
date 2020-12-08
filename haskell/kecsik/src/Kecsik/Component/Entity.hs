module Kecsik.Component.Entity(
  -- * Entity allocation
    EntityCounter(..)
  -- * Components registry
  , EntityComponents(..)
  , HasEntityComponents
  , destroyAll
  -- ** Low level control
  , registryComponent
  , unregistryComponent
  , getEntityComponents
  ) where

import Data.Default
import Control.Monad.Primitive
import Data.Foldable (for_, traverse_)
import Data.Maybe
import Data.Mutable
import Data.Mutable.Instances
import Data.Typeable
import GHC.Generics
import Kecsik.Core
import Kecsik.Storage.Global
import Kecsik.Storage.HashTable

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C

newtype EntityCounter = EntityCounter { unEntityCounter :: Entity }
  deriving (Generic, Typeable, Eq, Show, Read, Num)

instance Mutable s EntityCounter where
  type Ref s EntityCounter = GRef s EntityCounter

instance Component s EntityCounter where
  type Storage s EntityCounter = Global s EntityCounter

instance Default EntityCounter where
  def = EntityCounter 0
  {-# INLINE def #-}

-- | Stores which components the entity has
newtype EntityComponents s = EntityComponents { unEntityComponents :: C.HashTable s ComponentId () }
  deriving (Generic, Typeable)

instance Mutable s (EntityComponents s) where
  type Ref s (EntityComponents s) = ImmutableRef s (EntityComponents s)

instance Typeable s => Component s (EntityComponents s) where
  type Storage s (EntityComponents s) = HashTable s (EntityComponents s)

type HasEntityComponents w m = HasStore w m (EntityComponents (PrimState m))

registryComponent :: forall w m . HasEntityComponents w m => Entity -> ComponentId -> SystemT w m ()
registryComponent e i = do
  s :: Storage (PrimState m) (EntityComponents (PrimState m)) <- getStore
  mr <- storeGet s e
  case mr of
    Nothing -> do
      c <- stToPrim $ do
        h <- H.newSized 1
        H.insert h i ()
        pure $ EntityComponents h
      storeSet s e c
    Just (EntityComponents h) -> stToPrim $ H.insert h i ()
{-# INLINABLE registryComponent #-}

unregistryComponent :: forall w m . HasEntityComponents w m => Entity -> ComponentId -> SystemT w m ()
unregistryComponent e i = do
  s :: Storage (PrimState m) (EntityComponents (PrimState m)) <- getStore
  mr <- storeGet s e
  for_ mr $ \(EntityComponents r) -> stToPrim $ H.delete r i
{-# INLINABLE unregistryComponent #-}

getEntityComponents :: forall w m . HasEntityComponents w m => Entity -> SystemT w m [ComponentId]
getEntityComponents e = do
  s :: Storage (PrimState m) (EntityComponents (PrimState m)) <- getStore
  mr <- storeGet s e
  maybe (pure []) (stToPrim . fmap (fmap fst) . H.toList . unEntityComponents) mr
{-# INLINABLE getEntityComponents #-}

-- | Remove all components from given entity.
destroyAll :: forall w m . (HasEntityComponents w m, WorldComponents w m) => Entity -> SystemT w m ()
destroyAll e = do
  is <- getEntityComponents e
  traverse_ (destroyById e) is
{-# INLINABLE destroyAll #-}
