module Kecsik.System(
    newEntity
  , get
  , set
  , exists
  , modify
  , update
  , destroy
  , destroyAll
  ) where

import Control.Monad
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Mutable
import Data.Proxy
import Kecsik.Component.Entity
import Kecsik.Core
import Kecsik.Storage.Global

newEntity :: Store w m EntityCounter => SystemT w m Entity
newEntity = fmap (fromMaybe global) $ update global $ \(EntityCounter e) -> (EntityCounter $ e+1, e)
{-# INLINE newEntity #-}

get :: forall w m a . Store w m a => Entity -> SystemT w m (Maybe a)
get e = do
  sa :: Storage (PrimState m) a <- getStore
  storeGet sa e
{-# INLINABLE get #-}

set :: forall w m a . (HasEntityComponents w m, Store w m a) => Entity -> a -> SystemT w m ()
set e a = do
  sa :: Storage (PrimState m) a <- getStore
  storeSet sa e a
  registryComponent e $ componentId (Proxy @(PrimState m)) (Proxy @a)
{-# INLINABLE set #-}

exists :: forall w m a . Store w m a => Entity -> Proxy a -> SystemT w m Bool
exists e _ = do
  sa :: Storage (PrimState m) a <- getStore
  storeExists sa e
{-# INLINABLE exists #-}

modify :: forall w m a . Store w m a => Entity -> (a -> a) -> SystemT w m ()
modify ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse_ mref $ \r -> modifyRef' r f
{-# INLINABLE modify #-}

update :: forall w m a b . Store w m a => Entity -> (a -> (a, b)) -> SystemT w m (Maybe b)
update ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse mref $ \r -> updateRef' r f
{-# INLINABLE update #-}

-- | Destroy component for given entity. Return 'True' if there was a component.
destroy :: forall w m a . (HasEntityComponents w m, Store w m a) => Entity -> Proxy a -> SystemT w m Bool
destroy e p = do
  sx :: Storage (PrimState m) a <- getStore
  r <- storeExists sx e
  when r $ do
    storeDestroy sx e
    unregistryComponent e $ componentId (Proxy @(PrimState m)) (Proxy @a)
  pure r
{-# INLINABLE destroy #-}
