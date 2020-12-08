module Kecsik.System(
    Has
  , allocEntity
  , newEntity
  , get
  , set
  , exists
  , modify
  , modify1
  , modifyM
  , modify1M
  , modifyMay
  , modify1May
  , modifyMayM
  , modify1MayM
  , update
  , updateM
  , destroy
  , destroy_
  , destroyAll
  , cmap
  , ctraverse_
  , cmapM_
  , cfold
  , withC
  , withCM
  , withCM_
  ) where

import Control.Monad
import Data.Foldable (traverse_, for_)
import Data.Maybe
import Data.Mutable
import Data.Proxy
import Data.Typeable
import Kecsik.Component.Entity
import Kecsik.Core
import Kecsik.Storage.Global

import qualified Data.Vector.Unboxed as U

-- | Marks that we have the component and metadata about it
type Has w m c = (HasStore w m c, HasEntityComponents w m, Typeable (PrimState m))

-- | Allocate id for new entity without any components
allocEntity :: Has w m EntityCounter => SystemT w m Entity
allocEntity = fmap (fromMaybe global) $ update global $ \(EntityCounter e) -> (EntityCounter $ e+1, e)
{-# INLINE allocEntity #-}

-- | Create new entity with given components (use tuples to set multiple components)
newEntity :: (Has w m EntityCounter, Has w m a) => a -> SystemT w m Entity
newEntity a = do
  e <- allocEntity
  set e a
  pure e
{-# INLINE newEntity #-}

get :: forall w m a . Has w m a => Entity -> SystemT w m (Maybe a)
get e = do
  sa :: Storage (PrimState m) a <- getStore
  storeGet sa e
{-# INLINABLE get #-}

set :: forall w m a . Has w m a => Entity -> a -> SystemT w m ()
set e a = do
  sa :: Storage (PrimState m) a <- getStore
  storeSet sa e a
  registryComponent e $ componentId (Proxy @(PrimState m)) (Proxy @a)
{-# INLINABLE set #-}

exists :: forall w m a . Has w m a => Entity -> Proxy a -> SystemT w m Bool
exists e _ = do
  sa :: Storage (PrimState m) a <- getStore
  storeExists sa e
{-# INLINABLE exists #-}

modify1 :: forall w m a . Has w m a => Entity -> (a -> a) -> SystemT w m ()
modify1 ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse_ mref $ \r -> modifyRef' r f
{-# INLINABLE modify1 #-}

modify :: forall w m a b . (Has w m a, Has w m b) => Entity -> (a -> b) -> SystemT w m ()
modify ety f = do
  sx :: Storage (PrimState m) a <- getStore
  sy :: Storage (PrimState m) b <- getStore
  ma <- storeGet sx ety
  traverse_ (storeSet sy ety . f) ma
{-# INLINABLE modify #-}

modify1M :: forall w m a . Has w m a => Entity -> (a -> SystemT w m a) -> SystemT w m ()
modify1M ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse_ mref $ \r -> do
    a <- f =<< freezeRef r
    modifyRef' r $ const a
{-# INLINABLE modify1M #-}

modifyM :: forall w m a b . (Has w m a, Has w m b) => Entity -> (a -> SystemT w m b) -> SystemT w m ()
modifyM ety f = do
  sx :: Storage (PrimState m) a <- getStore
  sy :: Storage (PrimState m) b <- getStore
  ma <- storeGet sx ety
  traverse_ (storeSet sy ety <=< f) ma
{-# INLINABLE modifyM #-}

modify1May :: forall w m a . Has w m a => Entity -> (a -> Maybe a) -> SystemT w m ()
modify1May ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  for_ mref $ \r -> do
    a <- freezeRef r
    traverse_ (modifyRef' r . const) $ f a
{-# INLINABLE modify1May #-}

modifyMay :: forall w m a b . (Has w m a, Has w m b) => Entity -> (a -> Maybe b) -> SystemT w m ()
modifyMay ety f = do
  sx :: Storage (PrimState m) a <- getStore
  sy :: Storage (PrimState m) b <- getStore
  ma <- storeGet sx ety
  traverse_ (traverse_ (storeSet sy ety) . f) ma
{-# INLINABLE modifyMay #-}

modify1MayM :: forall w m a . Has w m a => Entity -> (a -> SystemT w m (Maybe a)) -> SystemT w m ()
modify1MayM ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  for_ mref $ \r -> do
    ma <- f =<< freezeRef r
    traverse_ (modifyRef' r . const) ma
{-# INLINABLE modify1MayM #-}

modifyMayM :: forall w m a b . (Has w m a, Has w m b) => Entity -> (a -> SystemT w m (Maybe b)) -> SystemT w m ()
modifyMayM ety f = do
  sx :: Storage (PrimState m) a <- getStore
  sy :: Storage (PrimState m) b <- getStore
  ma <- storeGet sx ety
  traverse_ (traverse_ (storeSet sy ety) <=< f) ma
{-# INLINABLE modifyMayM #-}

update :: forall w m a b . Has w m a => Entity -> (a -> (a, b)) -> SystemT w m (Maybe b)
update ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse mref $ \r -> updateRef' r f
{-# INLINABLE update #-}

updateM :: forall w m a b . Has w m a => Entity -> (a -> SystemT w m (a, b)) -> SystemT w m (Maybe b)
updateM ety f = do
  sx :: Storage (PrimState m) a <- getStore
  mref <- storeRef sx ety
  flip traverse mref $ \r -> do
    (a, b) <- f =<< freezeRef r
    modifyRef' r $ const a
    pure b
{-# INLINABLE updateM #-}

-- | Destroy component for given entity. Return 'True' if there was a component.
destroy :: forall w m a . Has w m a => Entity -> Proxy a -> SystemT w m Bool
destroy e p = do
  sx :: Storage (PrimState m) a <- getStore
  r <- storeExists sx e
  when r $ do
    storeDestroy sx e
    unregistryComponent e $ componentId (Proxy @(PrimState m)) (Proxy @a)
  pure r
{-# INLINABLE destroy #-}

-- | Same as 'destroy' but returns '()'
destroy_ :: forall w m a . Has w m a => Entity -> Proxy a -> SystemT w m ()
destroy_ e p = void $ destroy e p
{-# INLINE destroy_ #-}

-- | Iterate throw all instances of the given component and apply action
ctraverse_ :: forall w m a . Has w m a => (a -> SystemT w m ()) -> SystemT w m ()
ctraverse_ f = do
  sx :: Storage (PrimState m) a <- getStore
  is <- storeMembers sx
  flip U.mapM_ is $ \i -> do
    ma <- storeGet sx i
    traverse_ f ma
{-# INLINEABLE ctraverse_ #-}

-- | Iterate over all instances of component 'a' and produce component 'b' (possible the same of 'a')
-- that is applied to the world.
cmap :: forall w m a b . (Has w m a, Has w m b) => (a -> b) -> SystemT w m ()
cmap f = do
  sx :: Storage (PrimState m) a <- getStore
  sy :: Storage (PrimState m) b <- getStore
  is <- storeMembers sx
  flip U.mapM_ is $ \i -> do
    ma <- storeGet sx i
    for_ ma $ \a -> storeSet sy i $ f a
{-# INLINE cmap #-}

-- | Same as 'ctraverse_'
cmapM_ :: forall w m a . Has w m a => (a -> SystemT w m ()) -> SystemT w m ()
cmapM_ = ctraverse_
{-# INLINE cmapM_ #-}

-- | Fold over all instances of component
cfold :: forall w m a c . Has w m c => (a -> c -> a) -> a -> SystemT w m a
cfold f a0 = do
  sx :: Storage (PrimState m) c <- getStore
  es <- storeMembers sx
  U.foldM' (g sx) a0 es
  where
    g sx !a !e = do
      mc <- storeGet sx e
      pure $ maybe a (f a) mc
{-# INLINABLE cfold #-}

withC :: forall w m a b . (Has w m a)
  => Entity
  -> (a -> b)
  -> SystemT w m (Maybe b)
withC e f = do
  sx :: Storage (PrimState m) a <- getStore
  ma <- storeGet sx e
  pure $ f <$> ma
{-# INLINABLE withC #-}

withCM :: forall w m a b . (Has w m a)
  => Entity
  -> (a -> SystemT w m b)
  -> SystemT w m (Maybe b)
withCM e f = do
  sx :: Storage (PrimState m) a <- getStore
  ma <- storeGet sx e
  traverse f ma
{-# INLINABLE withCM #-}

withCM_ :: forall w m a b . (Has w m a)
  => Entity
  -> (a -> SystemT w m ())
  -> SystemT w m ()
withCM_ e f = do
  sx :: Storage (PrimState m) a <- getStore
  ma <- storeGet sx e
  traverse_ f ma
{-# INLINABLE withCM_ #-}
