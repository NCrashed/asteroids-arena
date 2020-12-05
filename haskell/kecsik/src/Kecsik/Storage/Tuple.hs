{-# LANGUAGE InstanceSigs #-}
module Kecsik.Storage.Tuple() where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core

import qualified Data.Vector.Unboxed as U

type instance Elem (a, b) = (Elem a, Elem b)
type instance Elem (a, b, c) = (Elem a, Elem b, Elem c)
type instance Elem (a, b, c, d) = (Elem a, Elem b, Elem c, Elem d)
type instance Elem (a, b, c, d, e) = (Elem a, Elem b, Elem c, Elem d, Elem e)
type instance Elem (a, b, c, d, e, f) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f)
type instance Elem (a, b, c, d, e, f, g) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f, Elem g)
type instance Elem (a, b, c, d, e, f, g, k) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f, Elem g, Elem k)

instance (Component s a, Component s b) => Component s (a, b) where
  type Storage s (a, b) = (Storage s a, Storage s b)

instance (Component s a, Component s b, Component s c) => Component s (a, b, c) where
  type Storage s (a, b, c) = (Storage s a, Storage s b, Storage s c)

instance (Component s a, Component s b, Component s c, Component s d) => Component s (a, b, c, d) where
  type Storage s (a, b, c, d) = (Storage s a, Storage s b, Storage s c, Storage s d)

instance (Component s a, Component s b, Component s c, Component s d, Component s e) => Component s (a, b, c, d, e) where
  type Storage s (a, b, c, d, e) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e)

instance (Component s a, Component s b, Component s c, Component s d, Component s e, Component s f) => Component s (a, b, c, d, e, f) where
  type Storage s (a, b, c, d, e, f) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e, Storage s f)

instance (Component s a, Component s b, Component s c, Component s d, Component s e, Component s f, Component s g) => Component s (a, b, c, d, e, f, g) where
  type Storage s (a, b, c, d, e, f, g) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e, Storage s f, Storage s g)

instance (Component s a, Component s b, Component s c, Component s d, Component s e, Component s f, Component s g, Component s k) => Component s (a, b, c, d, e, f, g, k) where
  type Storage s (a, b, c, d, e, f, g, k) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e, Storage s f, Storage s g, Storage s k)

instance (Has w m a, Has w m b) => Has w m (a, b) where
  getStore = (,) <$> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c) => Has w m (a, b, c) where
  getStore = (,,) <$> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c, Has w m d) => Has w m (a, b, c, d) where
  getStore = (,,,) <$> getStore <*> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c, Has w m d, Has w m e) => Has w m (a, b, c, d, e) where
  getStore = (,,,,) <$> getStore <*> getStore <*> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c, Has w m d, Has w m e, Has w m f) => Has w m (a, b, c, d, e, f) where
  getStore = (,,,,,) <$> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c, Has w m d, Has w m e, Has w m f, Has w m g) => Has w m (a, b, c, d, e, f, g) where
  getStore = (,,,,,,) <$> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

instance (Has w m a, Has w m b, Has w m c, Has w m d, Has w m e, Has w m f, Has w m g, Has w m k) => Has w m (a, b, c, d, e, f, g, k) where
  getStore = (,,,,,,,) <$> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore <*> getStore
  {-# INLINABLE getStore #-}

storeRefT :: IsStorage m s => s -> Entity -> MaybeT m (ElemRef m s)
storeRefT s e = MaybeT $ storeRef s e
{-# INLINE storeRefT #-}

instance (IsStorage m a, IsStorage m b) => IsStorage m (a, b) where
  storeInit ms = (,) <$> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b) e = runMaybeT $ (,) <$> storeRefT a e <*> storeRefT b e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b) e (ac, bc) = (,) <$> storeAlloc a e ac <*> storeAlloc b e bc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b) e = storeDestroy a e >> storeDestroy b e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b) = storeMembers a >>= U.filterM (storeExists b)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c) => IsStorage m (a, b, c) where
  storeInit ms = (,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c) e = runMaybeT $ (,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c) e (ac, bc, cc) = (,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c, IsStorage m d) => IsStorage m (a, b, c, d) where
  storeInit ms = (,,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c, d) e = runMaybeT $ (,,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e <*> storeRefT d e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c, d) e (ac, bc, cc, dc) = (,,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc <*> storeAlloc d e dc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c, d) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e >> storeDestroy d e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c, d) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c) >>= U.filterM (storeExists d)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c, IsStorage m d, IsStorage m e) => IsStorage m (a, b, c, d, e) where
  storeInit ms = (,,,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c, d, es) e = runMaybeT $ (,,,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e <*> storeRefT d e <*> storeRefT es e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c, d, es) e (ac, bc, cc, dc, ec) = (,,,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc <*> storeAlloc d e dc <*> storeAlloc es e ec
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c, d, es) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e >> storeDestroy d e >> storeDestroy es e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c, d, es) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c) >>= U.filterM (storeExists d) >>= U.filterM (storeExists es)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c, IsStorage m d, IsStorage m e, IsStorage m f) => IsStorage m (a, b, c, d, e, f) where
  storeInit ms = (,,,,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c, d, es, f) e = runMaybeT $ (,,,,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e <*> storeRefT d e <*> storeRefT es e <*> storeRefT f e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c, d, es, f) e (ac, bc, cc, dc, ec, fc) = (,,,,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc <*> storeAlloc d e dc <*> storeAlloc es e ec <*> storeAlloc f e fc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c, d, es, f) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e >> storeDestroy d e >> storeDestroy es e >> storeDestroy f e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c, d, es, f) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c) >>= U.filterM (storeExists d) >>= U.filterM (storeExists es) >>= U.filterM (storeExists f)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c, IsStorage m d, IsStorage m e, IsStorage m f, IsStorage m g) => IsStorage m (a, b, c, d, e, f, g) where
  storeInit ms = (,,,,,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c, d, es, f, g) e = runMaybeT $ (,,,,,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e <*> storeRefT d e <*> storeRefT es e <*> storeRefT f e <*> storeRefT g e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c, d, es, f, g) e (ac, bc, cc, dc, ec, fc, gc) = (,,,,,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc <*> storeAlloc d e dc <*> storeAlloc es e ec <*> storeAlloc f e fc <*> storeAlloc g e gc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c, d, es, f, g) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e >> storeDestroy d e >> storeDestroy es e >> storeDestroy f e >> storeDestroy g e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c, d, es, f, g) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c) >>= U.filterM (storeExists d) >>= U.filterM (storeExists es) >>= U.filterM (storeExists f) >>= U.filterM (storeExists g)
  {-# INLINABLE storeMembers #-}

instance (IsStorage m a, IsStorage m b, IsStorage m c, IsStorage m d, IsStorage m e, IsStorage m f, IsStorage m g, IsStorage m k) => IsStorage m (a, b, c, d, e, f, g, k) where
  storeInit ms = (,,,,,,,) <$> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms <*> storeInit ms
  {-# INLINABLE storeInit #-}

  storeRef (a, b, c, d, es, f, g, k) e = runMaybeT $ (,,,,,,,) <$> storeRefT a e <*> storeRefT b e <*> storeRefT c e <*> storeRefT d e <*> storeRefT es e <*> storeRefT f e <*> storeRefT g e <*> storeRefT k e
  {-# INLINABLE storeRef #-}

  storeAlloc (a, b, c, d, es, f, g, k) e (ac, bc, cc, dc, ec, fc, gc, kc) = (,,,,,,,) <$> storeAlloc a e ac <*> storeAlloc b e bc <*> storeAlloc c e cc <*> storeAlloc d e dc <*> storeAlloc es e ec <*> storeAlloc f e fc <*> storeAlloc g e gc <*> storeAlloc k e kc
  {-# INLINABLE storeAlloc #-}

  storeDestroy (a, b, c, d, es, f, g, k) e = storeDestroy a e >> storeDestroy b e >> storeDestroy c e >> storeDestroy d e >> storeDestroy es e >> storeDestroy f e >> storeDestroy g e >> storeDestroy k e
  {-# INLINABLE storeDestroy #-}

  storeMembers (a, b, c, d, es, f, g, k) = storeMembers a >>= U.filterM (storeExists b) >>= U.filterM (storeExists c) >>= U.filterM (storeExists d) >>= U.filterM (storeExists es) >>= U.filterM (storeExists f) >>= U.filterM (storeExists g) >>= U.filterM (storeExists k)
  {-# INLINABLE storeMembers #-}
