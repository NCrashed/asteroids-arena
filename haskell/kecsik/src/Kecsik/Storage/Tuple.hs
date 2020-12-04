{-# LANGUAGE InstanceSigs #-}
module Kecsik.Storage.Tuple() where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core

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

instance (Component s a, Component s b, Component s c, Component s d, Component s e, Component s f, Component s g, Component s m) => Component s (a, b, c, d, e, f, g, m) where
  type Storage s (a, b, c, d, e, f, g, m) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e, Storage s f, Storage s g, Storage s m)

instance (Component s a, Component s b, Component s c, Component s d, Component s e, Component s f, Component s g, Component s m, Component s k) => Component s (a, b, c, d, e, f, g, m, k) where
  type Storage s (a, b, c, d, e, f, g, m, k) = (Storage s a, Storage s b, Storage s c, Storage s d, Storage s e, Storage s f, Storage s g, Storage s m, Storage s k)

instance Has s (a, b) a where subPart = mutFst
instance Has s (a, b) b where subPart = mutSnd

instance Has s (a, b, c) a where subPart = MutPart $ \(v, _, _) -> v
instance Has s (a, b, c) b where subPart = MutPart $ \(_, v, _) -> v
instance Has s (a, b, c) c where subPart = MutPart $ \(_, _, v) -> v

instance Has s (a, b, c, d) a where subPart = MutPart $ \(v, _, _, _) -> v
instance Has s (a, b, c, d) b where subPart = MutPart $ \(_, v, _, _) -> v
instance Has s (a, b, c, d) c where subPart = MutPart $ \(_, _, v, _) -> v
instance Has s (a, b, c, d) d where subPart = MutPart $ \(_, _, _, v) -> v

instance Has s (a, b, c, d, e) a where subPart = MutPart $ \(v, _, _, _, _) -> v
instance Has s (a, b, c, d, e) b where subPart = MutPart $ \(_, v, _, _, _) -> v
instance Has s (a, b, c, d, e) c where subPart = MutPart $ \(_, _, v, _, _) -> v
instance Has s (a, b, c, d, e) d where subPart = MutPart $ \(_, _, _, v, _) -> v
instance Has s (a, b, c, d, e) e where subPart = MutPart $ \(_, _, _, _, v) -> v

instance Has s (a, b, c, d, e, f) a where subPart = MutPart $ \(v, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f) b where subPart = MutPart $ \(_, v, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f) c where subPart = MutPart $ \(_, _, v, _, _, _) -> v
instance Has s (a, b, c, d, e, f) d where subPart = MutPart $ \(_, _, _, v, _, _) -> v
instance Has s (a, b, c, d, e, f) e where subPart = MutPart $ \(_, _, _, _, v, _) -> v
instance Has s (a, b, c, d, e, f) f where subPart = MutPart $ \(_, _, _, _, _, v) -> v

instance Has s (a, b, c, d, e, f, g) a where subPart = MutPart $ \(v, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g) b where subPart = MutPart $ \(_, v, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g) c where subPart = MutPart $ \(_, _, v, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g) d where subPart = MutPart $ \(_, _, _, v, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g) e where subPart = MutPart $ \(_, _, _, _, v, _, _) -> v
instance Has s (a, b, c, d, e, f, g) f where subPart = MutPart $ \(_, _, _, _, _, v, _) -> v
instance Has s (a, b, c, d, e, f, g) g where subPart = MutPart $ \(_, _, _, _, _, _, v) -> v

instance Has s (a, b, c, d, e, f, g, m) a where subPart = MutPart $ \(v, _, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) b where subPart = MutPart $ \(_, v, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) c where subPart = MutPart $ \(_, _, v, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) d where subPart = MutPart $ \(_, _, _, v, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) e where subPart = MutPart $ \(_, _, _, _, v, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) f where subPart = MutPart $ \(_, _, _, _, _, v, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m) g where subPart = MutPart $ \(_, _, _, _, _, _, v, _) -> v
instance Has s (a, b, c, d, e, f, g, m) m where subPart = MutPart $ \(_, _, _, _, _, _, _, v) -> v

instance Has s (a, b, c, d, e, f, g, m, k) a where subPart = MutPart $ \(v, _, _, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) b where subPart = MutPart $ \(_, v, _, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) c where subPart = MutPart $ \(_, _, v, _, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) d where subPart = MutPart $ \(_, _, _, v, _, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) e where subPart = MutPart $ \(_, _, _, _, v, _, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) f where subPart = MutPart $ \(_, _, _, _, _, v, _, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) g where subPart = MutPart $ \(_, _, _, _, _, _, v, _, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) m where subPart = MutPart $ \(_, _, _, _, _, _, _, v, _) -> v
instance Has s (a, b, c, d, e, f, g, m, k) k where subPart = MutPart $ \(_, _, _, _, _, _, _, _, v) -> v

instance (IsStorage s a, IsStorage s b) => IsStorage s (a, b) where
  type Elem (a, b) = (Elem a, Elem b)
  type Allocate (a, b) a = Elem a
  type Allocate (a, b) b = Elem b

  newStorage ms = (,) <$> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c) => IsStorage s (a, b, c) where
  type Elem (a, b, c) = (Elem a, Elem b, Elem c)
  type Allocate (a, b, c) a = Elem a
  type Allocate (a, b, c) b = Elem b
  type Allocate (a, b, c) c = Elem c

  newStorage ms = (,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d) => IsStorage s (a, b, c, d) where
  type Elem (a, b, c, d) = (Elem a, Elem b, Elem c, Elem d)
  type Allocate (a, b, c, d) a = Elem a
  type Allocate (a, b, c, d) b = Elem b
  type Allocate (a, b, c, d) c = Elem c
  type Allocate (a, b, c, d) d = Elem d

  newStorage ms = (,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d, IsStorage s e) => IsStorage s (a, b, c, d, e) where
  type Elem (a, b, c, d, e) = (Elem a, Elem b, Elem c, Elem d, Elem e)
  type Allocate (a, b, c, d, e) a = Elem a
  type Allocate (a, b, c, d, e) b = Elem b
  type Allocate (a, b, c, d, e) c = Elem c
  type Allocate (a, b, c, d, e) d = Elem d
  type Allocate (a, b, c, d, e) e = Elem e

  newStorage ms = (,,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d, IsStorage s e, IsStorage s f) => IsStorage s (a, b, c, d, e, f) where
  type Elem (a, b, c, d, e, f) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f)
  type Allocate (a, b, c, d, e, f) a = Elem a
  type Allocate (a, b, c, d, e, f) b = Elem b
  type Allocate (a, b, c, d, e, f) c = Elem c
  type Allocate (a, b, c, d, e, f) d = Elem d
  type Allocate (a, b, c, d, e, f) e = Elem e
  type Allocate (a, b, c, d, e, f) f = Elem f

  newStorage ms = (,,,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d, IsStorage s e, IsStorage s f, IsStorage s g) => IsStorage s (a, b, c, d, e, f, g) where
  type Elem (a, b, c, d, e, f, g) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f, Elem g)
  type Allocate (a, b, c, d, e, f, g) a = Elem a
  type Allocate (a, b, c, d, e, f, g) b = Elem b
  type Allocate (a, b, c, d, e, f, g) c = Elem c
  type Allocate (a, b, c, d, e, f, g) d = Elem d
  type Allocate (a, b, c, d, e, f, g) e = Elem e
  type Allocate (a, b, c, d, e, f, g) f = Elem f
  type Allocate (a, b, c, d, e, f, g) g = Elem g

  newStorage ms = (,,,,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d, IsStorage s e, IsStorage s f, IsStorage s g, IsStorage s m) => IsStorage s (a, b, c, d, e, f, g, m) where
  type Elem (a, b, c, d, e, f, g, m) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f, Elem g, Elem m)
  type Allocate (a, b, c, d, e, f, g, m) a = Elem a
  type Allocate (a, b, c, d, e, f, g, m) b = Elem b
  type Allocate (a, b, c, d, e, f, g, m) c = Elem c
  type Allocate (a, b, c, d, e, f, g, m) d = Elem d
  type Allocate (a, b, c, d, e, f, g, m) e = Elem e
  type Allocate (a, b, c, d, e, f, g, m) f = Elem f
  type Allocate (a, b, c, d, e, f, g, m) g = Elem g
  type Allocate (a, b, c, d, e, f, g, m) m = Elem m

  newStorage ms = (,,,,,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}

instance (IsStorage s a, IsStorage s b, IsStorage s c, IsStorage s d, IsStorage s e, IsStorage s f, IsStorage s g, IsStorage s m, IsStorage s k) => IsStorage s (a, b, c, d, e, f, g, m, k) where
  type Elem (a, b, c, d, e, f, g, m, k) = (Elem a, Elem b, Elem c, Elem d, Elem e, Elem f, Elem g, Elem m, Elem k)
  type Allocate (a, b, c, d, e, f, g, m, k) a = Elem a
  type Allocate (a, b, c, d, e, f, g, m, k) b = Elem b
  type Allocate (a, b, c, d, e, f, g, m, k) c = Elem c
  type Allocate (a, b, c, d, e, f, g, m, k) d = Elem d
  type Allocate (a, b, c, d, e, f, g, m, k) e = Elem e
  type Allocate (a, b, c, d, e, f, g, m, k) f = Elem f
  type Allocate (a, b, c, d, e, f, g, m, k) g = Elem g
  type Allocate (a, b, c, d, e, f, g, m, k) m = Elem m
  type Allocate (a, b, c, d, e, f, g, m, k) k = Elem k

  newStorage ms = (,,,,,,,,) <$> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms <*> newStorage ms
  {-# INLINE newStorage #-}
