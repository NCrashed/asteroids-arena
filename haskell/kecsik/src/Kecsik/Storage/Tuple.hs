module Kecsik.Storage.Tuple() where

import Kecsik.Core

instance (Component a, Component b) => Component (a, b) where
  type Storage (a, b) = (Storage a, Storage b)

instance (Component a, Component b, Component c) => Component (a, b, c) where
  type Storage (a, b, c) = (Storage a, Storage b, Storage c)

instance (Component a, Component b, Component c, Component d) => Component (a, b, c, d) where
  type Storage (a, b, c, d) = (Storage a, Storage b, Storage c, Storage d)

instance (Component a, Component b, Component c, Component d, Component e) => Component (a, b, c, d, e) where
  type Storage (a, b, c, d, e) = (Storage a, Storage b, Storage c, Storage d, Storage e)

instance (Component a, Component b, Component c, Component d, Component e, Component f) => Component (a, b, c, d, e, f) where
  type Storage (a, b, c, d, e, f) = (Storage a, Storage b, Storage c, Storage d, Storage e, Storage f)

instance (Component a, Component b, Component c, Component d, Component e, Component f, Component g) => Component (a, b, c, d, e, f, g) where
  type Storage (a, b, c, d, e, f, g) = (Storage a, Storage b, Storage c, Storage d, Storage e, Storage f, Storage g)

instance (Component a, Component b, Component c, Component d, Component e, Component f, Component g, Component m) => Component (a, b, c, d, e, f, g, m) where
  type Storage (a, b, c, d, e, f, g, m) = (Storage a, Storage b, Storage c, Storage d, Storage e, Storage f, Storage g, Storage m)

instance (Component a, Component b, Component c, Component d, Component e, Component f, Component g, Component m, Component k) => Component (a, b, c, d, e, f, g, m, k) where
  type Storage (a, b, c, d, e, f, g, m, k) = (Storage a, Storage b, Storage c, Storage d, Storage e, Storage f, Storage g, Storage m, Storage k)

instance (IsStorage a, IsStorage b) => IsStorage (a, b) where
  newStorage = (,) <$> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c) => IsStorage (a, b, c) where
  newStorage = (,,) <$> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d) => IsStorage (a, b, c, d) where
  newStorage = (,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d, IsStorage e) => IsStorage (a, b, c, d, e) where
  newStorage = (,,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d, IsStorage e, IsStorage f) => IsStorage (a, b, c, d, e, f) where
  newStorage = (,,,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d, IsStorage e, IsStorage f, IsStorage g) => IsStorage (a, b, c, d, e, f, g) where
  newStorage = (,,,,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d, IsStorage e, IsStorage f, IsStorage g, IsStorage m) => IsStorage (a, b, c, d, e, f, g, m) where
  newStorage = (,,,,,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}

instance (IsStorage a, IsStorage b, IsStorage c, IsStorage d, IsStorage e, IsStorage f, IsStorage g, IsStorage m, IsStorage k) => IsStorage (a, b, c, d, e, f, g, m, k) where
  newStorage = (,,,,,,,,) <$> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage <*> newStorage
  {-# INLINE newStorage #-}
