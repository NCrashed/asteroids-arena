module Kecsik.Component.Entity(
    EntityCounter(..)
  , newEntity
  ) where

import Data.Default
import Data.Maybe
import Data.Mutable
import GHC.Generics
import Kecsik.Core
import Kecsik.Storage.Global
import Kecsik.System

newtype EntityCounter = EntityCounter { unEntityCounter :: Entity }
  deriving (Generic, Eq, Show, Read, Num)

instance Mutable s EntityCounter where
  type Ref s EntityCounter = GRef s EntityCounter

instance Component s EntityCounter where
  type Storage s EntityCounter = Global s EntityCounter

instance Default EntityCounter where
  def = EntityCounter 0
  {-# INLINE def #-}

newEntity :: Store w m EntityCounter => SystemT w m Entity
newEntity = fmap (fromMaybe global) $ update global $ \(EntityCounter e) -> (EntityCounter $ e+1, e)
{-# INLINE newEntity #-}
