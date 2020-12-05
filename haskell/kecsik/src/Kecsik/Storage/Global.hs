module Kecsik.Storage.Global(
    Global(..)
  , global
  ) where

import Control.Monad.Primitive
import Data.Default
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core

import qualified Data.Vector.Unboxed as U

-- | Global entity to reference. Although you can use any entity id, but it is
-- more pleasant looking constant.
global :: Entity
global = 0

-- | Global storage with single element inside. 'a' should have instance of 'Default'
-- for initial value.
newtype Global s a = Global { unGlobal :: Ref s a }
  deriving (Generic)

instance Mutable s a => Mutable s (Global s a)

type instance Elem (Global s a) = a

instance (PrimMonad m, PrimState m ~ s, Mutable s a, Default a) => IsStorage m (Global s a) where
  storeInit _ = Global <$> thawRef def

  storeRef (Global h) _ = pure $ Just h
  {-# INLINEABLE storeRef #-}

  storeAlloc (Global h) _ _ = pure h
  {-# INLINABLE storeAlloc #-}

  storeDestroy _ _ = pure ()
  {-# INLINEABLE storeDestroy #-}

  storeMembers _ = pure $ U.singleton global
  {-# INLINABLE storeMembers #-}
