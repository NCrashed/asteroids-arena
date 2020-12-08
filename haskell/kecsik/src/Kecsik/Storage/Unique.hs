module Kecsik.Storage.Unique(
    Unique(..)
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Default
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core

import qualified Data.Vector.Unboxed as U

-- | Unique storage with single element inside. 'a' should have instance of 'Default'
-- for initial value.
data Unique s a = Unique { unUnique :: !(Ref s a), uniqueEntity :: !(Ref s (Maybe Entity)) }
  deriving (Generic)

instance Mutable s a => Mutable s (Unique s a)

type instance Elem (Unique s a) = a

instance (PrimMonad m, PrimState m ~ s, Mutable s a, Default a) => IsStorage m (Unique s a) where
  storeInit _ = Unique <$> thawRef def <*> thawRef Nothing

  storeRef (Unique h er) e = do
    e' <- freezeRef er
    pure $ if e' == Just e then Just h else Nothing
  {-# INLINEABLE storeRef #-}

  storeAlloc (Unique h er) e v = do
    modifyRef' h $ const v
    modifyRef' er $ const $ Just e
    pure h
  {-# INLINABLE storeAlloc #-}

  storeDestroy (Unique h er) e = do
    e' <- freezeRef er
    when (e' == Just e) $ modifyRef' er $ const Nothing
  {-# INLINEABLE storeDestroy #-}

  storeMembers (Unique h er) = do
    me <- freezeRef er
    pure $ maybe U.empty U.singleton me
  {-# INLINABLE storeMembers #-}
