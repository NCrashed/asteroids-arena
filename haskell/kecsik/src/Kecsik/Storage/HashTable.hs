module Kecsik.Storage.HashTable(
    HashTable(..)
  ) where

import Control.Monad.Primitive
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.Vector.Unboxed as U

newtype HashTable s a = HashTable { unHashTable :: C.HashTable s Int (Ref s a) }
  deriving (Generic)

instance Mutable s a => Mutable s (HashTable s a)

type instance Elem (HashTable s a) = a

instance (PrimMonad m, PrimState m ~ s, Mutable s a) => IsStorage m (HashTable s a) where
  storeInit Nothing = stToPrim $ fmap HashTable H.new
  storeInit (Just s) = stToPrim $ fmap HashTable $ H.newSized s

  storeRef (HashTable h) (Entity e) = stToPrim $ H.lookup h e
  {-# INLINEABLE storeRef #-}

  storeAlloc (HashTable h) (Entity e) c = do
    mr <- stToPrim $ H.lookup h e
    case mr of
      Nothing -> do
        cr <- thawRef c
        stToPrim $ H.insert h e cr
        pure cr
      Just cr -> pure cr
  {-# INLINABLE storeAlloc #-}

  storeDestroy (HashTable h) (Entity e) = stToPrim $ H.delete h e
  {-# INLINEABLE storeDestroy #-}

  storeMembers (HashTable h) = do
    es <- stToPrim $ H.toList h
    pure $ U.fromList $ fmap (Entity . fst) es
  {-# INLINABLE storeMembers #-}
