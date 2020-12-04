module Kecsik.Storage.HashTable(
    HashTable(..)
  ) where

import Control.Monad.IO.Class
import Data.Mutable
import Data.Proxy
import GHC.Generics
import Kecsik.Core
import qualified Data.HashTable.IO as H

newtype HashTable s a = HashTable { unHashTable :: H.CuckooHashTable Int (Ref s a) }
  deriving (Generic)

instance Mutable s a => Mutable s (HashTable s a)

instance Mutable s a => IsStorage s (HashTable s a) where
  type Elem (HashTable s a) = a
  type Allocate (HashTable s a) c = a

  newStorage Nothing = liftIO $ fmap HashTable H.new
  newStorage (Just s) = liftIO $ fmap HashTable $ H.newSized s

  getComponentRef r (Entity e) = do
    HashTable h <- freezeRef r
    mr <- liftIO $ H.lookup h e
    pure $ getMutPart subPart <$> mr
  {-# INLINE getComponentRef #-}

  writeComponent r (Entity e) _ c = do
    HashTable h <- freezeRef r
    cr <- thawRef c
    liftIO $ H.insert h e cr
    pure $ getMutPart subPart cr
  {-# INLINE writeComponent #-}
