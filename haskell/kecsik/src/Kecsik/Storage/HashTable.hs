module Kecsik.Storage.HashTable(
    HashTable(..)
  ) where

import Data.Mutable
import GHC.Generics
import Kecsik.Core
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H

newtype HashTable a = HashTable { unHashTable :: H.CuckooHashTable Int a }
  deriving (Generic)

instance Mutable s a => Mutable s (HashTable a)

instance IsStorage (HashTable a) where
  newStorage = liftIO $ fmap HashTable H.new
