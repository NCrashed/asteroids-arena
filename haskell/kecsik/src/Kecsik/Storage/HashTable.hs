module Kecsik.Storage.HashTable(
    HashTable(..)
  ) where

import GHC.Generics
import Kecsik.Core
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H

newtype HashTable a = HashTable { unHashTable :: H.CuckooHashTable Int a }
  deriving (Generic)

instance IsStorage (HashTable a) where
  newStorage = liftIO $ fmap HashTable H.new
