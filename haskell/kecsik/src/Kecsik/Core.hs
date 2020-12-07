{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Kecsik.Core(
    Entity(..)
  , SystemT
  , System
  , SystemBase
  , runSystem
  , execSystem
  , execSystem_
  , runWith
  , runWith_
  , ComponentId(..)
  , Component(..)
  , Elem
  , ElemRef
  , Has(..)
  , IsStorage(..)
  , Store(..)
  , SomeStore(..)
  , WorldComponents(..)
  , destroyById
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Trans
import Data.ByteArray.Hash (SipHash(..), SipKey(..), sipHash)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Maybe
import Data.Mutable
import Data.Proxy
import Data.Typeable
import Data.Vector.Unboxed.Deriving
import Data.Word
import GHC.Generics (Generic)

import Kecsik.Core.Component
import Kecsik.Core.Entity
import Kecsik.Core.Storage
import Kecsik.Core.System

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

class WorldComponents w m where
  getStoreById :: ComponentId -> SystemT w m (Maybe (SomeStore m))

-- | Destroy component by id
destroyById :: (WorldComponents w m, SystemBase w m) => Entity -> ComponentId -> SystemT w m ()
destroyById e i = do
  ms <- getStoreById i
  for_ ms $ \(SomeStore s) -> lift $ storeDestroy s e
{-# INLINE destroyById #-}
