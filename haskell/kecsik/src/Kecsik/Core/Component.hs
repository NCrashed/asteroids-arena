{-# LANGUAGE TemplateHaskell #-}
module Kecsik.Core.Component(
    ComponentId(..)
  , Component(..)
  , HasStore(..)
  ) where

import Data.ByteArray.Hash (SipHash(..), SipKey(..), sipHash)
import Data.Hashable (Hashable)
import Data.Mutable
import Data.Typeable
import Data.Vector.Unboxed.Deriving
import Data.Word
import GHC.Generics

import Kecsik.Core.Storage
import Kecsik.Core.System

import qualified Data.ByteString.Char8 as BS

-- | Component unique id that is used to track which components the entity has.
newtype ComponentId = ComponentId { unComponentId :: Word64 }
  deriving (Generic, Show, Read, Eq, Ord, Num, Hashable)

instance Mutable s ComponentId where
  type Ref s ComponentId = GRef s ComponentId

derivingUnbox "ComponentId" [t| ComponentId -> Word64 |] [| unComponentId |] [| ComponentId |]

class (Elem (Storage s a) ~ a, Typeable a) => Component s a where
  type Storage s a :: *

  -- | Get unique component id. By default it is hash from it type.
  componentId :: Proxy s -> Proxy a -> ComponentId
  componentId _ p = ComponentId h
    where
      SipHash h = sipHash (SipKey 23 42) bs
      bs = BS.pack $ show $ typeRep p
  {-# INLINE componentId #-}

class (SystemBase w m, Component (PrimState m) c, IsStorage (SystemT w m) (Storage (PrimState m) c)) => HasStore w m c where
  getStore :: SystemT w m (Storage (PrimState m) c)
