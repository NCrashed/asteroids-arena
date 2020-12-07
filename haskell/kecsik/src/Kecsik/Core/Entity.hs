{-# LANGUAGE TemplateHaskell #-}
module Kecsik.Core.Entity(
    Entity(..)
  ) where

import Data.Hashable (Hashable)
import Data.Mutable
import Data.Vector.Unboxed.Deriving
import GHC.Generics

newtype Entity = Entity { unEntity :: Int }
  deriving (Generic, Show, Read, Eq, Ord, Num, Hashable)

instance Mutable s Entity where
  type Ref s Entity = GRef s Entity

derivingUnbox "Entity" [t| Entity -> Int |] [| unEntity |] [| Entity |]
