module Kecsik.Core.Storage(
    Elem
  , ElemRef
  , IsStorage(..)
  , SomeStore(..)
  ) where

import Data.Maybe 
import Data.Mutable
import Kecsik.Core.Entity
import Kecsik.Core.System

import qualified Data.Vector.Unboxed as U

type family Elem s

type ElemRef m a = Ref (PrimState m) (Elem a)

class (PrimMonad m, Mutable (PrimState m) (Elem s)) => IsStorage m s where
  -- | Initialize a new empty store. Takes optional preallocation size.
  storeInit :: Maybe Int -> m s
  -- | Read a reference from the store.
  storeRef :: s -> Entity -> m (Maybe (ElemRef m s))
  -- | Allocate with given value if missing, if exists return reference to current value.
  storeAlloc :: s -> Entity -> Elem s -> m (ElemRef m s)
  -- | Destroys the component for a given index.
  storeDestroy :: s -> Entity -> m ()
  -- | Returns an unboxed vector of member indices
  storeMembers :: s -> m (U.Vector Entity)

  -- | Reads a component from the store and freeze it to immutable copy.
  storeGet :: s -> Entity -> m (Maybe (Elem s))
  storeGet s e = do
    mr <- storeRef s e
    traverse freezeRef mr
  {-# INLINABLE storeGet #-}

  -- | Write down immutable value of component
  storeSet :: s -> Entity -> Elem s -> m ()
  storeSet s e c = do
    cr <- storeAlloc s e c
    copyRef cr c
  {-# INLINABLE storeSet #-}

  -- | Returns whether there is a component for the given index.
  storeExists :: s -> Entity -> m Bool
  storeExists s e = do
    mr <- storeRef s e
    pure $ isJust mr
  {-# INLINABLE storeExists #-}

data SomeStore m = forall s . IsStorage m s => SomeStore { unSomeStore :: s }
