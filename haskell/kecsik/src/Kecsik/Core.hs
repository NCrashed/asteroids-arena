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

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

newtype Entity = Entity { unEntity :: Int }
  deriving (Generic, Show, Read, Eq, Ord, Num, Hashable)

instance Mutable s Entity where
  type Ref s Entity = GRef s Entity

derivingUnbox "Entity" [t| Entity -> Int |] [| unEntity |] [| Entity |]

type WorldRef m w = Ref (PrimState m) w

newtype SystemT w m a = SystemT { unSystemT :: ReaderT (WorldRef m w) m a }
  deriving (Functor, Monad, MonadIO, Applicative)

deriving instance (Monad m, r ~ WorldRef m w) => MonadReader r (SystemT w m)

instance PrimMonad m => PrimMonad (SystemT w m) where
  type PrimState (SystemT w m) = PrimState m
  primitive f = lift $ primitive f
  {-# INLINE primitive #-}

instance MonadTrans (SystemT w) where
  lift = SystemT . lift
  {-# INLINE lift #-}

type System w a = SystemT w IO

runSystem :: SystemT w m a -> WorldRef m w -> m a
runSystem ma w = runReaderT (unSystemT ma) w

type SystemBase w m = (Mutable (PrimState m) w, PrimMonad m)

execSystem :: SystemBase w m => SystemT w m a -> w -> m (a, w)
execSystem ma w = do
  wr <- thawRef w
  a <- runSystem ma wr
  w' <- freezeRef wr
  pure (a, w')

execSystem_ :: SystemBase w m => SystemT w m a -> w -> m a
execSystem_ ma w = runSystem ma =<< thawRef w

runWith :: SystemBase w m => w -> SystemT w m a -> m (a, w)
runWith = flip execSystem

runWith_ :: SystemBase w m => w -> SystemT w m a -> m a
runWith_ = flip execSystem_

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

type family Elem s

class (SystemBase w m, Component (PrimState m) c) => Has w m c where
  getStore :: SystemT w m (Storage (PrimState m) c)

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

type Store w m c = (Has w m c, IsStorage (SystemT w m) (Storage (PrimState m) c))

data SomeStore m = forall s . IsStorage m s => SomeStore { unSomeStore :: s }

class WorldComponents w m where
  getStoreById :: ComponentId -> SystemT w m (Maybe (SomeStore m))

-- | Destroy component by id
destroyById :: (WorldComponents w m, SystemBase w m) => Entity -> ComponentId -> SystemT w m ()
destroyById e i = do
  ms <- getStoreById i
  for_ ms $ \(SomeStore s) -> lift $ storeDestroy s e
{-# INLINE destroyById #-}
