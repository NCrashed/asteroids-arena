module Kecsik.Core(
    Entity(..)
  , World(..)
  , WorldRef
  , newWorld
  , SystemT
  , System
  , runSystem
  , execSystem
  , execSystem_
  , runWith
  , runWith_
  , Component(..)
  , Has(..)
  , StorageHas
  , SubComponent
  , AllocateComponent
  , IsStorage(..)
  , newEntity
  , setComponent
  , getComponent
  , ComponentOps(..)
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Maybe
import Data.Mutable
import Data.Proxy
import GHC.Generics (Generic)

newtype Entity = Entity { unEntity :: Int }
  deriving (Generic, Show, Read, Eq, Ord, Num)

data World s cs = World {
-- | Entity counter that defines id of next new entity
  worldCounter  :: !Int
-- | Components storage
, worldStorage  :: !(Storage s cs)
} deriving (Generic)

instance Mutable s (Storage s cs) => Mutable s (World s cs) where
  type Ref s (World s cs) = GRef s (World s cs)

-- | Create new world with preallocated size in components count.
newWorld :: (MonadIO m, Component s cs) => Maybe Int -> m (World s cs)
newWorld prealloc = World
  <$> pure 0
  <*> newStorage prealloc

type WorldRef m cs = Ref (PrimState m) (World (PrimState m) cs)

newtype SystemT cs m a = SystemT { unSystemT :: ReaderT (WorldRef m cs) m a }
  deriving (Functor, Monad, MonadIO, Applicative)

instance PrimMonad m => PrimMonad (SystemT cs m) where
  type PrimState (SystemT cs m) = PrimState m
  primitive f = lift $ primitive f
  {-# INLINE primitive #-}

instance MonadTrans (SystemT cs) where
  lift = SystemT . lift
  {-# INLINE lift #-}

type System cs a = SystemT cs IO

runSystem :: SystemT cs m a -> WorldRef m cs -> m a
runSystem ma w = runReaderT (unSystemT ma) w

type SystemBase cs m = (Mutable (PrimState m) (Storage (PrimState m) cs), IsStorage (PrimState m) (Storage (PrimState m) cs), PrimMonad m, MonadIO m)

execSystem :: SystemBase cs m => SystemT cs m a -> World (PrimState m) cs -> m (a, World (PrimState m) cs)
execSystem ma w = do
  wr <- thawRef w
  a <- runSystem ma wr
  w' <- freezeRef wr
  pure (a, w')

execSystem_ :: SystemBase cs m => SystemT cs m a -> World (PrimState m) cs -> m a
execSystem_ ma w = runSystem ma =<< thawRef w

runWith :: SystemBase cs m => World (PrimState m) cs -> SystemT cs m a -> m (a, World (PrimState m) cs)
runWith = flip execSystem

runWith_ :: SystemBase cs m => World (PrimState m) cs -> SystemT cs m a -> m a
runWith_ = flip execSystem_

class (IsStorage s (Storage s a), Elem (Storage s a) ~ a) => Component s a where
  type Storage s a :: *

-- | Defines that 'b' has 'a' as subfield with state token 's'
class Has s a b where
  subPart :: MutPart s a b

instance Has s a a where
  subPart = MutPart id

-- | States that storage a contain subcomponent b
type StorageHas s a b = (Has s (Elem a) b, Has s a (Storage s b), Component s b, Mutable s (Elem a), Mutable s b, Allocate a (Storage s b) ~ Allocate (Storage s b) (Storage s b))
-- | States that b is subcomponent of a
type SubComponent m a b = (Component (PrimState m) a, StorageHas (PrimState m) (Storage (PrimState m) a) b)
-- | Allocation information that is needed to write subcomponent b within a
type AllocateComponent m a b = Allocate (Storage (PrimState m) a) (Storage (PrimState m) b)

-- | Storage backend that contains components inside. The typeclass defines low-level primitives to work
-- with mutable components inside the storage.
class IsStorage s a | a -> s where
  -- | Element of the storage.
  type Elem a :: *
  -- | Required info to allocate component inside the storage. The storages that doesn't support partial
  -- allocation only of parts of composite component this will be equal 'Elem a'.
  type Allocate a c :: *

  -- | Create new storage with possible preallocation size (in components count).
  newStorage :: MonadIO m => Maybe Int -> m a

  -- | Get writiable reference to component by entity. If there is no component, return 'Nothing'.
  -- Default implementation selects substorage and redirects call into it. Redefine this for storages at the lists of storage tree.
  getComponentRef :: forall c m . (MonadIO m, PrimMonad m, PrimState m ~ s, StorageHas s a c) => Ref s a -> Entity -> m (Maybe (Ref (PrimState m) c))
  getComponentRef rs e = do
    let csr :: Ref s (Storage s c) = getMutPart subPart rs
    getComponentRef csr e
  {-# INLINE getComponentRef #-}

  -- | Create component for given entity and write given value. If there is no component, it will be allocated.
  -- Default implementation selects substorage and redirects call into it. Redefine this for storages at the lists of storage tree.
  writeComponent :: forall c m . (MonadIO m, PrimMonad m, PrimState m ~ s, StorageHas s a c) => Ref s a -> Entity -> Proxy c -> Allocate a (Storage s c) -> m (Ref s c)
  writeComponent rs e p c = do
    let csr :: Ref s (Storage s c) = getMutPart subPart rs
    writeComponent csr e p c
  {-# INLINE writeComponent #-}

  -- | Return 'True' if storage already has allocated component for the entity.
  hasComponentRef :: forall m c . (MonadIO m, PrimMonad m, PrimState m ~ s, StorageHas s a c) => Ref s a -> Entity -> Proxy c -> m Bool
  hasComponentRef r e _ = do
    mr :: Maybe (Ref (PrimState m) c) <- getComponentRef r e
    pure $ isJust mr

newEntity :: forall cs m . SystemBase cs m => SystemT cs m Entity
newEntity = do
  wr :: WorldRef m cs <- SystemT ask
  i <- withPart (fieldMut #worldCounter) wr $ \cr -> updateRef' cr $ \c -> (c+1, c)
  pure $ Entity i

setComponent :: forall cs c m . (SystemBase cs m, SubComponent m cs c) => Entity -> Proxy c -> AllocateComponent m cs c -> SystemT cs m ()
setComponent e p c = do
  wr :: WorldRef m cs <- SystemT ask
  withPart (fieldMut #worldStorage) wr $ \sr -> void $ writeComponent sr e p c

getComponent :: forall c cs m. (SystemBase cs m, SubComponent m cs c) => Entity -> SystemT cs m (Maybe c)
getComponent e = do
  wr :: WorldRef m cs <- SystemT ask
  withPart (fieldMut #worldStorage) wr $ \sr -> do
    mr <- getComponentRef sr e
    traverse freezeRef mr

-- | Operations with components that are overloaded such you can use it on tuples of components with any order.
class Component (PrimState m) a => ComponentOps m cs a where
  -- | Read component from memory and freeze it. Provides you with immutable copy of data.
  get :: SystemBase cs m => Entity -> SystemT cs m (Maybe a)

  -- | Write component into memory. Copies immutable content into mutable storage.
  set :: SystemBase cs m => Entity -> Proxy a -> AllocateComponent m cs a -> SystemT cs m ()

instance {-# OVERLAPPABLE #-} SubComponent m cs a => ComponentOps m cs a where
  get = getComponent
  {-# INLINE get #-}
  set = setComponent
  {-# INLINE set #-}
