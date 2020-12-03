module Kecsik.Core(
    Entity(..)
  , World(..)
  , newWorld
  , SystemT
  , System
  , runSystem
  , execSystem
  , execSystem_
  , runWith
  , runWith_
  , Component(..)
  , IsStorage(..)
  , newEntity
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Mutable
import GHC.Generics (Generic)

newtype Entity = Entity { unEntity :: Int }
  deriving (Generic, Show, Read, Eq, Ord, Num)

data World cs = World {
-- | Entity counter that defines id of next new entity
  worldCounter  :: !Int
-- | Components storage
, worldStorage  :: !(Storage cs)
} deriving (Generic)

instance Mutable s (Storage cs) => Mutable s (World cs) where
  type Ref s (World cs) = GRef s (World cs)

newWorld :: (MonadIO m, Component cs) => m (World cs)
newWorld = World
  <$> pure 0
  <*> newStorage

newtype SystemT cs m a = SystemT { unSystemT :: ReaderT (Ref (PrimState m) (World cs)) m a }
  deriving (Functor, Monad, Applicative)

instance PrimMonad m => PrimMonad (SystemT cs m) where
  type PrimState (SystemT cs m) = PrimState m
  primitive f = lift $ primitive f
  {-# INLINE primitive #-}

instance MonadTrans (SystemT cs) where
  lift = SystemT . lift
  {-# INLINE lift #-}

type System cs a = SystemT cs IO

runSystem :: SystemT cs m a -> Ref (PrimState m) (World cs) -> m a
runSystem ma w = runReaderT (unSystemT ma) w

type SystemBase cs m = (Mutable (PrimState m) (Storage cs), PrimMonad m)

execSystem :: SystemBase cs m => SystemT cs m a -> World cs -> m (a, World cs)
execSystem ma w = do
  wr <- thawRef w
  a <- runSystem ma wr
  w' <- freezeRef wr
  pure (a, w')

execSystem_ :: SystemBase cs m => SystemT cs m a -> World cs -> m a
execSystem_ ma w = runSystem ma =<< thawRef w

runWith :: SystemBase cs m => World cs -> SystemT cs m a -> m (a, World cs)
runWith = flip execSystem

runWith_ :: SystemBase cs m => World cs -> SystemT cs m a -> m a
runWith_ = flip execSystem_

class IsStorage (Storage a) => Component a where
  type Storage a :: *

class IsStorage a where
  newStorage :: MonadIO m => m a

newEntity :: forall cs m . SystemBase cs m => SystemT cs m Entity
newEntity = do
  wr ::  GRef (PrimState m) (World cs) <- SystemT ask
  i <- withPart (fieldMut #worldCounter) wr $ \cr -> updateRef' cr $ \c -> (c+1, c)
  pure $ Entity i
