{-# LANGUAGE StandaloneDeriving #-}
module Kecsik.Core.System(
    WorldRef
  , SystemT(..)
  , System
  , SystemBase
  , runSystem
  , execSystem
  , execSystem_
  , runWith
  , runWith_
  ) where

import Control.Monad.Primitive
import Control.Monad.Reader
import Data.Mutable

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
