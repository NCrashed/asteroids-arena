module Game.Asteroids.System(
    withCM_
  , withCM
  , withC
  , modifyMay
  , citerate_
  ) where

import Apecs
import Apecs.Core
import Control.Monad
import Data.Foldable (traverse_)

import qualified Data.Vector.Unboxed as U

withC :: forall a b w m . (Get w m a) => Entity -> (a -> b) -> SystemT w m (Maybe b)
withC (Entity e) f = do
  sx :: Storage a <- getStore
  ex <- lift $ explExists sx e
  if ex then do
    a <- lift $ explGet sx e
    pure . Just $! f a
  else pure Nothing
{-# INLINABLE withC #-}

withCM_ :: forall a w m . (Get w m a) => Entity -> (a -> SystemT w m ()) -> SystemT w m ()
withCM_ (Entity e) f = do
  sx :: Storage a <- getStore
  ex <- lift $ explExists sx e
  when ex $ f =<< lift (explGet sx e)
{-# INLINABLE withCM_ #-}

withCM :: forall a b w m . (Get w m a) => Entity -> (a -> SystemT w m b) -> SystemT w m (Maybe b)
withCM (Entity e) f = do
  sx :: Storage a <- getStore
  ex <- lift $ explExists sx e
  if ex then do
    a <- lift $ explGet sx e
    b <- f a
    pure . Just $! b
  else pure Nothing
{-# INLINABLE withCM #-}

modifyMay :: forall a b w m . (Get w m a, Set w m b) => Entity -> (a -> Maybe b) -> SystemT w m ()
modifyMay (Entity e) f = do
  sx :: Storage a <- getStore
  sy :: Storage b <- getStore
  ex <- lift $ explExists sx e
  when ex $ do
    a <- lift $ explGet sx e
    traverse_ (lift . explSet sy e) $ f a
{-# INLINABLE modifyMay #-}

-- | Iterate over all entities that have given component but check that we haven't destroyed it
-- before each loop iteration.
citerate_ :: forall a b w m . (Get w m a, Members w m a) => Proxy a -> (Entity -> SystemT w m ()) -> SystemT w m ()
citerate_ _ f = do
  sx :: Storage a <- getStore
  is <- lift $ explMembers sx
  U.forM_ is $ \i -> do
    ex <- lift $ explExists sx i
    when ex . f . Entity $! i
{-# INLINABLE citerate_ #-}
