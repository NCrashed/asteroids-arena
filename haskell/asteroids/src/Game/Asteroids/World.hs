{-# LANGUAGE TemplateHaskell #-}
module Game.Asteroids.World(
    World(..)
  , newWorld
  , simulateWorld
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IORef
import Data.Typeable
import Kecsik
import System.Clock

import Game.Asteroids.World.Asteroid
import Game.Asteroids.World.Audio
import Game.Asteroids.World.Bullet
import Game.Asteroids.World.Event
import Game.Asteroids.World.Mass
import Game.Asteroids.World.Physics
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity

-- makeWorld "World" [
--     ''WorldWidth
--   , ''AudioState
--   , ''Asteroid
--   , ''Bullet
--   , ''Mass
--   , ''Player
--   , ''Position
--   , ''Rotation
--   , ''Timer
--   , ''Velocity
--   , ''WorldHeight
--   ]

data World s = World {
    worldECounter     :: !(Storage s EntityCounter)
  , worldComponents   :: !(Storage s (EntityComponents s))
  , worldWidth        :: !(Storage s WorldWidth)
  , worldHeight       :: !(Storage s WorldHeight)
  , worldAudioState   :: !(Storage s AudioState)
  , worldAsteroid     :: !(Storage s Asteroid)
  , worldBullet       :: !(Storage s Bullet)
  , worldMass         :: !(Storage s Mass)
  , worldPlayer       :: !(Storage s Player)
  , worldPosition     :: !(Storage s Position)
  , worldRotation     :: !(Storage s Rotation)
  , worldTimer        :: !(Storage s Timer)
  , worldVelocity     :: !(Storage s Velocity)
  } deriving (Generic)

instance Mutable s (World s) where
  type Ref s (World s) = GRef s (World s)

initWorld :: PrimMonad m => m (World (PrimState m))
initWorld = World
  <$> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  <*> storeInit s
  where
    s = Just 1000

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m EntityCounter where
  getStore = freezePart (posMut @1) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m, Typeable s) => HasStore (World s) m (EntityComponents s) where
  getStore = freezePart (posMut @2) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m WorldWidth where
  getStore = freezePart (posMut @3) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m WorldHeight where
  getStore = freezePart (posMut @4) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m AudioState where
  getStore = freezePart (posMut @5) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Asteroid where
  getStore = freezePart (posMut @6) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Bullet where
  getStore = freezePart (posMut @7) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Mass where
  getStore = freezePart (posMut @8) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Player where
  getStore = freezePart (posMut @9) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Position where
  getStore = freezePart (posMut @10) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Rotation where
  getStore = freezePart (posMut @11) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Timer where
  getStore = freezePart (posMut @12) =<< ask
  {-# INLINE getStore #-}

instance (PrimMonad m, s ~ PrimState m) => HasStore (World s) m Velocity where
  getStore = freezePart (posMut @13) =<< ask
  {-# INLINE getStore #-}

instance (s ~ PrimState m, PrimMonad m) => WorldComponents (World s) m where
  getStoreById i
    | i == componentId (Proxy @s) (Proxy @Asteroid) = do
      s :: Storage s Asteroid <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Bullet) = do
      s :: Storage s Asteroid <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Mass) = do
      s :: Storage s Asteroid <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Position) = do
      s :: Storage s Position <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Rotation) = do
      s :: Storage s Asteroid <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Velocity) = do
      s :: Storage s Velocity <- getStore
      pure . Just . SomeStore $ s
    | otherwise = pure Nothing

newWorld :: (MonadIO m, Typeable (PrimState m), PrimMonad m) => m (World (PrimState m))
newWorld = do
  w <- initWorld
  runWith_ w $ do
    initTimer
    initSizes
    spawnAsteroids
    spawnPlayer
  pure w

simulateWorld :: forall m . (MonadIO m, Typeable (PrimState m), PrimMonad m) => [InputEvent] -> (World (PrimState m)) -> m (World (PrimState m))
simulateWorld es w = do
  t <- liftIO $ getTime Monotonic
  runWith w $ do
    setDelta t
    setPlayerThursting False
    updateAudioCooldowns
    updatePlayerCooldown
    traverse_ reactInputEvent es
    cmapM_ $ \(Position{}, Immutable e :: ImmutableEnt m) -> do
      applyMotion e
      wrapSpace e
      collidePlayer e
      collideBullets e
      ageBullets e
  pure w

collidePlayer :: (MonadIO m
  , Has w m Player
  , Has w m Asteroid
  , Has w m Position
  , Has w m Mass
  , Has w m Rotation
  , Has w m Velocity
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m EntityCounter
  ) => Entity -> SystemT w m ()
collidePlayer e = do
  collided <- playerCollide e
  when collided $ do
    killPlayer
    void $ spawnPlayer

collideBullets :: forall w m . (MonadIO m
  , Has w m Bullet
  , Has w m Asteroid
  , Has w m AudioState
  , Has w m Position
  , Has w m Mass
  , Has w m Rotation
  , Has w m Velocity
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m EntityCounter
  ) => Entity -> SystemT w m ()
collideBullets e = withCM_ e $ \(Bullet{}, Position bpos) -> do
  collided <- bulletCollide bpos
  case collided of
    Nothing -> pure ()
    Just ae -> do
      breakAsteroid ae
      void $ destroyBullet e
