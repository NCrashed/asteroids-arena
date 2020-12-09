{-# LANGUAGE TemplateHaskell #-}
module Game.Asteroids.World(
    World(..)
  , newWorld
  , simulateWorld
  ) where

import Apecs
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IORef
import Data.Mutable
import Data.Typeable
import GHC.Generics
import System.Clock
import System.Mem           (performMajorGC)

import Game.Asteroids.System
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

makeWorld "World" [
    ''WorldWidth
  , ''AudioState
  , ''Asteroid
  , ''Bullet
  , ''Mass
  , ''Player
  , ''Position
  , ''Rotation
  , ''Timer
  , ''Velocity
  , ''WorldHeight
  ]

newWorld :: (MonadIO m, Typeable (PrimState m), PrimMonad m) => m World
newWorld = do
  w <- liftIO initWorld
  runWith w $ do
    initTimer
    initSizes
    spawnAsteroids
    spawnPlayer
  pure w

simulateWorld :: forall m . (MonadIO m, Typeable (PrimState m), PrimMonad m) => [InputEvent] -> World -> m World
simulateWorld es w = do
  t <- liftIO $ getTime Monotonic
  runWith w $ do
    setDelta t
    setPlayerThursting False
    updateAudioCooldowns
    updatePlayerCooldown
    traverse_ reactInputEvent es
    citerate_ (Proxy @Position) $ \e -> do
      applyMotion e
      wrapSpace e
      collidePlayer e
      collideBullets e
      ageBullets e
    liftIO performMajorGC
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
