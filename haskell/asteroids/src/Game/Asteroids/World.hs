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
import System.Clock

import Game.Asteroids.World.Asteroid
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

newWorld :: IO World
newWorld = do
  w <- initWorld
  runWith w $ do
    initTimer
    initSizes
    spawnAsteroids
    spawnPlayer
    ask

simulateWorld :: [InputEvent] -> World -> IO World
simulateWorld es w = do
  t <- getTime Monotonic
  runWith w $ do
    setDelta t
    setPlayerThursting False
    updatePlayerCooldown
    traverse_ reactInputEvent es
    applyMotion
    wrapSpace
    collidePlayer
    collideBullets
    ageBullets
    runGC
    ask

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
  ) => SystemT w m ()
collidePlayer = do
  collided <- playerCollide
  when collided $ do
    killPlayer
    void $ spawnPlayer

collideBullets :: (MonadIO m
  , Has w m Bullet
  , Has w m Asteroid
  , Has w m Position
  , Has w m Mass
  , Has w m Rotation
  , Has w m Velocity
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m EntityCounter
  ) => SystemT w m ()
collideBullets = do
  cmapM_ $ \(Bullet _, Position bpos, e) -> do
    collided <- bulletCollide bpos
    case collided of
      Nothing -> pure ()
      Just ae -> do
        breakAsteroid ae
        destroyBullet e
