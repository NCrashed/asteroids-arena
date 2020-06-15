{-# LANGUAGE TemplateHaskell #-}
module Game.Asteroids.World(
    World(..)
  , newWorld
  , simulateWorld
  ) where

import Apecs
import Control.Monad
import Data.Foldable (traverse_)
import Data.IORef
import System.Clock

import Game.Asteroids.World.Asteroid
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
  , ''WorldHeight
  , ''Asteroid
  , ''Mass
  , ''Player
  , ''Position
  , ''Rotation
  , ''Timer
  , ''Velocity
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
    traverse_ reactInputEvent es
    applyMotion
    wrapSpace
    runGC
    ask
