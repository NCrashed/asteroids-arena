{-# LANGUAGE TemplateHaskell #-}
module Game.Asteroids.World(
    World(..)
  , newWorld
  , simulateWorld
  ) where

import Apecs
import Data.IORef
import Control.Monad
import System.Clock

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
    spawnPlayer
    ask

simulateWorld :: World -> IO World
simulateWorld w = do
  t <- getTime Monotonic
  runWith w $ do
    setDelta t
    applyMotion
    ask
