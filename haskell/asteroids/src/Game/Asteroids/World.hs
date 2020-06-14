{-# LANGUAGE TemplateHaskell #-}
module Game.Asteroids.World(
    World(..)
  , newWorld
  , simulateWorld
  ) where

import Apecs
import Control.Concurrent.STM
import Control.Monad

import Game.Asteroids.World.Mass
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Game.Asteroids.World.Velocity

makeWorld "World" [
    ''WorldWidth
  , ''WorldHeight
  , ''Mass
  , ''Player
  , ''Position
  , ''Rotation
  , ''Velocity
  ]

newWorld :: IO World
newWorld = do
  w <- initWorld
  runWith w $ do
    initSizes
    spawnPlayer
    ask

simulateWorld :: TVar World -> IO ()
simulateWorld worldRef = forever $ do
  w <- atomically . readTVar $ worldRef
  w' <- stepWorld w
  atomically $ writeTVar worldRef w'

stepWorld :: World -> IO World
stepWorld = pure
