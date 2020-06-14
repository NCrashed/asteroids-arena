module Game.Asteroids.World(
    World(..)
  , initWorld
  , simulateWorld
  ) where

import Apecs
import Control.Concurrent.STM
import Control.Monad

import Game.Asteroids.World.Mass
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Velocity

data World = World {
  worldWidth    :: !Int
, worldHeight   :: !Int
, worldMass     :: !(Storage Mass)
, worldPlayer   :: !(Storage Player)
, worldPosition :: !(Storage Position)
, worldVelocity :: !(Storage Velocity)
, worldEntities :: !(Storage EntityCounter)
}

initWorld :: IO World
initWorld = do
  worldMass <- explInit
  worldPlayer <- explInit
  worldPosition <- explInit
  worldVelocity <- explInit
  worldEntities <- explInit
  pure World {
      worldWidth  = 1400
    , worldHeight = 1024
    , ..
    }

simulateWorld :: TVar World -> IO ()
simulateWorld worldRef = forever $ do
  w <- atomically . readTVar $ worldRef
  w' <- stepWorld w
  atomically $ writeTVar worldRef w'

stepWorld :: World -> IO World
stepWorld = pure

instance Has World IO Mass where
  getStore = asks worldMass
  {-# INLINE getStore #-}

instance Has World IO Player where
  getStore = asks worldPlayer
  {-# INLINE getStore #-}

instance Has World IO Position where
  getStore = asks worldPosition
  {-# INLINE getStore #-}

instance Has World IO Velocity where
  getStore = asks worldVelocity
  {-# INLINE getStore #-}
