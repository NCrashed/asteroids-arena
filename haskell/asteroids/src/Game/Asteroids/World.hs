module Game.Asteroids.World(
    World(..)
  , initWorld
  , simulateWorld
  ) where

import Control.Concurrent.STM
import Control.Monad

data World = World {
  worldWidth  :: !Int
, worldHeight :: !Int
}

initWorld :: IO World
initWorld = pure World {
    worldWidth  = 1400
  , worldHeight = 1024
  }

simulateWorld :: TVar World -> IO ()
simulateWorld worldRef = forever $ do
  w <- atomically . readTVar $ worldRef
  putStrLn "World step"
  w' <- stepWorld w
  atomically $ writeTVar worldRef w'

stepWorld :: World -> IO World
stepWorld = pure
