module Game.Asteroids(
    runGame
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Game.Asteroids.Window
import Game.Asteroids.World
import Game.Asteroids.World.Render

runGame :: IO ()
runGame = do
  w <- newWorld
  wRef <- newTVarIO w
  _ <- forkIO $ simulateWorld wRef
  renderLoop wRef
