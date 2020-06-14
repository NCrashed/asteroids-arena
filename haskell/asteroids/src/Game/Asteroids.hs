module Game.Asteroids(
    runGame
  ) where

import Control.Concurrent
import Data.IORef
import Game.Asteroids.Window
import Game.Asteroids.World
import Game.Asteroids.World.Render

runGame :: IO ()
runGame = do
  w <- newWorld
  renderLoop w simulateWorld
