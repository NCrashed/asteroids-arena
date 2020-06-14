module Game.Asteroids.World.Physics(
    applyMotion
  ) where

import Apecs
import Control.Monad.IO.Class
import Linear

import Game.Asteroids.World.Position
import Game.Asteroids.World.Velocity
import Game.Asteroids.World.Timer

import Debug.Trace

applyMotion :: (MonadIO m
  , Has w m Position
  , Has w m Velocity
  , Has w m Timer
  ) => SystemT w m ()
applyMotion = do
  dt <- getDelta
  -- traceShowM dt
  cmapM_ $ \(Position p, Velocity v, e) -> set e $ Position (p + v * V2 dt dt)
