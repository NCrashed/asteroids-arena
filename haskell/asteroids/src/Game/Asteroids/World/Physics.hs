{-# LANGUAGE MultiWayIf #-}
module Game.Asteroids.World.Physics(
    applyMotion
  , wrapSpace
  ) where

import Apecs
import Control.Monad.IO.Class
import Linear

import Game.Asteroids.World.Position
import Game.Asteroids.World.Size
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity

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
{-# INLINE applyMotion #-}

wrapSpace :: (MonadIO m
  , Has w m Position
  , Has w m WorldWidth
  , Has w m WorldHeight
  ) => SystemT w m ()
wrapSpace = do
  V2 w h <- getWorldSize
  cmapM_ $ \(Position (V2 x y), e) -> if
    | x < 0 && y < 0 -> set e $ Position (V2 (w+x) (h+y))
    | x < 0          -> set e $ Position (V2 (w+x) y)
    | x > w && y > h -> set e $ Position (V2 (x-w) (y-h))
    | x > w          -> set e $ Position (V2 (x-w) y)
    | otherwise      -> pure ()
