{-# LANGUAGE MultiWayIf #-}
module Game.Asteroids.World.Physics(
    applyMotion
  , wrapSpace
  , playerCollide
  , bulletCollide
  ) where

import Apecs
import Control.Monad.IO.Class
import Linear

import Game.Asteroids.World.Asteroid
import Game.Asteroids.World.Player
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
    | y < 0          -> set e $ Position (V2 x (h+y))
    | x > w && y > h -> set e $ Position (V2 (x-w) (y-h))
    | x > w          -> set e $ Position (V2 (x-w) y)
    | y > h          -> set e $ Position (V2 x (y-h))
    | otherwise      -> pure ()

-- | Check collision of player with any of asteroids
playerCollide :: (MonadIO m
  , Has w m Player
  , Has w m Asteroid
  , Has w m Position
  ) => SystemT w m Bool
playerCollide = do
  pos <- cfold (\_ (Player _ _, Position pos) -> pos) 0
  let checkAsteroid True _ = True
      checkAsteroid _ (Asteroid _ r, Position apos) = let
        r' = r+playerCollideRadius
        in quadrance (pos - apos) <= r'*r'
  cfold checkAsteroid False

-- | Check collision of player with any of asteroids
bulletCollide :: (MonadIO m
  , Has w m Asteroid
  , Has w m Position
  ) => V2 Float -> SystemT w m (Maybe Entity)
bulletCollide pos = do
  let checkAsteroid (Just a) _ = Just a
      checkAsteroid _ (Asteroid _ r, Position apos, e) = if quadrance (pos - apos) <= r*r
        then Just e
        else Nothing
  cfold checkAsteroid Nothing
