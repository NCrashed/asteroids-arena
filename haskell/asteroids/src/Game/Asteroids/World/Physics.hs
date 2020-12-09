{-# LANGUAGE MultiWayIf #-}
module Game.Asteroids.World.Physics(
    applyMotion
  , wrapSpace
  , playerCollide
  , bulletCollide
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Apecs
import Apecs.Core
import Linear

import Game.Asteroids.System
import Game.Asteroids.World.Asteroid
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Size
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity

import qualified Data.Vector.Unboxed as U

applyMotion :: forall w m . (MonadIO m
  , Has w m Position
  , Has w m Velocity
  , Has w m Timer
  ) => Entity -> SystemT w m ()
applyMotion e = do
  dt <- getDelta
  modify e $ \(Position p, Velocity v) -> Position (p + v * V2 dt dt)
{-# INLINE applyMotion #-}

wrapSpace :: forall w m . (MonadIO m
  , Has w m Position
  , Has w m WorldWidth
  , Has w m WorldHeight
  ) => Entity -> SystemT w m ()
wrapSpace e = do
  V2 w h <- getWorldSize
  modifyMay e $ \(Position (V2 x y)) -> if
    | x < 0 && y < 0 -> Just $ Position (V2 (w+x) (h+y))
    | x < 0          -> Just $ Position (V2 (w+x) y)
    | y < 0          -> Just $ Position (V2 x (h+y))
    | x > w && y > h -> Just $ Position (V2 (x-w) (y-h))
    | x > w          -> Just $ Position (V2 (x-w) y)
    | y > h          -> Just $ Position (V2 x (y-h))
    | otherwise      -> Nothing

-- | Check collision of player with any of asteroids
playerCollide :: (MonadIO m
  , Has w m Player
  , Has w m Asteroid
  , Has w m Position
  ) => Entity -> SystemT w m Bool
playerCollide e = fmap (fromMaybe False) $ withCM e $ \(Player{}, Position pos) -> do
  let checkAsteroid True _ = True
      checkAsteroid _ (Asteroid _ r, Position apos) = let
        r' = r+playerCollideRadius
        in quadrance (pos - apos) <= r'*r'
  cfold checkAsteroid False

-- | Check collision of bullet with any of asteroids
bulletCollide :: forall w m . (MonadIO m
  , Has w m Asteroid
  , Has w m Position
  ) => V2 Float -> SystemT w m (Maybe Entity)
bulletCollide pos = do
  as :: Storage Asteroid <- getStore
  is <- {-# SCC "asteroidMembers" #-} explMembers as
  go is
  where
    go !is
      | U.null is = pure Nothing
      | otherwise = do
        let i = U.head is
        (Asteroid _ r, Position apos) <- {-# SCC "getAsteroid" #-} get (Entity i)
        if quadrance (pos - apos) <= r*r
          then pure . Just $! Entity i
          else go $ U.drop 1 is
