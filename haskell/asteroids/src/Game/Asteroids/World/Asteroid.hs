module Game.Asteroids.World.Asteroid(
    Asteroid(..)
  , spawnAsteroids
  ) where

import Apecs
import Control.Monad
import Control.Monad.IO.Class
import Game.Asteroids.World.Mass
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Game.Asteroids.World.Velocity
import Linear
import System.Random

-- | Asteroid is encoded with it amount of edges and radius
data Asteroid = Asteroid !Int !Float

instance Component Asteroid
  where type Storage Asteroid = Map Asteroid

-- | Asteroid possible amount of edges in generation
asteroidEdgesRange :: (Int, Int)
asteroidEdgesRange = (8, 20)

-- | Asteroid possible size in pixels (1 px == 1 meter)
asteroidSizeRange :: (Float, Float)
asteroidSizeRange = (10, 130)

-- | Asteroid mass density per square pixel
asteroidDensity :: Float
asteroidDensity = 1

-- | Amount of asteroids to spawn
asteroidsAmount :: Int
asteroidsAmount = 20

-- | Asteroid possible velocities
asteroidVelocityRange :: (Float, Float)
asteroidVelocityRange = (0, 100)

-- | Spawn random asteroids
spawnAsteroids :: (MonadIO m
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m Asteroid
  , Has w m Mass
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  , Has w m EntityCounter
  ) => SystemT w m [Entity]
spawnAsteroids = replicateM asteroidsAmount spawnAsteroid

spawnAsteroid :: (MonadIO m
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m Asteroid
  , Has w m Mass
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  , Has w m EntityCounter
  ) => SystemT w m Entity
spawnAsteroid = do
  V2 w h <- getWorldSize
  n <- liftIO $ randomRIO asteroidEdgesRange
  x <- liftIO $ randomRIO (0, w)
  y <- liftIO $ randomRIO (0, h)
  vx <- liftIO $ randomRIO asteroidVelocityRange
  vy <- liftIO $ randomRIO asteroidVelocityRange
  r <- liftIO $ randomRIO asteroidSizeRange
  let m = pi * r * r * asteroidDensity
  a <- liftIO $ randomRIO (0, 2*pi)
  newEntity (Asteroid n r, Mass m, Position (V2 x y), Rotation a, Velocity (V2 vx vy))
