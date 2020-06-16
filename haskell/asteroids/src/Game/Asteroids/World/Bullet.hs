module Game.Asteroids.World.Bullet(
    Bullet(..)
  , bulletSpeed
  , spawnBullet
  , ageBullets
  , destroyBullet
  ) where

import Apecs
import Control.Monad.IO.Class
import Game.Asteroids.World.Mass
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity
import Linear

-- | Bullet contains duration of life remains for that bullet
data Bullet = Bullet !Float

instance Component Bullet
  where type Storage Bullet = Map Bullet

-- | Bullet speed m per second
bulletSpeed :: Float
bulletSpeed = 200

-- | Amount of seconds bullet lives
bulletLifeTime :: Float
bulletLifeTime = 3

spawnBullet :: (MonadIO m
  , Has w m Bullet
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  , Has w m EntityCounter
  ) => V2 Float -- ^ Position
  -> V2 Float -- ^ Velocity
  -> SystemT w m Entity
spawnBullet p v = newEntity (Bullet bulletLifeTime, Position p, Velocity v)

-- | Destroy bullet entity
destroyBullet :: (MonadIO m
  , Has w m Bullet
  , Has w m Position
  , Has w m Velocity
  ) => Entity -> SystemT w m ()
destroyBullet e = destroy e (Proxy :: Proxy (Bullet, Position, Velocity))

-- | Updates life time of bullets and remove bullets that are exceeded their life times
ageBullets :: (MonadIO m
  , Has w m Bullet
  , Has w m Position
  , Has w m Velocity
  , Has w m Timer
  ) => SystemT w m ()
ageBullets = do
  dt <- getDelta
  cmapM_ $ \(Bullet l, e) -> do
    let l' = l - dt
    if(l' <= 0) then destroyBullet e
      else set e (Bullet l')
