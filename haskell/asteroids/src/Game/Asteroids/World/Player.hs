module Game.Asteroids.World.Player(
    Player(..)
  , playerSize
  , playerCollideRadius
  , playerMass
  , playerThrust
  , playerRotateSpeed
  , spawnPlayer
  , rotatePlayer
  , addPlayerVelocity
  , killPlayer
  , setPlayerThursting
  , updatePlayerCooldown
  , playerFire
  ) where

import Kecsik
import Control.Monad
import Control.Monad.IO.Class
import Game.Asteroids.Vector
import Game.Asteroids.World.Bullet
import Game.Asteroids.World.Mass
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity
import Linear

-- | Player contains flag if thrust engine is one (for rendering) and bullet
-- cooldown time.
data Player = Player !Bool !Float
  deriving (Show, Generic)

instance Default Player where
  def = Player False 0

instance Mutable s Player where
  type Ref s Player = GRef s Player

instance Component s Player where
  type Storage s Player = Unique s Player

-- | Player size in pixels (1 px == 1 meter)
playerSize :: V2 Float
playerSize = V2 30 25

-- | Collision radius for player
playerCollideRadius :: Float
playerCollideRadius = 15

-- | Player mass in kg
playerMass :: Float
playerMass = 1000

-- | Force in Newtons
playerThrust :: Float
playerThrust = 100000

-- | Rotation speed in radians per second
playerRotateSpeed :: Float
playerRotateSpeed = pi

-- | Amount of time
playerFireCooldown :: Float
playerFireCooldown = 0.3

spawnPlayer :: (MonadIO m
  , Has w m WorldWidth
  , Has w m WorldHeight
  , Has w m Player
  , Has w m Mass
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  , Has w m EntityCounter
  ) => SystemT w m Entity
spawnPlayer = do
  ws <- getWorldSize
  newEntity (Player False playerFireCooldown, Mass playerMass, Position (ws * 0.5), Rotation 0, Velocity 0)

-- | Rotate player to given amount of radians
rotatePlayer :: (MonadIO m
  , Has w m Player
  , Has w m Rotation
  ) => Float -> SystemT w m ()
rotatePlayer a = cmap $ \(Player t cd, Rotation r) -> (Player t cd, Rotation $ clamp $ r+a)
  where
    clamp a | a < 0     = 2*pi + a
            | a > 2*pi  = a - 2*pi
            | otherwise = a
    {-# INLINE clamp #-}
{-# INLINE rotatePlayer #-}

-- | Add velocity alonside player facing angle
addPlayerVelocity :: (MonadIO m
  , Has w m Player
  , Has w m Velocity
  , Has w m Rotation
  ) => Float -- ^ Velocity
  -> SystemT w m ()
addPlayerVelocity dv = cmap $ \(Player t cd, Velocity v, Rotation r) -> (Player t cd, Velocity $ v + V2 (dv * cos r) (dv * sin r))
{-# INLINE addPlayerVelocity #-}

-- | Destroy player entity
killPlayer :: forall w m . (MonadIO m
  , Has w m Player
  , Has w m Mass
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  ) => SystemT w m ()
killPlayer = cmapM_ $ \(Player _ _, Immutable e :: ImmutableEnt m) -> destroy_ e (Proxy :: Proxy (Player, Mass, Position, Rotation, Velocity))

-- | Mark engines on-off
setPlayerThursting :: (MonadIO m
  , Has w m Player
  ) => Bool -> SystemT w m ()
setPlayerThursting v = cmap $ \(Player _ ct) -> Player v ct
{-# INLINE setPlayerThursting #-}

-- | Decrease counter for cooldowns of player
updatePlayerCooldown :: (MonadIO m
  , Has w m Player
  , Has w m Timer
  ) => SystemT w m ()
updatePlayerCooldown = do
  dt <- getDelta
  cmap $ \(Player v cd) -> Player v (max 0 (cd - dt))
{-# INLINE updatePlayerCooldown #-}

-- | Spawn new bullet if cooldown is passed
playerFire :: forall w m . (MonadIO m
  , Has w m Player
  , Has w m Bullet
  , Has w m Position
  , Has w m Rotation
  , Has w m Velocity
  , Has w m EntityCounter
  ) => SystemT w m ()
playerFire = cmapM_ $ \(Player f cd, Position p, Rotation a, Velocity v, Immutable e :: ImmutableEnt m) -> when (cd == 0) $ do
  set e (Player f playerFireCooldown)
  void $ spawnBullet p (v + rotateV2 a (V2 1 0) * V2 bulletSpeed bulletSpeed)
