module Game.Asteroids.World.Player(
    Player(..)
  , playerSize
  , playerMass
  , spawnPlayer
  ) where

import Apecs
import Control.Monad.IO.Class
import Game.Asteroids.World.Mass
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Game.Asteroids.World.Velocity
import Linear

data Player = Player

instance Component Player
  where type Storage Player = Unique Player

-- | Player size in pixels (1 px == 1 meter)
playerSize :: V2 Float
playerSize = V2 30 25

-- | Player mass in kg
playerMass :: Float
playerMass = 1000

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
  newEntity (Player, Mass playerMass, Position (ws * 0.5), Rotation 0, Velocity 0)
