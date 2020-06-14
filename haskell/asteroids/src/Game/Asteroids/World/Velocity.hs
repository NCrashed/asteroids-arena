module Game.Asteroids.World.Velocity(
    Velocity(..)
  ) where

import Apecs
import Linear

newtype Velocity = Velocity { unVelocity :: V2 Float } deriving (Show, Num)

instance Component Velocity where
  type Storage Velocity = Map Velocity
