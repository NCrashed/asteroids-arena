module Game.Asteroids.World.Velocity(
    Velocity(..)
  ) where

import Kecsik
import Linear
import Game.Asteroids.Mutable

newtype Velocity = Velocity { unVelocity :: V2 Float } deriving (Show, Num, Generic)

instance Mutable s Velocity where
  type Ref s Velocity = GRef s Velocity

instance Component s Velocity where
  type Storage s Velocity = HashTable s Velocity
