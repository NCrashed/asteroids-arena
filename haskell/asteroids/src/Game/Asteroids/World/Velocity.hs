module Game.Asteroids.World.Velocity(
    Velocity(..)
  ) where

import Apecs
import Data.Mutable
import Game.Asteroids.Mutable
import Game.Asteroids.Storage.Judy
import GHC.Generics
import Linear

newtype Velocity = Velocity { unVelocity :: V2 Float } deriving (Show, Num, Generic)

instance Mutable s Velocity where
  type Ref s Velocity = GRef s Velocity

instance Component Velocity where
  type Storage Velocity = Judy 1000 Velocity
