module Game.Asteroids.World.Rotation(
    Rotation(..)
  ) where

import Kecsik
import Linear

-- | Rotation in radians
newtype Rotation = Rotation { unRotation :: Float } deriving (Show, Num, Generic)

instance Mutable s Rotation where
  type Ref s Rotation = GRef s Rotation

instance Component s Rotation where
  type Storage s Rotation = HashTable s Rotation
