module Game.Asteroids.World.Rotation(
    Rotation(..)
  ) where

import Apecs
import Linear

-- | Rotation in radians
newtype Rotation = Rotation { unRotation :: Float } deriving (Show, Num)

instance Component Rotation where
  type Storage Rotation = Map Rotation
