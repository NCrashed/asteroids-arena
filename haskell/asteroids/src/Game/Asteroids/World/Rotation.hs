module Game.Asteroids.World.Rotation(
    Rotation(..)
  ) where

import Apecs
import Data.Mutable
import GHC.Generics
import Linear

-- | Rotation in radians
newtype Rotation = Rotation { unRotation :: Float } deriving (Show, Num, Generic)

instance Mutable s Rotation where
  type Ref s Rotation = GRef s Rotation

instance Component Rotation where
  type Storage Rotation = Cache 1000 (Map Rotation)
