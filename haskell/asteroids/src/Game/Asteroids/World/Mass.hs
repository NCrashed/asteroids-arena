module Game.Asteroids.World.Mass(
    Mass(..)
  ) where

import Kecsik

newtype Mass = Mass { unMass :: Float } deriving (Show, Num, Generic)

instance Mutable s Mass where
  type Ref s Mass = GRef s Mass

instance Component s Mass where
  type Storage s Mass = HashTable s Mass
