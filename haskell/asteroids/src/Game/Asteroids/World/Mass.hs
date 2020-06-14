module Game.Asteroids.World.Mass(
    Mass(..)
  ) where

import Apecs
import Linear

newtype Mass = Mass { unMass :: Float } deriving (Show, Num)

instance Component Mass where
  type Storage Mass = Map Mass
