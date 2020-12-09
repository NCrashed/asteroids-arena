module Game.Asteroids.World.Mass(
    Mass(..)
  ) where

import Apecs
import Data.Mutable
import Game.Asteroids.Storage.Judy
import GHC.Generics

newtype Mass = Mass { unMass :: Float } deriving (Show, Num, Generic)

instance Mutable s Mass where
  type Ref s Mass = GRef s Mass

instance Component Mass where
  type Storage Mass = Judy 1000 Mass
