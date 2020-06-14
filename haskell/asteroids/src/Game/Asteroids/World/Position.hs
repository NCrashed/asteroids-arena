module Game.Asteroids.World.Position(
    Position(..)
  ) where

import Apecs
import Linear

newtype Position = Position { unPosition :: V2 Float } deriving (Show, Num)

instance Component Position where
  type Storage Position = Map Position
