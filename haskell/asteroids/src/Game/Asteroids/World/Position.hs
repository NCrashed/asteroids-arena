module Game.Asteroids.World.Position(
    Position(..)
  ) where

import Kecsik
import Linear
import Game.Asteroids.Mutable

newtype Position = Position { unPosition :: V2 Float } deriving (Show, Num, Generic)

instance Mutable s Position where
  type Ref s Position = GRef s Position

instance Component s Position where
  type Storage s Position = HashTable s Position
