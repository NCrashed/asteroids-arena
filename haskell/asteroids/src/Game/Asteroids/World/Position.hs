module Game.Asteroids.World.Position(
    Position(..)
  ) where

import Apecs
import Data.Mutable
import Game.Asteroids.Mutable
import Game.Asteroids.Storage.Judy
import GHC.Generics
import Linear

newtype Position = Position { unPosition :: V2 Float } deriving (Show, Num, Generic)

instance Mutable s Position where
  type Ref s Position = GRef s Position

instance Component Position where
  type Storage Position = Judy 1000 Position
