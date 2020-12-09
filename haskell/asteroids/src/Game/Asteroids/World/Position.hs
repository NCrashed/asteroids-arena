module Game.Asteroids.World.Position(
    Position(..)
  ) where

import Apecs
import Data.Mutable
import Game.Asteroids.Mutable
import GHC.Generics
import Linear

newtype Position = Position { unPosition :: V2 Float } deriving (Show, Num, Generic)

instance Mutable s Position where
  type Ref s Position = GRef s Position

instance Component Position where
  type Storage Position = Cache 500 (Map Position)
