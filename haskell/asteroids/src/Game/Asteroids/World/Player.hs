module Game.Asteroids.World.Player(
    Player(..)
  ) where

import Apecs

data Player = Player

instance Component Player
  where type Storage Player = Unique Player
