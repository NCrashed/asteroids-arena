module Game.Asteroids.Mutable() where

import Data.Mutable
import Linear

instance Mutable s a => Mutable s (V2 a) where
  type Ref s (V2 a) = GRef s (V2 a)
