module Game.Asteroids.World.Event(
    InputEvent(..)
  , reactInputEvent
  ) where

import Apecs
import Control.Monad.IO.Class
import Game.Asteroids.World.Player
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Timer

-- | Possible inputs into simulation from player
data InputEvent =
    InputRotateLeft
  | InputRotateRight
  | InputThrust
  deriving (Show, Eq)

reactInputEvent :: (MonadIO m
  , Has w m Player
  , Has w m Rotation
  , Has w m Timer
  ) => InputEvent -> SystemT w m ()
reactInputEvent e = do
  dt <- getDelta
  case e of
    InputRotateLeft -> rotatePlayer $ negate $ dt * playerRotateSpeed
    InputRotateRight -> rotatePlayer $ dt * playerRotateSpeed
    InputThrust -> pure ()
