module Game.Asteroids.World.Event(
    InputEvent(..)
  , reactInputEvent
  ) where

import Kecsik
import Control.Monad.IO.Class
import Game.Asteroids.World.Audio
import Game.Asteroids.World.Bullet
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Timer
import Game.Asteroids.World.Velocity

-- | Possible inputs into simulation from player
data InputEvent =
    InputRotateLeft
  | InputRotateRight
  | InputThrust
  | InputFire
  deriving (Show, Eq)

reactInputEvent :: (MonadIO m
  , Has w m AudioState
  , Has w m Player
  , Has w m Rotation
  , Has w m Position
  , Has w m Velocity
  , Has w m Bullet
  , Has w m EntityCounter
  , Has w m Timer
  ) => InputEvent -> SystemT w m ()
reactInputEvent e = do
  dt <- getDelta
  case e of
    InputRotateLeft -> rotatePlayer $ negate $ dt * playerRotateSpeed
    InputRotateRight -> rotatePlayer $ dt * playerRotateSpeed
    InputThrust -> do
      playSound audioThrust 0 50 0.35
      addPlayerVelocity $ dt * playerThrust / playerMass
      setPlayerThursting True
    InputFire -> do
      playSound audioFire 1 50 0.2
      playerFire
