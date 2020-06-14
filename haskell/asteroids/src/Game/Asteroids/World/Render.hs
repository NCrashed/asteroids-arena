module Game.Asteroids.World.Render(
  ) where

import Game.Asteroids.Render
import Game.Asteroids.World

instance WorldRender World where
  worldRenderWidth = worldWidth
  worldRenderHeight = worldHeight
  worldRender _ _ = pure ()
