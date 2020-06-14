module Game.Asteroids.Render(
    WorldRender(..)
  ) where

import SDL

class WorldRender world where
  worldRenderHeight :: world -> Int
  worldRenderWidth  :: world -> Int
  worldRender ::  Renderer -> world -> IO ()
