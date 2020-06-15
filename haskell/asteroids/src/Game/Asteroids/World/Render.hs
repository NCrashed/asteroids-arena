{-# LANGUAGE OverloadedLists #-}
module Game.Asteroids.World.Render(
  ) where

import Game.Asteroids.Render
import Game.Asteroids.World
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Linear
import Control.Monad.IO.Class
import SDL
import Apecs as A

import qualified Data.Vector.Storable as V

instance WorldRender World where
  getRenderSize w = runWith w $ fmap (fmap ceiling) getWorldSize

instance CanRender World World where
  render r _ = do
    cmapM_ (renderPlayer r)
    pure ()
  {-# INLINE render #-}

renderPlayer :: MonadIO m => Renderer -> (Player, Position, Rotation) -> m ()
renderPlayer rd (_, Position p, Rotation r) = do
  let V2 dx dy = playerSize * 0.5
  rendererDrawColor rd SDL.$= 255
  drawLines rd $ V.map (P . fmap round . (p +) . rotateV2 r) [
      V2 dx 0
    , V2 (-dx) (-dy)
    , V2 (-dx) dy
    , V2 dx 0 ]
{-# INLINE renderPlayer #-}

rotateV2 :: Float -> V2 Float -> V2 Float
rotateV2 a (V2 x y) = V2 (x * cos a - y * sin a) (x * sin a + y * cos a)
{-# INLINE rotateV2 #-}
