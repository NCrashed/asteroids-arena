{-# LANGUAGE OverloadedLists #-}
module Game.Asteroids.World.Render(
  ) where

import Apecs as A
import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types
import Game.Asteroids.Render
import Game.Asteroids.World
import Game.Asteroids.World.Asteroid
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Linear
import SDL

import qualified Data.Vector.Storable as V

instance WorldRender World where
  getRenderSize w = runWith w $ fmap (fmap ceiling) getWorldSize

instance CanRender World World where
  render r _ = do
    cmapM_ (renderPlayer r)
    cmapM_ (renderAsteroid r)
    pure ()
  {-# INLINE render #-}

renderPlayer :: MonadIO m => Renderer -> (Player, Position, Rotation) -> m ()
renderPlayer rd (Player isThrust, Position p, Rotation r) = do
  let V2 dx dy = playerSize * 0.5
  rendererDrawColor rd SDL.$= 255
  let mkPoints = V.map (P . fmap round . (p +) . rotateV2 r)
  drawLines rd $ mkPoints [
      V2 dx 0
    , V2 (-dx) (-dy)
    , V2 (-dx) dy
    , V2 dx 0 ]
  when isThrust $ drawLines rd $ mkPoints [
      V2 (-dx) (0.5 * dy)
    , V2 (-dx-10) 0
    , V2 (-dx) (-0.5 * dy)
    ]
{-# INLINE renderPlayer #-}

renderAsteroid :: MonadIO m => Renderer -> (Asteroid, Position, Rotation) -> m ()
renderAsteroid rd (Asteroid n r, Position p, Rotation a) = do
  rendererDrawColor rd SDL.$= 255
  drawCircloid rd n a (fmap round p) (round r)
{-# INLINE renderAsteroid #-}

drawCircloid :: MonadIO m => Renderer -> Int -> Float -> V2 CInt -> CInt -> m ()
drawCircloid rd n a0 (V2 x0 y0) r = drawLines rd $ V.fromList [
    P $ V2 (x0 + x) (y0 + y)
  | i <- [0 .. n]
  , let a = fromIntegral i * (2 * pi / fromIntegral n)
  , let x = round $ fromIntegral r * (1 + sin a * 0.3) * cos (a0 + a)
  , let y = round $ fromIntegral r * (1 + cos a * 0.3) * sin (a0 + a)
  ]
{-# INLINE drawCircloid #-}

rotateV2 :: Float -> V2 Float -> V2 Float
rotateV2 a (V2 x y) = V2 (x * cos a - y * sin a) (x * sin a + y * cos a)
{-# INLINE rotateV2 #-}
