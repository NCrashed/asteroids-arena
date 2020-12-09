{-# LANGUAGE OverloadedLists #-}
module Game.Asteroids.World.Render(
  ) where

import Apecs as A
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import Foreign.C.Types
import Game.Asteroids.Render
import Game.Asteroids.System
import Game.Asteroids.Vector
import Game.Asteroids.World
import Game.Asteroids.World.Asteroid
import Game.Asteroids.World.Bullet
import Game.Asteroids.World.Player
import Game.Asteroids.World.Position
import Game.Asteroids.World.Rotation
import Game.Asteroids.World.Size
import Linear
import SDL

import qualified Data.Vector.Storable as V

instance MonadIO m => WorldRender m World where
  getRenderSize w = runWith w $ fmap (fmap ceiling) getWorldSize

instance MonadIO m => CanRender World m World where
  render r _ = cmapM_ $ \(Position{}, e) -> do
    renderPlayer r e
    renderAsteroid r e
    renderBullet r e
  {-# INLINE render #-}

renderPlayer :: MonadIO m => Renderer -> Entity -> SystemT World m ()
renderPlayer rd e = withCM_ e $ \(Player isThrust _, Position p, Rotation r) -> do
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

renderAsteroid :: MonadIO m => Renderer -> Entity -> SystemT World m ()
renderAsteroid rd e = withCM_ e $ \(Asteroid n r, Position p, Rotation a) -> do
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

renderBullet :: MonadIO m => Renderer -> Entity -> SystemT World m ()
renderBullet rd e = withCM_ e $ \(Bullet _, Position p) -> do
  rendererDrawColor rd SDL.$= 255
  drawPoint rd (P $ fmap round p)
{-# INLINE renderBullet #-}
