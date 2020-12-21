{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Game.Asteroids.World.Render(
  ) where

import Apecs as A
import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import Data.Foldable (traverse_, for_)
import Foreign
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
import SDL.Internal.Types
import Text.RawString.QQ

import qualified Data.Vector.Storable as V
import qualified Language.C.Inline as C

C.include "<math.h>"
C.include "<SDL2/SDL.h>"

C.verbatim [r|
void circloid_point(int i, int n, float r, float a0, int *x, int *y) {
  float a = (float)i * (2 * M_PI / (float)n);
  *x = (int)(r * (1 + sin(a) * 0.3) * cos(a0 + a));
  *y = (int)(r * (1 + cos(a) * 0.3) * sin(a0 + a));
}
|]

drawCircloid :: MonadIO m => Renderer -> CInt -> CFloat -> V2 CInt -> CInt -> m ()
drawCircloid (Renderer rd) n a0 (V2 x0 y0) r = {-# SCC "drawCircloid" #-} liftIO $ do
  [C.block| void {
    float r = (float)$(int r);
    float a0 = $(float a0);
    int x0 = $(int x0);
    int y0 = $(int y0);
    SDL_Renderer *renderer = (SDL_Renderer *)$(void *rd);
    int n = $(int n);
    static SDL_Point points[100];
    for(int i=0; i <= n-1; i++) {
      circloid_point(i,   n, r, a0, &points[i*2].x, &points[i*2].y);
      circloid_point(i+1, n, r, a0, &points[i*2+1].x, &points[i*2+1].y);
      points[i*2].x += x0;
      points[i*2].y += y0;
      points[i*2+1].x += x0;
      points[i*2+1].y += y0;
    }
    SDL_RenderDrawLines(renderer, (SDL_Point*)&points, n*2);
  } |]
{-# INLINE drawCircloid #-}

renderPlayer :: MonadIO m => Renderer -> Entity -> SystemT World m ()
renderPlayer rd e = {-# SCC "renderPlayer" #-} withCM_ e $ \(Player isThrust _, Position p, Rotation r) -> do
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
renderAsteroid rd e = {-# SCC "renderAsteroid" #-} withCM_ e $ \(Asteroid n r, Position p, Rotation a) -> do
  rendererDrawColor rd SDL.$= 255
  drawCircloid rd (fromIntegral n) (realToFrac a) (fmap round p) (round r)
{-# INLINE renderAsteroid #-}

renderBullet :: MonadIO m => Renderer -> Entity -> SystemT World m ()
renderBullet rd e = {-# SCC "renderBullet" #-} withCM_ e $ \(Bullet _, Position p) -> do
  rendererDrawColor rd SDL.$= 255
  drawPoint rd (P $ fmap round p)
{-# INLINE renderBullet #-}

instance MonadIO m => WorldRender m World where
  getRenderSize w = runWith w $ fmap (fmap ceiling) getWorldSize

instance MonadIO m => CanRender World m World where
  render r _ = citerate_ (Proxy @Position) $ \e -> do
    renderPlayer r e
    renderAsteroid r e
    renderBullet r e
  {-# INLINE render #-}
