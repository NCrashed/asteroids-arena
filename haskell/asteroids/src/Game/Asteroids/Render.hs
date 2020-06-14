module Game.Asteroids.Render(
    WorldRender(..)
  , CanRender(..)
  ) where

import Apecs
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL

class CanRender world world => WorldRender world where
  getRenderSize :: MonadIO m => world -> m (V2 CInt)

class CanRender w a where
  render :: MonadIO m => Renderer -> a -> SystemT w m ()
