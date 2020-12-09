module Game.Asteroids.Render(
    WorldRender(..)
  , CanRender(..)
  ) where

import Apecs
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL

class (MonadIO m, CanRender w m w) => WorldRender m w where
  getRenderSize :: w -> m (V2 CInt)

class MonadIO m => CanRender w m a where
  render :: Renderer -> a -> SystemT w m ()
