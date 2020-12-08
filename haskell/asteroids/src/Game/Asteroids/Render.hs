module Game.Asteroids.Render(
    WorldRender(..)
  , CanRender(..)
  ) where

import Kecsik
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL

class (PrimMonad m, CanRender w m w) => WorldRender m w where
  getRenderSize :: w -> m (V2 CInt)

class PrimMonad m => CanRender w m a where
  render :: Renderer -> a -> SystemT w m ()
