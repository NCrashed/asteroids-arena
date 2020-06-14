module Game.Asteroids.World.Size(
    WorldWidth(..)
  , WorldHeight(..)
  , initSizes
  , getWorldSize
  ) where

import Apecs
import Control.Monad.IO.Class
import Linear

newtype WorldWidth = WorldWidth { unWorldWidth :: Float } deriving (Show, Num)

instance Component WorldWidth where
  type Storage WorldWidth = Unique WorldWidth

newtype WorldHeight = WorldHeight { unWorldHeight :: Float } deriving (Show, Num)

instance Component WorldHeight where
  type Storage WorldHeight = Unique WorldHeight

initSizes :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m ()
initSizes = do
  set global $ WorldWidth 1496
  set global $ WorldHeight 1024

getWorldSize :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m (V2 Float)
getWorldSize = V2
  <$> (fmap unWorldWidth $ get global)
  <*> (fmap unWorldHeight $ get global)
