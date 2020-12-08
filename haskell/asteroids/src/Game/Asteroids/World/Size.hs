module Game.Asteroids.World.Size(
    WorldWidth(..)
  , WorldHeight(..)
  , initSizes
  , getWorldSize
  ) where

import Kecsik
import Control.Monad.IO.Class
import Linear

newtype WorldWidth = WorldWidth { unWorldWidth :: Float } deriving (Show, Num, Generic)

instance Default WorldWidth where
  def = 1496

instance Mutable s WorldWidth where
  type Ref s WorldWidth = GRef s WorldWidth

instance Component s WorldWidth where
  type Storage s WorldWidth = Global s WorldWidth

newtype WorldHeight = WorldHeight { unWorldHeight :: Float } deriving (Show, Num, Generic)

instance Default WorldHeight where
  def = 1024

instance Mutable s WorldHeight where
  type Ref s WorldHeight = GRef s WorldHeight

instance Component s WorldHeight where
  type Storage s WorldHeight = Global s WorldHeight

initSizes :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m ()
initSizes = do
  set global (def :: WorldWidth)
  set global (def :: WorldHeight)

getWorldSize :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m (V2 Float)
getWorldSize = V2
  <$> (maybe 0 unWorldWidth <$> get global)
  <*> (maybe 0 unWorldHeight <$> get global)
