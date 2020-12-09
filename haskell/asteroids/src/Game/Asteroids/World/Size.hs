module Game.Asteroids.World.Size(
    WorldWidth(..)
  , WorldHeight(..)
  , initSizes
  , getWorldSize
  ) where

import Apecs
import Data.Default
import Data.Mutable
import GHC.Generics
import Control.Monad.IO.Class
import Linear

newtype WorldWidth = WorldWidth { unWorldWidth :: Float } deriving (Show, Num, Generic)

instance Default WorldWidth where
  def = 1496

instance Semigroup WorldWidth where
  _ <> a = a

instance Monoid WorldWidth where
  mempty = def

instance Mutable s WorldWidth where
  type Ref s WorldWidth = GRef s WorldWidth

instance Component WorldWidth where
  type Storage WorldWidth = Global WorldWidth

newtype WorldHeight = WorldHeight { unWorldHeight :: Float } deriving (Show, Num, Generic)

instance Default WorldHeight where
  def = 1024

instance Semigroup WorldHeight where
  _ <> a = a

instance Monoid WorldHeight where
  mempty = def

instance Mutable s WorldHeight where
  type Ref s WorldHeight = GRef s WorldHeight

instance Component WorldHeight where
  type Storage WorldHeight = Global WorldHeight

initSizes :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m ()
initSizes = do
  set global (def :: WorldWidth)
  set global (def :: WorldHeight)

getWorldSize :: (MonadIO m, Has w m WorldWidth, Has w m WorldHeight) => SystemT w m (V2 Float)
getWorldSize = V2
  <$> (maybe 0 unWorldWidth <$> get global)
  <*> (maybe 0 unWorldHeight <$> get global)
