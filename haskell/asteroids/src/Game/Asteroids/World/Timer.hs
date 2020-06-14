module Game.Asteroids.World.Timer(
    Timer(..)
  , initTimer
  , getDelta
  , setDelta
  ) where

import Apecs
import Control.Monad.IO.Class
import System.Clock

-- | Holds time passed since previous tick
data Timer = Timer {
    timerOld   :: !TimeSpec
  , timerDelta :: !Float
  } deriving (Show)

instance Component Timer where
  type Storage Timer = Unique Timer

initTimer :: (MonadIO m, Has w m Timer) => SystemT w m ()
initTimer = do
  t0 <- liftIO $ getTime Monotonic
  set global $ Timer t0 0

getDelta :: (MonadIO m, Has w m Timer) => SystemT w m Float
getDelta = fmap timerDelta $ get global

deltaTime :: TimeSpec -> TimeSpec -> Float
deltaTime t1 t2 = (/ 1000000000) $ fromIntegral $ toNanoSecs $ diffTimeSpec t1 t2

setDelta :: (MonadIO m, Has w m Timer) => TimeSpec -> SystemT w m ()
setDelta t2 = modify global $ \(Timer t1 _) -> Timer t2 (deltaTime t2 t1)
