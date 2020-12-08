module Game.Asteroids.World.Audio(
    AudioResources(..)
  , AudioState(..)
  , setupAudioResources
  , playSound
  , updateAudioCooldowns
  ) where

import Kecsik
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (foldr')
import Data.IntMap (IntMap)
import Game.Asteroids.Audio
import Game.Asteroids.World.Timer
import SDL.Mixer

import qualified Data.IntMap.Strict as IM

data AudioState = AudioState {
  audioResources :: !(Maybe AudioResources)
, audioDelays    :: !(IntMap Float)
}

instance Default AudioState where
  def = AudioState Nothing IM.empty

instance Mutable s AudioState

instance Component s AudioState where
  type Storage s AudioState = Global s AudioState

setupAudioResources :: (MonadIO m
  , Has w m AudioState
  ) => AudioResources -> SystemT w m ()
setupAudioResources r = set global $ AudioState (Just r) IM.empty

playSound :: (MonadIO m
  , Has w m AudioState
  ) => (AudioResources -> Chunk) -- ^ Which sound to play
  -> Channel
  -> Volume
  -> Float
  -> SystemT w m ()
playSound sound ch v cd = cmapM_ $ \(AudioState mas _) -> case mas of
  Nothing -> pure ()
  Just as -> do
    setVolume v $ sound as
    cooldown <- isAudioCooldown ch
    unless cooldown $ do
      setAudioCooldown ch cd
      void $ playOn ch 1 $ sound as

isAudioCooldown :: (MonadIO m
  , Has w m AudioState
  ) => Channel -> SystemT w m Bool
isAudioCooldown ch = cfold f False
  where
    f True _ = True
    f _ (AudioState _ dm) = case IM.lookup (fromIntegral ch) dm of
      Nothing -> False
      Just v -> v > 0

setAudioCooldown :: (MonadIO m
  , Has w m AudioState
  ) => Channel -> Float -> SystemT w m ()
setAudioCooldown ch v = cmap $ \(AudioState r dm) -> AudioState r $ IM.insert (fromIntegral ch) v dm

updateAudioCooldowns :: (MonadIO m
  , Has w m AudioState
  , Has w m Timer
  ) => SystemT w m ()
updateAudioCooldowns = do
  dt <- getDelta
  let f v = if v - dt < 0 then Nothing else Just (v - dt)
  cmap $ \(AudioState r dm) -> AudioState r $ foldr' (IM.update f) dm $ IM.keys dm
