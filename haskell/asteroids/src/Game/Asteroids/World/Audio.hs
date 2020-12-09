module Game.Asteroids.World.Audio(
    AudioResources(..)
  , AudioState(..)
  , setupAudioResources
  , playSound
  , updateAudioCooldowns
  ) where

import Apecs
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable (foldr')
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Mutable
import Game.Asteroids.Audio
import GHC.Generics
import Game.Asteroids.System
import Game.Asteroids.World.Timer
import SDL.Mixer

import qualified Data.IntMap.Strict as IM

data AudioState = AudioState {
  audioResources :: !(Maybe AudioResources)
, audioDelays    :: !(IntMap Float)
} deriving (Generic)

instance Default AudioState where
  def = AudioState Nothing IM.empty

instance Semigroup AudioState where
  _ <> a = a

instance Monoid AudioState where
  mempty = def

instance Mutable s AudioState

instance Component AudioState where
  type Storage AudioState = Global AudioState

setupAudioResources :: (MonadIO m
  , Has w m AudioState
  ) => AudioResources -> SystemT w m ()
setupAudioResources r = set global $ AudioState (Just r) IM.empty

playSound :: (MonadIO m
  , Get w m AudioState
  ) => (AudioResources -> Chunk) -- ^ Which sound to play
  -> Channel
  -> Volume
  -> Float
  -> SystemT w m ()
playSound sound ch v cd = withCM_ global $ \(AudioState mas _) -> case mas of
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
isAudioCooldown ch = fmap (fromMaybe False) $ withC global $ \(AudioState _ dm) ->
  maybe False (> 0) $ IM.lookup (fromIntegral ch) dm

setAudioCooldown :: (MonadIO m
  , Has w m AudioState
  ) => Channel -> Float -> SystemT w m ()
setAudioCooldown ch v = modify global $ \(AudioState r dm) -> AudioState r $ IM.insert (fromIntegral ch) v dm

updateAudioCooldowns :: (MonadIO m
  , Has w m AudioState
  , Has w m Timer
  ) => SystemT w m ()
updateAudioCooldowns = do
  dt <- getDelta
  let f v = if v - dt < 0 then Nothing else Just (v - dt)
  modify global $ \(AudioState r dm) -> AudioState r $ foldr' (IM.update f) dm $ IM.keys dm
