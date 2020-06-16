module Game.Asteroids.Audio(
    AudioResources(..)
  , loadAudioResources
  ) where

import SDL.Mixer
import Control.Monad.IO.Class

-- | Loaded audio files
data AudioResources = AudioResources {
  audioBang   :: !Chunk
, audioFire   :: !Chunk
, audioThrust :: !Chunk
}

loadAudioResources :: MonadIO m => FilePath -> m AudioResources
loadAudioResources folder = AudioResources
  <$> ld "bangMedium.wav"
  <*> ld "fire.wav"
  <*> ld "thrust.wav"
  where
    ld r = load $ folder <> "/" <> r
