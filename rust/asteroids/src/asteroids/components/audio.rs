use sdl2::mixer::Chunk;
use std::path::Path;

/// Resource with all sound files loaded in memory.
pub struct AudioReses {
    pub bang: CooledChunk,
    pub fire: CooledChunk,
    pub thrust: CooledChunk,
}

/// Sound chunk with attached cooldown timer
pub struct CooledChunk {
    pub chunk: Chunk,
    pub cooldown: f32,
}

/// Event that is triggered when some system wants to play sound
#[derive(Debug)]
pub enum PlayAudio {
    BangSound,
    FireSound,
    ThrustSound,
}

impl CooledChunk {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, String> {
        Ok(CooledChunk {
            chunk: Chunk::from_file(path)?,
            cooldown: 0.0,
        })
    }
}

/// Load all wav files from given directory
pub fn load_audio_chunks(directory: &Path) -> Result<AudioReses, String> {
    Ok(AudioReses {
        bang: CooledChunk::from_file(directory.join("bangMedium.wav"))?,
        fire: CooledChunk::from_file(directory.join("fire.wav"))?,
        thrust: CooledChunk::from_file(directory.join("thrust.wav"))?,
    })
}
