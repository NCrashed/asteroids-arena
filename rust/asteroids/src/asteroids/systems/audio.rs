use sdl2::mixer::Channel;
use shrev::EventChannel;
use specs::prelude::*;
use std::path::Path;

use super::super::components::audio::*;
use super::super::components::time::*;

/// Default search path for sounds
pub const SOUNDS_PATH : &str = "./sounds";

/// Default volume level for all channels
pub const DEFAULT_VOLUME_PERCENT: i32 = 50;

/// System that play sounds and control their cooldowns.
/// The system is designed to be thread local, so use `with_thread_local` when
/// register it.
pub struct SysAudio {
    reader: ReaderId<PlayAudio>,
    sounds: AudioReses,
}

impl SysAudio {
    pub fn new(world: &mut World) -> Result<Self, String> {
        <Self as System<'_>>::SystemData::setup(world);
        let reader_id = world.fetch_mut::<EventChannel<PlayAudio>>().register_reader();
        let sounds = load_audio_chunks(Path::new(SOUNDS_PATH))?;
        Channel::all().set_volume(DEFAULT_VOLUME_PERCENT);
        Ok(Self { reader: reader_id, sounds: sounds })
    }

    fn play_sounds(&mut self, audio_chan: &EventChannel<PlayAudio>) -> Result<(), String> {
        for event in audio_chan.read(&mut self.reader) {
            match event {
                PlayAudio::BangSound => play_with_cooldown(&mut self.sounds.bang, 0.4)?,
                PlayAudio::FireSound => play_with_cooldown(&mut self.sounds.fire, 0.2)?,
                PlayAudio::ThrustSound => play_with_cooldown(&mut self.sounds.thrust, 0.2)?,
            }
        }
        Ok(())
    }
}

impl<'a> System<'a> for SysAudio {
    type SystemData = (
        Read<'a, EventChannel<PlayAudio>>,
        Read<'a, DeltaTime>,
    );

    fn run(&mut self, (audio_chan, delta): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        // Play sounds
        self.play_sounds(&audio_chan).unwrap_or_else(|err| println!("Failed to play sound: {}", err));
        // Update cooldowns
        update_cooldown(&mut self.sounds.bang, dt);
        update_cooldown(&mut self.sounds.fire, dt);
        update_cooldown(&mut self.sounds.thrust, dt);
    }
}


fn play_with_cooldown(ch: &mut CooledChunk, dt: f32) -> Result<(), String> {
    if ch.cooldown == 0.0 {
        ch.cooldown = dt;
        Channel::all().play(&ch.chunk, 0).map(|_| ())?;
    }
    Ok(())
}

fn update_cooldown(ch: &mut CooledChunk, dt: f32) {
    if ch.cooldown > 0.0 {
        ch.cooldown -= dt;
        if ch.cooldown < 0.0 {
            ch.cooldown = 0.0;
        }
    }

}
