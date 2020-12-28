use legion::*;
use sdl2::mixer::Channel;
use crossbeam::channel::*;

use super::super::components::audio::*;
use super::super::components::time::*;

/// System that play sounds and control their cooldowns.
/// The system is designed to be thread local, so use `with_thread_local` when
/// register it.
#[system]
pub fn audio(
    #[state] mut sounds: &mut AudioReses,
    #[resource] audio_chan: &Receiver<PlayAudio>,
    #[resource] delta: &DeltaTime,
) {
    let dt = delta.0.as_secs_f32();
    // Play sounds
    play_sounds(&mut sounds, &audio_chan)
        .unwrap_or_else(|err| println!("Failed to play sound: {}", err));
    // Update cooldowns
    update_cooldown(&mut sounds.bang, dt);
    update_cooldown(&mut sounds.fire, dt);
    update_cooldown(&mut sounds.thrust, dt);
}

fn play_sounds(sounds: &mut AudioReses, audio_chan: &Receiver<PlayAudio>) -> Result<(), String> {
    for event in audio_chan.try_iter() {
        match event {
            PlayAudio::BangSound => play_with_cooldown(&mut sounds.bang, 0.4)?,
            PlayAudio::FireSound => play_with_cooldown(&mut sounds.fire, 0.2)?,
            PlayAudio::ThrustSound => play_with_cooldown(&mut sounds.thrust, 0.2)?,
        }
    }
    Ok(())
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
