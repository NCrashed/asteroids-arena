pub mod asteroid;
pub mod audio;
pub mod bullet;
pub mod physics;
pub mod player;
pub mod render;

use crossbeam::channel::*;
use legion::*;
use self::audio::audio_system;
use self::asteroid::asteroid_system;
use self::bullet::bullet_system;
use self::physics::physics_system;
use self::player::player_system;
use self::render::render_system;
use std::collections::HashSet;
use super::components::asteroid::*;
use super::components::audio::*;
use super::components::bullet::*;
use super::components::player::*;
use super::components::size::*;
use super::components::time::*;

pub fn init_systems() -> Result<Schedule, String> {
    let schedule = Schedule::builder()
        .add_system(player_system())
        .add_system(bullet_system())
        .add_system(asteroid_system())
        .add_system(physics_system())
        .flush()
        .add_thread_local(audio_system(AudioReses::init()?))
        .add_thread_local(render_system())
        .build();
    Ok(schedule)
}

pub fn init_resources() -> Resources {
    let mut resources = Resources::default();

    let (ast_r, ast_s) = unbounded::<AsteroidBreak>();
    resources.insert(ast_r);
    resources.insert(ast_s);

    let (audio_r, audio_s) = unbounded::<PlayAudio>();
    resources.insert(audio_r);
    resources.insert(audio_s);

    let (bullet_r, bullet_s) = unbounded::<BulletSpawn>();
    resources.insert(bullet_r);
    resources.insert(bullet_s);

    let (player_r, player_s) = unbounded::<PlayerCollide>();
    resources.insert(player_r);
    resources.insert(player_s);

    resources.insert(HashSet::<PlayerInput>::default());
    resources.insert(DeltaTime::default());
    resources.insert(WorldSize::default());

    resources
}
