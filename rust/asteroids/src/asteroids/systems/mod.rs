pub mod asteroid;
pub mod audio;
pub mod bullet;
pub mod physics;
pub mod player;

use self::asteroid::SysAsteroid;
use self::bullet::SysBullet;
use self::physics::SysPhysics;
use self::player::SysPlayer;
use specs::prelude::*;

pub fn init_systems<'a, 'b>(mut w : &mut World) -> Result<Dispatcher<'a, 'b>, String> {
    let mut dispatcher = DispatcherBuilder::new()
        .with(SysPlayer::new(w), "player", &[])
        .with(SysBullet::new(w), "bullet", &[])
        .with(SysAsteroid::new(w), "asteroid", &[])
        .with(SysPhysics, "physics", &[])
        .build();

    dispatcher.setup(&mut w);
    Ok(dispatcher)
}
