pub mod asteroid;
pub mod bullet;
pub mod physics;
pub mod player;

use self::asteroid::SysAsteroid;
use self::bullet::SysBullet;
use self::physics::SysPhysics;
use self::player::SysPlayer;
use specs::prelude::*;

pub fn init_systems<'a, 'b>(mut w : &mut World) -> Dispatcher<'a, 'b> {
    let mut dispatcher = DispatcherBuilder::new()
        .with(SysPlayer::new(w), "player", &[])
        .with(SysBullet::new(w), "bullet", &[])
        .with(SysAsteroid::new(w), "asteroid", &[])
        .with(SysPhysics, "physics", &[])
        .build();

    dispatcher.setup(&mut w);
    return dispatcher;
}
