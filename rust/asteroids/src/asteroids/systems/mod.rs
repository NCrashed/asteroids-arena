pub mod physics;
pub mod player;

use self::physics::SysPhysics;
use self::player::SysPlayer;
use specs::prelude::*;

pub fn init_systems<'a, 'b>(mut w : &mut World) -> Dispatcher<'a, 'b> {
    let mut dispatcher = DispatcherBuilder::new()
        .with(SysPlayer, "player", &[])
        .with(SysPhysics, "physics", &[])
        .build();

    dispatcher.setup(&mut w);
    return dispatcher;
}
