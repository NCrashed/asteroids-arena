pub mod physics;

use specs::prelude::*;
use self::physics::SysPhysics;

pub fn init_systems<'a, 'b>(mut w : &mut World) -> Dispatcher<'a, 'b> {
    let mut dispatcher = DispatcherBuilder::new()
        .with(SysPhysics, "physics", &[])
        .build();

    dispatcher.setup(&mut w);
    return dispatcher;
}
