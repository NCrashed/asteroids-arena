pub mod physics;

use specs::prelude::*;
use self::physics::SysPhysics;

pub fn init_systems(mut w : &mut World) -> Dispatcher<'static, 'static> {
    let mut dispatcher = DispatcherBuilder::new()
        .with(SysPhysics, "physics", &[])
        .build();

    dispatcher.setup(&mut w);
    return dispatcher;
}
