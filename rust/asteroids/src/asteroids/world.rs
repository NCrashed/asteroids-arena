use specs::prelude::*;

use super::components::pos::*;
use super::components::vel::*;

pub fn init_world() -> World {
    let mut world = World::new();
    world.register::<Pos>();
    world.register::<Vel>();

    return world;
}
