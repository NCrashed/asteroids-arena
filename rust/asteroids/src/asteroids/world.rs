use legion::*;

use super::systems::*;
use super::components::asteroid::*;
use super::components::player::*;

pub fn init_world() -> (World, Resources) {
    let mut world = World::default();
    let mut resources = init_resources();

    create_player(&mut resources, &mut world);
    create_asteroids(&mut resources, &mut world);
    (world, resources)
}
