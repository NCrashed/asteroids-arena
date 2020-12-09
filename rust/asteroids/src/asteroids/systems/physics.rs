use specs::prelude::*;

use super::super::components::pos::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPhysics;

impl<'a> System<'a> for SysPhysics {
    type SystemData = (WriteStorage<'a, Pos>, ReadStorage<'a, Vel>);

    fn run(&mut self, (mut pos, vel): Self::SystemData) {
        for (pos, vel) in (&mut pos, &vel).join() {
            pos.0 += vel.0;
        }
    }
}
