use specs::prelude::*;

use super::super::components::pos::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPhysics;

impl<'a> System<'a> for SysPhysics {
    type SystemData = (Read<'a, DeltaTime>, WriteStorage<'a, Pos>, ReadStorage<'a, Vel>);

    fn run(&mut self, (delta, mut pos, vel): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for (pos, vel) in (&mut pos, &vel).join() {
            pos.0 += vel.0 * dt;
        }
    }
}
