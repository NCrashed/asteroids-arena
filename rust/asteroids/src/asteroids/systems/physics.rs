use specs::prelude::*;
use glam::Vec2;

use super::super::components::pos::*;
use super::super::components::size::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPhysics;

impl<'a> System<'a> for SysPhysics {
    type SystemData = (Read<'a, DeltaTime>,
        Read<'a, WorldSize>,
        WriteStorage<'a, Pos>,
        ReadStorage<'a, Vel>);

    fn run(&mut self, (delta, wsize, mut pos, vel): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for (pos, vel) in (&mut pos, &vel).join() {
            // Apply all velocities
            pos.0 += vel.0 * dt;
            // Wrap space
            wrap_space(&wsize, pos);
        }
    }
}

/// If some entity goes offscreen, teleport it to other side.
fn wrap_space(wsize: &WorldSize, pos: &mut Pos) {
    let Vec2{x, y} = pos.0;
    if x > wsize.0 as f32 {
        pos.0.x = 0.0;
    } else if x < 0.0 {
        pos.0.x = wsize.0 as f32;
    }
    if y > wsize.1 as f32 {
        pos.0.y = 0.0;
    } else if y < 0.0 {
        pos.0.y = wsize.1 as f32;
    }
}
