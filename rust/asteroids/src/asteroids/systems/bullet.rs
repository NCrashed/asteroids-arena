use specs::prelude::*;
use glam::Vec2;

use super::super::components::bullet::*;
use super::super::components::pos::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that spawns bullets and destroys them when they live timer is over.
pub struct SysBullet;

impl<'a> System<'a> for SysBullet {
    type SystemData = (
        Read<'a, DeltaTime>,
        Entities<'a>,
        WriteStorage<'a, Bullet>,
        WriteStorage<'a, Pos>,
        WriteStorage<'a, Vel>);

    fn run(&mut self, (delta, entities, mut bullet, mut pos, mut vel): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for bullet in (&mut bullet).join() {

        }
    }
}
