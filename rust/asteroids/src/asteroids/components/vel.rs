use specs::prelude::*;
use glam::Vec2;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(VecStorage)]
pub struct Vel(pub Vec2);
