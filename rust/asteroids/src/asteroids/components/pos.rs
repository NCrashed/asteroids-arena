use specs::prelude::*;
use glam::Vec2;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(VecStorage)]
pub struct Pos(pub Vec2);
