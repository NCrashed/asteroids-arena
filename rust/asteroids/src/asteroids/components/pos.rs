use glam::Vec2;
use specs::prelude::*;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(VecStorage)]
pub struct Pos(pub Vec2);
