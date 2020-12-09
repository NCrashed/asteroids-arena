use specs::prelude::*;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(VecStorage)]
pub struct Rot(pub f32);
