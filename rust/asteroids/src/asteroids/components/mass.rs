use specs::prelude::*;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(VecStorage)]
pub struct Mass(pub f32);
