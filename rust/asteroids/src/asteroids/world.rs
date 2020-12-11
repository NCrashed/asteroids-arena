use specs::prelude::*;
use std::collections::HashSet;
use std::default::default;
use std::time::Duration;

use super::components::mass::*;
use super::components::bullet::*;
use super::components::player::*;
use super::components::pos::*;
use super::components::rot::*;
use super::components::size::*;
use super::components::time::*;
use super::components::vel::*;

pub fn init_world() -> World {
    let mut world = World::new();
    world.register::<Bullet>();
    world.register::<Mass>();
    world.register::<Player>();
    world.register::<Pos>();
    world.register::<Rot>();
    world.register::<Vel>();

    world.insert(DeltaTime(Duration::new(0, 0)));
    world.insert(default::<WorldSize>());
    world.insert(default::<HashSet<PlayerInput>>());

    spawn_player(&mut world);
    return world;
}
