use glam::Vec2;
use legion::*;
use rand::Rng;
use std::f32::consts::PI;
use std::iter::repeat_with;

use super::mass::*;
use super::pos::*;
use super::rot::*;
use super::size::*;
use super::vel::*;

/// Asteroid component contains amount edges and collision radius
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Asteroid {
    pub edges: u32,
    pub radius: f32,
}

/// Event that is fired when asteroid should break (bullet collision)
#[derive(Debug)]
pub struct AsteroidBreak(pub Entity);

/// Amount of possible edges for asteroid
pub const ASTEROID_EDGES_RANGE: (u32, u32) = (8, 20);

/// Asteroid possible size in pixels (1 px == 1 meter)
pub const ASTEROID_SIZE_RANGE: (f32, f32) = (10.0, 130.0);

/// Asteroid possible velocities
pub const ASTEROID_VELOCITY_RANGE: (f32, f32) = (0.0, 100.0);

/// Asteroid mass density kg per square pixel
pub const ASTEROID_DENSITY: f32 = 1.0;

/// Amount of asteroids at generation time
pub const ASTEROIDS_AMOUNT: usize = 20;

/// Spawn all asteroids at generation time
pub fn create_asteroids(resources: &mut Resources, world: &mut World) {
    let asts : Vec<(Asteroid, Mass, Pos, Rot, Vel)> = repeat_with(|| create_asteroid(resources)).take(ASTEROIDS_AMOUNT).collect();
    world.extend(asts);
}

/// Create new asteroid with random component values
pub fn create_asteroid(resources: &mut Resources) -> (Asteroid, Mass, Pos, Rot, Vel) {
    let mut rng = rand::thread_rng();
    let ws: WorldSize = *resources.get::<WorldSize>().unwrap();
    let r = rng.gen_range(ASTEROID_SIZE_RANGE.0, ASTEROID_SIZE_RANGE.1);
    (Asteroid {
        edges: rng.gen_range(ASTEROID_EDGES_RANGE.0, ASTEROID_EDGES_RANGE.1),
        radius: r,
    },
    Mass(PI * r * r * ASTEROID_DENSITY),
    Pos(Vec2 {
        x: rng.gen_range(0.0, ws.0 as f32),
        y: rng.gen_range(0.0, ws.1 as f32),
    }),
    Rot(rng.gen_range(0.0, 2. * PI)),
    Vel(Vec2 {
        x: rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1),
        y: rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1),
    }),
    )
}
