use glam::Vec2;
use rand::Rng;
use shrev::EventChannel;
use specs::prelude::*;
use std::f32::consts::PI;

use super::super::stringify::*;
use super::super::components::asteroid::*;
use super::super::components::mass::*;
use super::super::components::pos::*;
use super::super::components::rot::*;
use super::super::components::vel::*;

/// System that spawns bullets and destroys them when they live timer is over.
pub struct SysAsteroid {
    reader: ReaderId<AsteroidBreak>
}

impl SysAsteroid {
    pub fn new(world: &mut World) -> Self {
        <Self as System<'_>>::SystemData::setup(world);
        let reader_id = world.fetch_mut::<EventChannel<AsteroidBreak>>().register_reader();
        Self { reader: reader_id }
    }
}

impl<'a> System<'a> for SysAsteroid {
    type SystemData = (
        Read<'a, EventChannel<AsteroidBreak>>,
        Entities<'a>,
        WriteStorage<'a, Asteroid>,
        WriteStorage<'a, Mass>,
        WriteStorage<'a, Pos>,
        WriteStorage<'a, Vel>,
        WriteStorage<'a, Rot>,
    );

    fn run(&mut self, (chan, entities, mut asteroid, mut mass, mut pos, mut vel, mut rot): Self::SystemData) {
        let mut rng = rand::thread_rng();
        // Break and destroy asteroids on event
        for event in chan.read(&mut self.reader) {
            let e = event.0;
            let radius = asteroid.get(e).map(|a| a.radius).unwrap_or(0.0);
            let p = pos.get(e).map(|a| a.0).unwrap_or(Vec2::new(0.0, 0.0));
            let v = vel.get(e).map(|a| a.0).unwrap_or(Vec2::new(0.0, 0.0));
            let r = rot.get(e).map(|a| a.0).unwrap_or(0.0);
            match entities.delete(e).stringify() {
                Err(msg) => println!("Failed to delete asteroid {:?}: {}", e, msg),
                _ => ()
            }
            let child_radius = radius * 0.5;
            if child_radius > ASTEROID_SIZE_RANGE.0 {
                let vx = rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1);
                let vy = rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1);
                let dv = Vec2::new(vx, vy);
                let a1 = spawn_asteroid(&entities, &mut asteroid, &mut mass, &mut pos, &mut vel, &mut rot, child_radius, p, v+dv, r);
                match a1 {
                    Err(msg) => println!("Failed to spawn asteroid: {}", msg),
                    _ => ()
                }
                let a2 = spawn_asteroid(&entities, &mut asteroid, &mut mass, &mut pos, &mut vel, &mut rot, child_radius, p, v-dv, r);
                match a2 {
                    Err(msg) => println!("Failed to spawn asteroid: {}", msg),
                    _ => ()
                }
            }

        }
    }
}

/// Spawn asteroid on the fly
fn spawn_asteroid<'a>(
    entities: &Entities<'a>,
    asteroid_store: &mut WriteStorage<'a, Asteroid>,
    mass_store: &mut WriteStorage<'a, Mass>,
    pos_store: &mut WriteStorage<'a, Pos>,
    vel_store: &mut WriteStorage<'a, Vel>,
    rot_store: &mut WriteStorage<'a, Rot>,
    radius: f32,
    pos: Vec2,
    vel: Vec2,
    rot: f32
    ) -> Result<(), String>
{
    let mut rng = rand::thread_rng();
    let e = entities.create();
    asteroid_store.insert(e, Asteroid{
        edges: rng.gen_range(ASTEROID_EDGES_RANGE.0, ASTEROID_EDGES_RANGE.1),
        radius: radius
    }).stringify()?;
    mass_store.insert(e, Mass(PI * radius * radius * ASTEROID_DENSITY)).stringify()?;
    pos_store.insert(e, Pos(pos)).stringify()?;
    vel_store.insert(e, Vel(vel)).stringify()?;
    rot_store.insert(e, Rot(rot)).stringify()
}
