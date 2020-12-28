use crossbeam::channel::*;
use glam::Vec2;
use legion::*;
use legion::systems::*;
use legion::world::SubWorld;
use rand::Rng;
use std::f32::consts::PI;

use super::super::components::asteroid::*;
use super::super::components::audio::*;
use super::super::components::mass::*;
use super::super::components::pos::*;
use super::super::components::rot::*;
use super::super::components::vel::*;

/// System that spawns asteroids when bullet hits them.
#[system]
#[read_component(Asteroid)]
#[read_component(Pos)]
#[read_component(Vel)]
#[read_component(Rot)]
pub fn asteroid(
    cmd: &mut CommandBuffer,
    world: &SubWorld,
    #[resource] chan: &Receiver<AsteroidBreak>,
    #[resource] audio_chan: &Sender<PlayAudio>)
{
    let mut rng = rand::thread_rng();
    // Break and destroy asteroids on event
    for event in chan.try_iter() {
        audio_chan.send(PlayAudio::BangSound).unwrap();
        let e = event.0;
        if let Ok(entry) = world.entry_ref(e) {
            let radius = entry.get_component::<Asteroid>().map(|a| a.radius).unwrap_or(0.0);
            let p = entry.get_component::<Pos>().map(|a| a.0).unwrap_or(Vec2::new(0.0, 0.0));
            let v = entry.get_component::<Vel>().map(|a| a.0).unwrap_or(Vec2::new(0.0, 0.0));
            let r = entry.get_component::<Rot>().map(|a| a.0).unwrap_or(0.0);
            cmd.remove(e);
            let child_radius = radius * 0.5;
            if child_radius > ASTEROID_SIZE_RANGE.0 {
                let vx = rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1);
                let vy = rng.gen_range(ASTEROID_VELOCITY_RANGE.0, ASTEROID_VELOCITY_RANGE.1);
                let dv = Vec2::new(vx, vy);
                cmd.push((
                    Asteroid {
                        edges: rng.gen_range(ASTEROID_EDGES_RANGE.0, ASTEROID_EDGES_RANGE.1),
                        radius: child_radius,
                    },
                    Mass(PI * radius * radius * ASTEROID_DENSITY),
                    Pos(p),
                    Vel(v + dv),
                    Rot(r),
                ));
                cmd.push((
                    Asteroid {
                        edges: rng.gen_range(ASTEROID_EDGES_RANGE.0, ASTEROID_EDGES_RANGE.1),
                        radius: child_radius,
                    },
                    Mass(PI * radius * radius * ASTEROID_DENSITY),
                    Pos(p),
                    Vel(v - dv),
                    Rot(r),
                ));
            }
        }
    }
}
