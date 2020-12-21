use glam::Vec2;
use shrev::EventChannel;
use specs::prelude::*;
use std::sync::mpsc::*;

use super::super::components::asteroid::*;
use super::super::components::bullet::*;
use super::super::components::player::*;
use super::super::components::pos::*;
use super::super::components::size::*;
use super::super::components::time::*;
use super::super::components::vel::*;
use super::super::stringify::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPhysics;

impl<'a> System<'a> for SysPhysics {
    type SystemData = (
        Write<'a, EventChannel<AsteroidBreak>>,
        Write<'a, EventChannel<PlayerCollide>>,
        Read<'a, DeltaTime>,
        Read<'a, WorldSize>,
        WriteStorage<'a, Pos>,
        ReadStorage<'a, Vel>,
        ReadStorage<'a, Bullet>,
        ReadStorage<'a, Asteroid>,
        ReadStorage<'a, Player>,
        Entities<'a>,
    );

    fn run(
        &mut self,
        (
            mut asteroid_chan,
            mut player_chan,
            delta,
            wsize,
            mut pos,
            vel,
            bullet,
            asteroid,
            player,
            entities,
        ): Self::SystemData,
    ) {
        let dt = delta.0.as_secs_f32();
        (&mut pos, &vel).par_join()
            .for_each(|(pos, vel)| {
            // Apply all velocities
            pos.0 += vel.0 * dt;
            // Wrap space
            wrap_space(&wsize, pos);
        });


        // Check collisions between bullet and asteroids
        let (asender, areceiver) = channel();
        (&bullet, &pos, &entities).par_join().for_each_with(asender, |asender, (_, bpos, be)| {
            (&asteroid, &pos, &entities).join().for_each(|(asteroid, apos, ae)| {
                if asteroid_collide_point(asteroid, apos.0, bpos.0) {
                    asender.send(AsteroidBreak(ae)).unwrap();
                    match entities.delete(be).stringify() {
                        Err(msg) => println!("Failed to delete bullet {:?}: {}", be, msg),
                        _ => (),
                    }
                }
            });
        });
        for event in areceiver.iter() {
            asteroid_chan.single_write(event);
        }
        // Check collision between player and asteroids
        let (psender, preceiver) = channel();
        (&asteroid, &pos, &entities).par_join().for_each_with(psender, |psender, (asteroid, apos, ae)| {
            (&player, &pos, &entities).join().for_each(|(_, ppos, pe)| {
                if asteroid_collide_radius(asteroid, apos.0, ppos.0, PLAYER_COLLIDE_RADIUS) {
                    psender.send(PlayerCollide {
                        player: pe,
                        obstacle: ae,
                    }).unwrap();
                }
            });
        });
        for event in preceiver.iter() {
            player_chan.single_write(event);
        }
    }
}

/// If some entity goes offscreen, teleport it to other side.
fn wrap_space(wsize: &WorldSize, pos: &mut Pos) {
    let Vec2 { x, y } = pos.0;
    if x > wsize.0 as f32 {
        pos.0.x = 0.0;
    } else if x < 0.0 {
        pos.0.x = wsize.0 as f32;
    }
    if y > wsize.1 as f32 {
        pos.0.y = 0.0;
    } else if y < 0.0 {
        pos.0.y = wsize.1 as f32;
    }
}

/// Check that point is collided with asteroid
fn asteroid_collide_point(asteroid: &Asteroid, apos: Vec2, bpos: Vec2) -> bool {
    (bpos - apos).length_squared() <= asteroid.radius * asteroid.radius
}

/// Check that circle is collided with asteroid
fn asteroid_collide_radius(asteroid: &Asteroid, apos: Vec2, bpos: Vec2, radius: f32) -> bool {
    let r = asteroid.radius + radius;
    (bpos - apos).length_squared() <= r * r
}
