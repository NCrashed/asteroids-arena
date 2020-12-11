use shrev::EventChannel;
use specs::prelude::*;
use glam::Vec2;

use super::super::stringify::*;
use super::super::components::asteroid::*;
use super::super::components::player::*;
use super::super::components::bullet::*;
use super::super::components::pos::*;
use super::super::components::size::*;
use super::super::components::time::*;
use super::super::components::vel::*;

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
        Entities<'a>);

    fn run(&mut self, (mut asteroid_chan, mut player_chan, delta, wsize, mut pos, vel, bullet, asteroid, player, entities): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for (pos, vel) in (&mut pos, &vel).join() {
            // Apply all velocities
            pos.0 += vel.0 * dt;
            // Wrap space
            wrap_space(&wsize, pos);
        }
        // Check collisions between bullet and asteroids
        'outer1: for (_, bpos, be) in (&bullet, &pos, &entities).join() {
            for(asteroid, apos, ae) in (&asteroid, &pos, &entities).join() {
                if asteroid_collide_point(asteroid, apos.0, bpos.0) {
                    asteroid_chan.single_write(AsteroidBreak(ae));
                    match entities.delete(be).stringify() {
                        Err(msg) => println!("Failed to delete bullet {:?}: {}", be, msg),
                        _ => ()
                    }
                    break 'outer1;
                }
            }
        }
        // Check collision between player and asteroids
        'outer2: for(asteroid, apos, ae) in (&asteroid, &pos, &entities).join() {
            for(_, ppos, pe) in (&player, &pos, &entities).join() {
                if asteroid_collide_radius(asteroid, apos.0, ppos.0, PLAYER_COLLIDE_RADIUS) {
                    player_chan.single_write(PlayerCollide { player: pe, obstacle: ae });
                    break 'outer2;
                }
            }
        }
    }
}

/// If some entity goes offscreen, teleport it to other side.
fn wrap_space(wsize: &WorldSize, pos: &mut Pos) {
    let Vec2{x, y} = pos.0;
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
    let r = asteroid.radius+radius;
    (bpos - apos).length_squared() <= r * r
}
