use glam::Vec2;
use crossbeam::channel::*;
use legion::*;
use legion::systems::CommandBuffer;
use legion::world::SubWorld;

use super::super::components::asteroid::*;
use super::super::components::bullet::*;
use super::super::components::player::*;
use super::super::components::pos::*;
use super::super::components::size::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
#[system]
#[write_component(Pos)]
#[read_component(Vel)]
#[read_component(Bullet)]
#[read_component(Asteroid)]
#[read_component(Player)]
pub fn physics(
    #[resource] asteroid_chan: &Sender<AsteroidBreak>,
    #[resource] player_chan: &Sender<PlayerCollide>,
    #[resource] delta: &DeltaTime,
    #[resource] wsize: &WorldSize,
    cmd: &mut CommandBuffer,
    world: &mut SubWorld,
) {
    let dt = delta.0.as_secs_f32();

    for (pos, vel) in <(&mut Pos, &Vel)>::query().iter_mut(world) {
        // Apply all velocities
        pos.0 += vel.0 * dt;
        // Wrap space
        wrap_space(&wsize, pos);
    }

    // Check collisions between bullet and asteroids
    for (_, bpos, be) in <(&Bullet, &Pos, Entity)>::query().iter(world) {
        for (asteroid, apos, ae) in <(&Asteroid, &Pos, Entity)>::query().iter(world) {
            if asteroid_collide_point(asteroid, apos.0, bpos.0) {
                asteroid_chan.send(AsteroidBreak(*ae)).unwrap();
                cmd.remove(*be);
            }
        }
    }

    // Check collision between player and asteroids
    for (asteroid, apos, ae) in <(&Asteroid, &Pos, Entity)>::query().iter(world) {
        for (_, ppos, pe) in <(&Player, &Pos, Entity)>::query().iter(world) {
            if asteroid_collide_radius(asteroid, apos.0, ppos.0, PLAYER_COLLIDE_RADIUS) {
                player_chan.send(PlayerCollide {
                    player: *pe,
                    obstacle: *ae,
                }).unwrap();
            }
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
