use glam::Vec2;
use crossbeam::channel::*;
use legion::*;
use legion::world::SubWorld;
use std::collections::HashSet;

use super::super::components::audio::*;
use super::super::components::bullet::*;
use super::super::components::mass::*;
use super::super::components::player::*;
use super::super::components::pos::*;
use super::super::components::rot::*;
use super::super::components::size::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
#[system]
#[write_component(Player)]
#[write_component(Pos)]
#[write_component(Vel)]
#[write_component(Rot)]
#[read_component(Mass)]
pub fn player(
    #[resource] player_chan: &Receiver<PlayerCollide>,
    #[resource] bullet_chan: &Sender<BulletSpawn>,
    #[resource] audio_chan: &Sender<PlayAudio>,
    #[resource] pinput: &HashSet<PlayerInput>,
    #[resource] delta: &DeltaTime,
    #[resource] ws: &WorldSize,
    mut world: &mut SubWorld,
) {
    let dt = delta.0.as_secs_f32();
    for (player, pos, vel, rot, mass) in <(&mut Player, &Pos, &mut Vel, &mut Rot, &Mass)>::query().iter_mut(world)
    {
        // Drop thrusting flag. That disables rendering of engine when there is not pressed up key by player.
        player.0 = false;
        // Process cooldown for firing
        if player.1 > 0.0 {
            player.1 -= dt;
            if player.1 < 0.0 {
                player.1 = 0.0;
            }
        }
        // Process inputs
        for ie in pinput.iter() {
            match ie {
                PlayerInput::Thrust => {
                    let dv = dt * PLAYER_THRUST / mass.0;
                    add_vel_forward(vel, rot, dv);
                    player.0 = true;
                    audio_chan.send(PlayAudio::ThrustSound).unwrap();
                }
                PlayerInput::RotateLeft => {
                    rotation_increase(rot, -dt * PLAYER_ROTATE_SPEED);
                }
                PlayerInput::RotateRight => {
                    rotation_increase(rot, dt * PLAYER_ROTATE_SPEED);
                }
                PlayerInput::Fire => {
                    if player.1 == 0. {
                        player.1 = PLAYER_FIRE_COOLDOWN;
                        let bv = vel.0 + rot_vec(rot) * BULLET_SPEED;
                        bullet_chan.send(BulletSpawn {
                            pos: pos.0,
                            vel: bv,
                        }).unwrap();
                        audio_chan.send(PlayAudio::FireSound).unwrap();
                    }
                }
            }
        }
    }
    // Kill player on collision
    for event in player_chan.try_iter() {
        if let Ok(_) = world.entry_ref(event.player) {
            audio_chan.send(PlayAudio::BangSound).unwrap();
            respawn_player(
                event.player,
                &ws,
                &mut world,
            );
        }
    }
}

fn respawn_player<'a>(
    e: Entity,
    ws: &WorldSize,
    world: &mut SubWorld
) {
    if let Ok(mut entry) = world.entry_mut(e) {
        *entry.get_component_mut::<Player>().unwrap() = Player(false, PLAYER_FIRE_COOLDOWN);
        *entry.get_component_mut::<Pos>().unwrap() = Pos(Vec2 {
            x: ws.0 as f32 * 0.5,
            y: ws.1 as f32 * 0.5,
        });
        *entry.get_component_mut::<Vel>().unwrap() = Vel(Vec2 { x: 0.0, y: 0.0 });
        *entry.get_component_mut::<Rot>().unwrap() = Rot(0.0);
    }
}
