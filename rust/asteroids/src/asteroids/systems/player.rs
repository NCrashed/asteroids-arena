use shrev::EventChannel;
use specs::prelude::*;
use std::collections::HashSet;
use glam::Vec2;

use super::super::stringify::*;
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
pub struct SysPlayer {
    reader: ReaderId<PlayerCollide>
}

impl SysPlayer {
    pub fn new(world: &mut World) -> Self {
        <Self as System<'_>>::SystemData::setup(world);
        let reader_id = world.fetch_mut::<EventChannel<PlayerCollide>>().register_reader();
        Self { reader: reader_id }
    }
}

impl<'a> System<'a> for SysPlayer {
    type SystemData = (
        Read<'a, EventChannel<PlayerCollide>>,
        Write<'a, EventChannel<BulletSpawn>>,
        Write<'a, EventChannel<PlayAudio>>,
        Read<'a, HashSet<PlayerInput>>,
        Read<'a, DeltaTime>,
        Read<'a, WorldSize>,
        WriteStorage<'a, Player>,
        WriteStorage<'a, Pos>,
        WriteStorage<'a, Vel>,
        WriteStorage<'a, Rot>,
        ReadStorage<'a, Mass>);

    fn run(&mut self, (player_chan, mut bullet_chan, mut audio_chan, pinput, delta, ws, mut player, mut pos, mut vel_store, mut rot, mass): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for (player, pos, vel, rot, mass) in (&mut player, &pos, &mut vel_store, &mut rot, &mass).join() {
            // Drop thrusting flag. That disables rendering of engine when there is not pressed up key by player.
            player.0 = false;
            // Process cooldown for firing
            if player.1 > 0.0 {
                player.1 -= dt;
                if player.1 < 0.0 { player.1 = 0.0; }
            }
            // Process inputs
            for ie in pinput.iter() {
                match ie {
                    PlayerInput::Thrust => {
                        let dv = dt * PLAYER_THRUST / mass.0;
                        add_vel_forward(vel, rot, dv);
                        player.0 = true;
                        audio_chan.single_write(PlayAudio::ThrustSound);
                    },
                    PlayerInput::RotateLeft => {
                        rotation_increase(rot, -dt*PLAYER_ROTATE_SPEED);
                    },
                    PlayerInput::RotateRight => {
                        rotation_increase(rot, dt*PLAYER_ROTATE_SPEED);
                    },
                    PlayerInput::Fire => {
                        if player.1 == 0. {
                            player.1 = PLAYER_FIRE_COOLDOWN;
                            let bv = vel.0 + rot_vec(rot) * BULLET_SPEED;
                            bullet_chan.single_write(BulletSpawn{ pos: pos.0, vel: bv });
                            audio_chan.single_write(PlayAudio::FireSound);
                        }
                    },
                }
            }
        }
        // Kill player on collision
        for event in player_chan.read(&mut self.reader) {
            let hasp = player.contains(event.player);
            if hasp {
                audio_chan.single_write(PlayAudio::BangSound);
                match respawn_player(event.player, &ws, &mut player, &mut pos, &mut vel_store, &mut rot) {
                    Err(msg) => { println!("Failed to kill player {:?}: {}", event.player, msg); }
                    _ => ()
                }
            }
        }
    }
}

fn respawn_player<'a>(e: Entity, ws: &WorldSize, player: &mut WriteStorage<'a, Player>, pos: &mut WriteStorage<'a, Pos>, vel: &mut WriteStorage<'a, Vel>, rot: &mut WriteStorage<'a, Rot>) -> Result<(), String> {
    player.insert(e, Player(false, PLAYER_FIRE_COOLDOWN)).stringify()?;
    pos.insert(e, Pos(Vec2 { x: ws.0 as f32 * 0.5, y: ws.1 as f32 * 0.5})).stringify()?;
    vel.insert(e, Vel(Vec2 { x: 0.0, y: 0.0 })).stringify()?;
    rot.insert(e, Rot(0.0)).stringify()
}
