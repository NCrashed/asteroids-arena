use shrev::EventChannel;
use specs::prelude::*;
use std::collections::HashSet;

use super::super::components::bullet::*;
use super::super::components::mass::*;
use super::super::components::player::*;
use super::super::components::pos::*;
use super::super::components::rot::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPlayer;

impl<'a> System<'a> for SysPlayer {
    type SystemData = (
        Write<'a, EventChannel<BulletSpawn>>,
        Read<'a, HashSet<PlayerInput>>,
        Read<'a, DeltaTime>,
        WriteStorage<'a, Player>,
        ReadStorage<'a, Pos>,
        WriteStorage<'a, Vel>,
        WriteStorage<'a, Rot>,
        ReadStorage<'a, Mass>);

    fn run(&mut self, (mut bullet_chan, pinput, delta, mut player, pos, mut vel_store, mut rot, mass): Self::SystemData) {
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
                        }
                    },
                }
            }
        }
    }
}
