use specs::prelude::*;
use std::collections::HashSet;

use super::super::components::mass::*;
use super::super::components::player::*;
use super::super::components::rot::*;
use super::super::components::time::*;
use super::super::components::vel::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPlayer;

impl<'a> System<'a> for SysPlayer {
    type SystemData = (Read<'a, HashSet<PlayerInput>>,
        Read<'a, DeltaTime>,
        WriteStorage<'a, Player>,
        WriteStorage<'a, Vel>,
        WriteStorage<'a, Rot>,
        ReadStorage<'a, Mass>);

    fn run(&mut self, (pinput, delta, mut player, mut vel, mut rot, mass): Self::SystemData) {
        let dt = delta.0.as_secs_f32();
        for (player, vel, rot, mass) in (&mut player, &mut vel, &mut rot, &mass).join() {
            player.0 = false;
            for ie in pinput.iter() {
                match ie {
                    PlayerInput::Thrust => {
                        let dv = dt * PLAYER_THRUST / mass.0;
                        add_vel_forward(vel, rot, dv);
                        player.0 = true;
                    },
                    PlayerInput::RotateLeft => {

                    },
                    PlayerInput::RotateRight => {

                    },
                    PlayerInput::Fire => {

                    },
                }
            }
        }
    }
}
