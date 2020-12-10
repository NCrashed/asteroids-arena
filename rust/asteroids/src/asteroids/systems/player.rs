use specs::prelude::*;

use super::super::components::vel::*;
use super::super::components::mass::*;
use super::super::components::rot::*;
use super::super::components::player::*;

/// System that updates position, calcs space warping and collisions.
pub struct SysPlayer;

impl<'a> System<'a> for SysPlayer {
    type SystemData = (Read<'a, Vec<PlayerInput>>,
        WriteStorage<'a, Player>,
        WriteStorage<'a, Vel>,
        WriteStorage<'a, Rot>,
        ReadStorage<'a, Mass>);

    fn run(&mut self, (pinput, mut player, mut vel, mut rot, mut mass): Self::SystemData) {

    }
}
