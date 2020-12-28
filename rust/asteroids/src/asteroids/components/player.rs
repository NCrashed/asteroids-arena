use glam::Vec2;
use legion::*;
use std::f32::consts::PI;

use super::mass::*;
use super::pos::*;
use super::rot::*;
use super::size::*;
use super::vel::*;

/// Player component that has thrust engine flag (`true` for on) and bullet cooldown time left.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Player(pub bool, pub f32);

/// Player size in pixels
pub const PLAYER_SIZE: Vec2 = Vec2 { x: 30.0, y: 25.0 };

/// Player is circle for collision detection.
pub const PLAYER_COLLIDE_RADIUS: f32 = 15.0;

/// Player mass in kg
pub const PLAYER_MASS: f32 = 1000.0;

/// Force of engine in Newtons
pub const PLAYER_THRUST: f32 = 100000.0;

/// Rotation speed in radians per second
pub const PLAYER_ROTATE_SPEED: f32 = PI;

/// Amount of seconds between spawn of two player bullets
pub const PLAYER_FIRE_COOLDOWN: f32 = 0.3;

/// Possible inputs from player
#[derive(PartialEq, Eq, Hash)]
pub enum PlayerInput {
    Thrust,
    RotateLeft,
    RotateRight,
    Fire,
}

/// Event that fires when player collides with asteroid
#[derive(Debug)]
pub struct PlayerCollide {
    pub player: Entity,
    pub obstacle: Entity,
}

/// Allocate components for the player
pub fn create_player(resources: &mut Resources, w: &mut World) -> Entity {
    let ws: WorldSize = *resources.get::<WorldSize>().unwrap();
    w.push((
        Player(false, PLAYER_FIRE_COOLDOWN),
        Mass(PLAYER_MASS),
        Pos(Vec2 {
            x: ws.0 as f32 * 0.5,
            y: ws.1 as f32 * 0.5,
        }),
        Rot(0.0),
        Vel(Vec2 { x: 0.0, y: 0.0 }),
    ))
}
