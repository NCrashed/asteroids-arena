use specs::prelude::*;
use glam::Vec2;
use std::f32::consts::PI;

use super::pos::*;
use super::size::*;
use super::mass::*;
use super::rot::*;
use super::vel::*;

#[derive(Component, Copy, Clone, Debug, Default)]
#[storage(HashMapStorage)]

/// Player component that has thrust engine flag (`true` for on) and bullet cooldown time left.
pub struct Player(pub bool, pub f32);

/// Player size in pixels
pub const PLAYER_SIZE : Vec2 = Vec2 { x: 30.0, y: 25.0 };

/// Player is circle for collision detection.
pub const PLAYER_COLLIDE_RADIUS : f32 = 15.0;

/// Player mass in kg
pub const PLAYER_MASS : f32 = 1000.0;

/// Force of engine in Newtons
pub const PLAYER_THRUST : f32 = 100000.0;

/// Rotation speed in radians per second
pub const PLAYER_ROTATE_SPEED : f32 = PI;

/// Amount of seconds between spawn of two player bullets
pub const PLAYER_FIRE_COOLDOWN : f32 = 0.3;

/// Possible inputs from player
#[derive(PartialEq, Eq, Hash)]
pub enum PlayerInput {
    Thrust,
    RotateLeft,
    RotateRight,
    Fire
}

/// Allocate components for the player
pub fn spawn_player(w : &mut World) -> Entity {
    let ws : WorldSize = *w.read_resource::<WorldSize>();
    return w.create_entity()
        .with(Player(false, PLAYER_FIRE_COOLDOWN))
        .with(Mass(PLAYER_MASS))
        .with(Pos(Vec2 { x: ws.0 as f32 * 0.5, y: ws.1 as f32 * 0.5}))
        .with(Rot(0.0))
        .with(Vel(Vec2 { x: 0.0, y: 0.0 }))
        .build();
}
