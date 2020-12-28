use glam::Vec2;

/// Bullet component contains time to live for that bullet
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Bullet(pub f32);

/// Bullet speed meter per second
pub const BULLET_SPEED: f32 = 200.0;

/// Amount of seconds bullet has to live at spawn
pub const BULLET_LIFE_TIME: f32 = 3.0;

/// Event that is fired when bullet is spawned. Bullet system will allocate new bullets on the fly.
#[derive(Debug)]
pub struct BulletSpawn {
    pub pos: Vec2,
    pub vel: Vec2,
}
