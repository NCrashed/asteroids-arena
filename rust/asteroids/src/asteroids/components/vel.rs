use glam::Vec2;

use super::rot::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Vel(pub Vec2);

/// Add velocity to direction the rotation has
pub fn add_vel_forward(vel: &mut Vel, rot: &Rot, dv: f32) {
    let r = rot.0;
    *vel = Vel(vel.0 + Vec2::new(dv * r.cos(), dv * r.sin()))
}
