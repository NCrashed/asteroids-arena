use glam::Vec2;
use std::f32::consts::PI;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rot(pub f32);

/// Increase rotation of the component by given amount of radians
pub fn rotation_increase(rot: &mut Rot, da: f32) {
    *rot = Rot(clamp_angle(rot.0 + da));
}

/// Make angle lie in range [0 .. 2*PI)
fn clamp_angle(a: f32) -> f32 {
    if a < 0. {
        2. * PI + a
    } else if a > 2. * PI {
        a - 2. * PI
    } else {
        a
    }
}

/// Convert rotation to direction vector
pub fn rot_vec(rot: &Rot) -> Vec2 {
    let a = rot.0;
    Vec2::new(a.cos(), a.sin())
}
