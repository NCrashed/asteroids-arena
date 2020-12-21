const st = @import("../storage/vector.zig");
const std = @import("std");

/// Marks that the entity is bullet
pub const Bullet = struct {
    /// Amount of time left until despawn
    time: f32,
};

/// Store bullets in array
pub const Storage = st.VecStorage(Bullet);

/// Bullet spawn velocity absolute value
pub const speed: f32 = 200;
/// Amount of seconds bullet lives after spawn
pub const life_time: f32 = 3.0;
/// Collision radius for bullet
pub const radius: f32 = 1.0;
