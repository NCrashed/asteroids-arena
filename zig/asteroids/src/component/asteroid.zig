const st = @import("../storage/vector.zig");
const std = @import("std");

/// Asteroid component which each asteroid has
pub const Asteroid = struct {
    /// Amount of edges for rendering
    edges: i32,
};

/// Store asteroids in array
pub const Storage = st.VecStorage(Asteroid);

/// Minimum amount of edges for asteroid at generation time
pub const edges_min: i32 = 8;
/// Maximum amount of edges for asteroid at generation time
pub const edges_max: i32 = 20;

/// Minimum radius for asteroid at generation time
pub const size_min: i32 = 10;
/// Maximum radius for asteroid at generation time
pub const size_max: i32 = 130;

/// Minimum velocity for asteroid at generation time
pub const velocity_min: i32 = -100;
/// Maximum velocity for asteroid at generation time
pub const velocity_max: i32 = 100;

/// Density of the asteroid kg per m^2
pub const density: f32 = 1.0;

/// Amount of asteroids at generation time
pub const amount: usize = 20;
