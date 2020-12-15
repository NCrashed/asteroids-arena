const st = @import("../storage/unique.zig");

/// Player component that is unique per world
pub const Player = struct {
    /// Whether player ship is accelerating at the moment
    thrust: bool,
    /// Counts until moment when player can emit a new bullet
    fire_cooldown: f32,
};

/// We store only one player component in world
pub const Storage = st.UniqueStorage(Player);

/// Mass of player ship in kg
pub const mass : f32 = 100000;

/// Player collision radius in meters
pub const collision_radius : f32 = 15;
