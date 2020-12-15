const st = @import("../storage/unique.zig");

/// Player component that is unique per world
pub const Player = struct {
    /// Whether player ship is accelerating at the moment
    thrust: bool,
    /// Counts until moment when player can emit a new bullet
    fire_cooldown: f32,
};

/// We store only one player component in world
pub const Storage = st.UniqueStorage(f32);
