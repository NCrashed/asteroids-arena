const st = @import("../storage/global.zig");

/// Initial world width in pixels
pub const initial_width: i32 = 1480;
/// Initial world height in pixels
pub const initial_height: i32 = 1024;

/// World size is current size of rendering window in pixels
pub const WorldSize = struct {
    /// Current width of world
    width: i32,
    /// Current height of world
    height: i32,

    /// Default initial value that is required by global storage
    pub fn default() WorldSize {
        return WorldSize{
            .width = initial_width,
            .height = initial_height,
        };
    }

    pub fn deinit(self: *WorldSize) void {}
};

/// We store global single component in world
pub const Storage = st.GlobalStorage(WorldSize, WorldSize.deinit);
