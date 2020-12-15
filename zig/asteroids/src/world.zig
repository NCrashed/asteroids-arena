const input = @import("input.zig");
const position = @import("components/position.zig");
const storage = @import("storage.zig");

/// Initial world width in pixels
pub const width : i32 = 1480;
/// Initial world height in pixels
pub const height : i32 = 1024;

/// The game uses Entity-Component-System (ECS) design where all game entities
/// are decomposed into data pieces called Components. Components are stored
/// in structure-of-arrays style and an entity is a simple integer that points
/// to the arrays.
pub const World = struct {
    entity_counter: usize,
    position: position.Storage,

    /// Initialize internal storages, allocates memory for them. Return non zero
    /// result on error.
    pub fn init() !World {
        return World {
            .entity_counter = 0,
            .position = position.Storage.init(),
        };
    }

    /// Deallocate internal storages and free memory.
    pub fn deinit(self: *World) void {
        self.position.deinit();
    }

    ///  Make one tick of world simulation with given inputs. Return non zero if failed.
    pub fn step(self: *World, dt: f64, events: *const input.Events) !void {

    }
};
