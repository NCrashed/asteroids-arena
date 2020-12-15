const input = @import("input.zig");
const storage = @import("storage.zig");

const mass = @import("components/mass.zig");
const player = @import("components/player.zig");
const position = @import("components/position.zig");
const radius = @import("components/radius.zig");
const rotation = @import("components/rotation.zig");
const velocity = @import("components/velocity.zig");

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
    velocity: velocity.Storage,
    rotation: rotation.Storage,
    mass: mass.Storage,
    radius: radius.Storage,
    player: player.Storage,

    /// Initialize internal storages, allocates memory for them. Return non zero
    /// result on error.
    pub fn init() !World {
        return World {
            .entity_counter = 0,
            .position = position.Storage.init(),
            .velocity = velocity.Storage.init(),
            .rotation = rotation.Storage.init(),
            .mass = mass.Storage.init(),
            .radius = radius.Storage.init(),
            .player = player.Storage.init(),
        };
    }

    /// Deallocate internal storages and free memory.
    pub fn deinit(self: *World) void {
        self.position.deinit();
        self.velocity.deinit();
        self.rotation.deinit();
        self.mass.deinit();
        self.radius.deinit();
        self.player.deinit();
    }

    ///  Make one tick of world simulation with given inputs. Return non zero if failed.
    pub fn step(self: *World, dt: f64, events: *const input.Events) !void {

    }
};
