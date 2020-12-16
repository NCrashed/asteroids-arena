const c = @import("sdl.zig").c;
const r = @import("render.zig");

const entity = @import("entity.zig");
const Entity = entity.Entity;
const input = @import("input.zig");
const storage = @import("storage.zig");
const Vec2 = @import("v2.zig").Vec2;

const mass = @import("component/mass.zig");
const player = @import("component/player.zig");
const position = @import("component/position.zig");
const radius = @import("component/radius.zig");
const rotation = @import("component/rotation.zig");
const velocity = @import("component/velocity.zig");
const size = @import("component/size.zig");

/// Initial world width in pixels
pub const width = size.initial_width;
/// Initial world height in pixels
pub const height = size.initial_height;

/// The game uses Entity-Component-System (ECS) design where all game entities
/// are decomposed into data pieces called Components. Components are stored
/// in structure-of-arrays style and an entity is a simple integer that points
/// to the arrays.
pub const World = struct {
    entities: entity.Entities,
    position: position.Storage,
    velocity: velocity.Storage,
    rotation: rotation.Storage,
    mass: mass.Storage,
    radius: radius.Storage,
    player: player.Storage,
    size: size.Storage,

    /// Initialize internal storages, allocates memory for them. Return non zero
    /// result on error.
    pub fn init() !World {
        var w = World {
            .entities = entity.Entities.init(),
            .position = position.Storage.init(),
            .velocity = velocity.Storage.init(),
            .rotation = rotation.Storage.init(),
            .mass = mass.Storage.init(),
            .radius = radius.Storage.init(),
            .player = player.Storage.init(),
            .size = size.Storage.init(),
        };
        _ = try w.spawn_player();
        return w;
    }

    /// Deallocate internal storages and free memory.
    pub fn deinit(self: *World) void {
        self.entities.deinit();
        self.position.deinit();
        self.velocity.deinit();
        self.rotation.deinit();
        self.mass.deinit();
        self.radius.deinit();
        self.player.deinit();
        self.size.deinit();
    }

    ///  Make one tick of world simulation with given inputs. Return non zero if failed.
    pub fn step(self: *World, dt: f64, events: *const input.Events) !void {

    }

    /// Render world in current frame
    pub fn render(self: *const World, renderer: *c.SDL_Renderer) void {
        if (self.player.unique) |p| {
            const pos = self.position.get(self.player.owner) orelse unreachable;
            const rot = self.rotation.get(self.player.owner) orelse unreachable;
            r.render_player(renderer, p, pos, rot);
        }
    }

    /// Create entity for player and fill it with initial values
    pub fn spawn_player(self: *World) !Entity {
        const x = @intToFloat(f32, self.size.global.width) * 0.5;
        const y = @intToFloat(f32, self.size.global.height) * 0.5;
        const e = try self.entities.new();
        self.player.insert(e, player.Player { .thrust = false, .fire_cooldown = 0 });
        try self.position.insert(e, Vec2 { .x = x, .y = y });
        try self.velocity.insert(e, Vec2 { .x = 0, .y = 0 });
        try self.rotation.insert(e, 0);
        try self.mass.insert(e, player.mass);
        try self.radius.insert(e, player.collision_radius);
        return e;
    }
};
