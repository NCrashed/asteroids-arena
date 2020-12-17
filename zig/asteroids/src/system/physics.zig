const position = @import("../component/position.zig");
const velocity = @import("../component/velocity.zig");
const entity = @import("../entity.zig");
const Component = @import("../component.zig").Component;
const Entity = entity.Entity;
const Entities = entity.Entities;

/// Apply velocities to positions and calculate collisions
pub fn step(entities: *const Entities,
    pos_store: *position.Storage,
    vel_store: *const velocity.Storage,
    dt: f64) !void
{
    const c = Component.combine(.{Component.position, Component.velocity});
    var i: usize = 0;
    while (i < entities.alive.items.len) {
        const e = entities.alive.items[i];
        if (try entities.has(e, c)) {
            var pos = pos_store.get_ptr(e) orelse unreachable;
            var vel = vel_store.get(e) orelse unreachable;
            _ = vel.scale(@floatCast(f32, dt));
            _ = pos.add(vel);
        }
        i += 1;
    }
}
