const position = @import("../component/position.zig");
const velocity = @import("../component/velocity.zig");
const entity = @import("../entity.zig");
const Component = @import("../component.zig").Component;
const Entity = entity.Entity;
const Entities = entity.Entities;
const size = @import("../component/size.zig");

/// Apply velocities to positions and calculate collisions
pub fn step(entities: *const Entities,
    pos_store: *position.Storage,
    vel_store: *const velocity.Storage,
    ws: size.WorldSize,
    dt: f64) !void
{
    const c = Component.combine(.{Component.position, Component.velocity});
    var i: usize = 0;
    while (i < entities.alive.items.len) {
        const e = entities.alive.items[i];
        if (try entities.has(e, Component.position)) {
            var pos = pos_store.get_ptr(e) orelse unreachable;

            // Applying velocities
            if (try entities.has(e, Component.velocity)) {
                var vel = vel_store.get(e) orelse unreachable;
                _ = vel.scale(@floatCast(f32, dt));
                _ = pos.add(vel);
            }
        
            // Warping space
            const x = pos.x;
            const w = @intToFloat(f32, ws.width);
            if (x < 0) {
                pos.*.x += w;
            } else if (x >= w) {
                pos.*.x -= w;
            }

            const y = pos.y;
            const h = @intToFloat(f32, ws.height);
            if (y < 0) {
                pos.*.y += h;
            } else if (y >= h) {
                pos.*.y -= h;
            }
        }
        i += 1;
    }
}
