const bullet = @import("../component/bullet.zig");
const entity = @import("../entity.zig");
const Component = @import("../component.zig").Component;

/// Destroy bullets when their life time comes to end
pub fn step(bullet_store: *bullet.Storage, entities: *entity.Entities, dt: f64) !void {
    var i: usize = 0;
    while (i < entities.alive.items.len) {
        const e = entities.alive.items[i];
        if (try entities.has(e, Component.bullet)) {
            var b = bullet_store.get_ptr(e) orelse unreachable;
            b.time -= @floatCast(f32, dt);
            if (b.time < 0) {
                _ = try entities.destroy(e);
            }
        }
        i += 1;
    }
}
