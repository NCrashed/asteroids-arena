const player = @import("../component/player.zig");
const rotation = @import("../component/rotation.zig");
const mass = @import("../component/mass.zig");
const velocity = @import("../component/velocity.zig");

pub fn step(player_store: *player.Storage, rot_store: *const rotation.Storage, vel_store: *velocity.Storage, mass_store: *const mass.Storage, dt: f64) void {
    if (player_store.unique) |p| {
        const e = player_store.owner;

        if (p.thrust) {
            const rot = rot_store.get(e) orelse unreachable;
            const m = mass_store.get(e) orelse unreachable;
            const acc = @floatCast(f32, dt) * player.thrust / m;
            var vel = vel_store.get_ptr(e) orelse unreachable;
            vel.*.x += @cos(rot) * acc;
            vel.*.y += @sin(rot) * acc;
        }
        player_store.unique.?.thrust = false;
        if (p.fire_cooldown > 0) {
            player_store.unique.?.fire_cooldown -= @floatCast(f32, dt);
        }
    }
}
