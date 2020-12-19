const asteroid = @import("../component/asteroid.zig");
const player = @import("../component/player.zig");
const position = @import("../component/position.zig");
const velocity = @import("../component/velocity.zig");
const radius = @import("../component/radius.zig");
const size = @import("../component/size.zig");
const rotation = @import("../component/rotation.zig");
const entity = @import("../entity.zig");
const v2 = @import("../v2.zig");
const Component = @import("../component.zig").Component;
const Entity = entity.Entity;
const Entities = entity.Entities;

/// Apply velocities to positions and calculate collisions
pub fn step(entities: *const Entities,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    astetoid_store: *const asteroid.Storage,
    player_store: *player.Storage,
    rad_store: *const radius.Storage,
    rot_store: *rotation.Storage,
    ws: size.WorldSize,
    dt: f64) !void
{
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
        // Collisions for asteroids
        comptime const ac = Component.combine(.{Component.asteroid, Component.position, Component.radius});
        if (try entities.has(e, ac)) {
            var apos = pos_store.get(e) orelse unreachable;
            var arad = rad_store.get(e) orelse unreachable;

            // Collision with player
            if (player_store.unique) |_| {
                var ppos = pos_store.get_ptr(player_store.owner) orelse unreachable;
                var prad = rad_store.get_ptr(player_store.owner) orelse unreachable;
                var pvel = vel_store.get_ptr(player_store.owner) orelse unreachable;
                var prot = rot_store.get_ptr(player_store.owner) orelse unreachable;

                const r = arad + prad.*;
                if (ppos.dist_squared(apos) <= r*r) {
                    player_store.unique = player.Player { .thrust = false, .fire_cooldown = 0.0 };
                    ppos.x = @intToFloat(f32, ws.width) * 0.5;
                    ppos.y = @intToFloat(f32, ws.height) * 0.5;
                    prot.* = 0.0;
                    pvel.* = v2.Vec2 { .x = 0, .y = 0 };
                }
            }

            // Collision with bullets
            var k: usize = i+1;
            while (k < entities.alive.items.len) {

                k += 1;
            }
        }
        i += 1;
    }
}
