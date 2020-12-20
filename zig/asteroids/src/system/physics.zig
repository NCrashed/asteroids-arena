const asteroid = @import("../component/asteroid.zig");
const bullet = @import("../component/bullet.zig");
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
pub fn step(entities: *Entities,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    asteroid_store: *asteroid.Storage,
    player_store: *player.Storage,
    rad_store: *radius.Storage,
    rot_store: *rotation.Storage,
    bullet_store: *bullet.Storage,
    ws: size.WorldSize,
    dt: f64) !void
{
    for (entities.alive.items) |_, i| {
        // Apply vels and warping of space
        try process_movement(entities, pos_store, vel_store, ws, i, dt);
        // Collisions for asteroids
        try process_asteroids(entities, pos_store, vel_store, rot_store,
            rad_store, asteroid_store, bullet_store, player_store, ws, i);
    }
}

/// Process movement of entity
fn process_movement(entities: *Entities,
    pos_store: *position.Storage,
    vel_store: *const velocity.Storage,
    ws: size.WorldSize,
    i: usize,
    dt: f64,
) !void {
    const e = entities.alive.items[i];
    if (try entities.alive_has(i, Component.position)) {
        var pos = pos_store.get_ptr(e) orelse unreachable;

        // Applying velocities
        if (try entities.alive_has(i, Component.velocity)) {
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
}

// Process collisions with asteroid
fn process_asteroids(entities: *Entities,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    rot_store: *rotation.Storage,
    rad_store: *radius.Storage,
    asteroid_store: *asteroid.Storage,
    bullet_store: *bullet.Storage,
    player_store: *player.Storage,
    ws: size.WorldSize,
    i: usize,
    ) !void
{
    const e = entities.alive.items[i];
    comptime const ac = Component.combine(.{Component.asteroid, Component.position, Component.radius});
    if (try entities.alive_has(i, ac)) {
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
                player_store.unique = player.Player { .thrust = false, .fire_cooldown = player.fire_cooldown };
                ppos.x = @intToFloat(f32, ws.width) * 0.5;
                ppos.y = @intToFloat(f32, ws.height) * 0.5;
                prot.* = 0.0;
                pvel.* = v2.Vec2 { .x = 0, .y = 0 };
            }
        }

        // Collision with bullets
        var k: usize = i+1;
        while (k < entities.alive.items.len) {
            const be = entities.alive.items[k];

            const bc = Component.combine(.{
                Component.bullet, Component.position
            });
            if (try entities.alive_has(k, bc)) {
                try bullet_collision(entities, pos_store, vel_store,
                    rot_store, rad_store, asteroid_store, bullet_store, ws,
                    be, e);
            }
            k += 1;
        }
    }
}

/// Check collision between bullet and asteroid and break asteroid on
// collision.
fn bullet_collision(entities: *Entities,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    rot_store: *rotation.Storage,
    rad_store: *radius.Storage,
    asteroid_store: *asteroid.Storage,
    bullet_store: *bullet.Storage,
    ws: size.WorldSize,
    be: Entity, ae: Entity
    ) !void
{
    const apos = pos_store.get(ae) orelse unreachable;
    const arad = rad_store.get(ae) orelse unreachable;
    const bpos = pos_store.get(be) orelse unreachable;

    const r = arad + bullet.radius;
    if (bpos.dist_squared(apos) <= r*r) {
        try entities.destroy(ae);
        try entities.destroy(be);
    }
}
