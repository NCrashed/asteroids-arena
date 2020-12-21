const asteroid = @import("../component/asteroid.zig");
const bullet = @import("../component/bullet.zig");
const player = @import("../component/player.zig");
const position = @import("../component/position.zig");
const velocity = @import("../component/velocity.zig");
const radius = @import("../component/radius.zig");
const mass = @import("../component/mass.zig");
const size = @import("../component/size.zig");
const rotation = @import("../component/rotation.zig");
const sound = @import("../component/sound.zig");
const entity = @import("../entity.zig");
const v2 = @import("../v2.zig");
const Component = @import("../component.zig").Component;
const Entity = entity.Entity;
const Entities = entity.Entities;
const std = @import("std");

const DefaultPrng = std.rand.DefaultPrng;
const Vec2 = v2.Vec2;

/// Apply velocities to positions and calculate collisions
pub fn step(entities: *Entities, rng: *DefaultPrng, pos_store: *position.Storage, vel_store: *velocity.Storage, asteroid_store: *asteroid.Storage, player_store: *player.Storage, rad_store: *radius.Storage, mass_store: *mass.Storage, rot_store: *rotation.Storage, bullet_store: *bullet.Storage, sound_store: *sound.Storage, ws: size.WorldSize, dt: f64) !void {
    for (entities.alive.items) |_, i| {
        // Apply vels and warping of space
        try process_movement(entities, pos_store, vel_store, ws, i, dt);
        // Collisions for asteroids
        try process_asteroids(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, bullet_store, player_store, sound_store, ws, i);
        // Collisions for bullets
        try process_bullets(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, bullet_store, player_store, sound_store, ws, i);
    }
}

/// Process movement of entity
fn process_movement(
    entities: *Entities,
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
fn process_asteroids(
    entities: *Entities,
    rng: *DefaultPrng,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    rot_store: *rotation.Storage,
    rad_store: *radius.Storage,
    mass_store: *mass.Storage,
    asteroid_store: *asteroid.Storage,
    bullet_store: *bullet.Storage,
    player_store: *player.Storage,
    sound_store: *sound.Storage,
    ws: size.WorldSize,
    i: usize,
) !void {
    const e = entities.alive.items[i];
    comptime const ac = Component.combine(.{ Component.asteroid, Component.position, Component.radius });
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
            if (ppos.dist_squared(apos) <= r * r) {
                player_store.unique = player.Player{ .thrust = false, .fire_cooldown = player.fire_cooldown };
                ppos.x = @intToFloat(f32, ws.width) * 0.5;
                ppos.y = @intToFloat(f32, ws.height) * 0.5;
                prot.* = 0.0;
                pvel.* = v2.Vec2{ .x = 0, .y = 0 };
            }
        }

        // Collision with bullets
        var k: usize = i + 1;
        while (k < entities.alive.items.len) {
            const be = entities.alive.items[k];

            const bc = Component.combine(.{
                Component.bullet, Component.position,
            });
            if (try entities.alive_has(k, bc)) {
                try bullet_collision(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, bullet_store, sound_store, ws, be, e);
            }
            k += 1;
        }
    }
}

// Process collisions with asteroid
fn process_bullets(
    entities: *Entities,
    rng: *DefaultPrng,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    rot_store: *rotation.Storage,
    rad_store: *radius.Storage,
    mass_store: *mass.Storage,
    asteroid_store: *asteroid.Storage,
    bullet_store: *bullet.Storage,
    player_store: *player.Storage,
    sound_store: *sound.Storage,
    ws: size.WorldSize,
    i: usize,
) !void {
    const be = entities.alive.items[i];
    comptime const bc = Component.combine(.{ Component.bullet, Component.position });
    if (try entities.alive_has(i, bc)) {
        // Collision with asteroids
        var k: usize = i + 1;
        while (k < entities.alive.items.len) {
            const ae = entities.alive.items[k];
            comptime const ac = Component.combine(.{ Component.asteroid, Component.position, Component.radius });
            if (try entities.alive_has(k, ac)) {
                try bullet_collision(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, bullet_store, sound_store, ws, be, ae);
            }
            k += 1;
        }
    }
}
/// Check collision between bullet and asteroid and break asteroid on
// collision.
fn bullet_collision(entities: *Entities, rng: *DefaultPrng, pos_store: *position.Storage, vel_store: *velocity.Storage, rot_store: *rotation.Storage, rad_store: *radius.Storage, mass_store: *mass.Storage, asteroid_store: *asteroid.Storage, bullet_store: *bullet.Storage, sound_store: *sound.Storage, ws: size.WorldSize, be: Entity, ae: Entity) !void {
    const apos = pos_store.get(ae) orelse unreachable;
    const avel = vel_store.get(ae) orelse unreachable;
    const arad = rad_store.get(ae) orelse unreachable;
    const arot = rot_store.get(ae) orelse unreachable;
    const bpos = pos_store.get(be) orelse unreachable;

    const r = arad + bullet.radius;
    if (bpos.dist_squared(apos) <= r * r) {
        try entities.destroy(ae);
        try entities.destroy(be);
        _ = try spawn_shard(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, apos, avel, arot, arad);
        _ = try spawn_shard(entities, rng, pos_store, vel_store, rot_store, rad_store, mass_store, asteroid_store, apos, avel, arot, arad);
        sound_store.global.play(sound.Sound.bang_medium);
    }
}

/// Spawn shard of asteroid with inheritance of position and velocity
fn spawn_shard(
    entities: *Entities,
    rng: *DefaultPrng,
    pos_store: *position.Storage,
    vel_store: *velocity.Storage,
    rot_store: *rotation.Storage,
    rad_store: *radius.Storage,
    mass_store: *mass.Storage,
    asteroid_store: *asteroid.Storage,
    parent_pos: Vec2,
    parent_vel: Vec2,
    parent_rot: f32,
    parent_rad: f32,
) !?Entity {
    const rad = parent_rad * 0.5;
    if (rad >= asteroid.size_min) {
        const mkint = rng.random.intRangeLessThan;

        const m = std.math.pi * rad * rad * asteroid.density;
        const edges = mkint(i32, asteroid.edges_min, asteroid.edges_max);

        const vx = @intToFloat(f32, mkint(i32, asteroid.velocity_min, asteroid.velocity_max));
        const vy = @intToFloat(f32, mkint(i32, asteroid.velocity_min, asteroid.velocity_max));
        var vel = Vec2{ .x = vx, .y = vy };
        _ = vel.add(parent_vel);

        const e = try entities.new();
        try asteroid_store.insert(e, asteroid.Asteroid{ .edges = edges });
        try pos_store.insert(e, parent_pos);
        try vel_store.insert(e, vel);
        try rot_store.insert(e, parent_rot);
        try mass_store.insert(e, m);
        try rad_store.insert(e, rad);
        const components = Component.combine(.{
            Component.asteroid,
            Component.position,
            Component.velocity,
            Component.rotation,
            Component.mass,
            Component.radius,
        });

        try entities.add_component(e, components);
        return e;
    } else {
        return null;
    }
}
