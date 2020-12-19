const c = @import("sdl.zig").c;
const std = @import("std");
const asteroid = @import("component/asteroid.zig");
const player = @import("component/player.zig");
const v2 = @import("v2.zig");
const Vec2 = v2.Vec2;
const debug = @import("std").debug;

const player_dx = 0.5 * player.render_width;
const player_dy = 0.5 * player.render_height;

inline fn render_line(renderer: *c.SDL_Renderer, pos: Vec2, rot: f32, p1: *Vec2, p2: *Vec2) void {
    _ = p1.rotate(rot);
    _ = p2.rotate(rot);
    const x1 = @floatToInt(c_int, pos.x + p1.x);
    const y1 = @floatToInt(c_int, pos.y + p1.y);
    const x2 = @floatToInt(c_int, pos.x + p2.x);
    const y2 = @floatToInt(c_int, pos.y + p2.y);
    _ = c.SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
}

inline fn render_lines(renderer: *c.SDL_Renderer, comptime n: usize, k: usize, vs: [n]Vec2) void {
    var points : [n]c.SDL_Point = undefined;
    var i: usize = 0;
    while (i < k) {
        points[i].x = @floatToInt(c_int, vs[i].x);
        points[i].y = @floatToInt(c_int, vs[i].y);
        i += 1;
    }

    _ = c.SDL_RenderDrawLines(renderer, &points, @intCast(c_int, k));
}

inline fn transform_vecs(pos: Vec2, rot: f32, comptime n: usize, k: usize, vs: *[n]Vec2) void {
    var i: usize = 0;
    while (i < k) {
        _ = vs[i].rotate(rot).add(pos);
        i += 1;
    }
}

pub fn render_player(renderer: *c.SDL_Renderer, pl: player.Player, pos: Vec2, rot: f32) void {
    var p1 = Vec2 { .x =  player_dx, .y =  0 };
    var p2 = Vec2 { .x = -player_dx, .y = -player_dy };
    render_line(renderer, pos, rot, &p1, &p2);

    p1 = Vec2 { .x =  player_dx, .y =  0 };
    p2 = Vec2 { .x = -player_dx, .y =  player_dy};
    render_line(renderer, pos, rot, &p1, &p2);

    p1 = Vec2 { .x = -player_dx, .y = -player_dy };
    p2 = Vec2 { .x = -player_dx, .y =  player_dy};
    render_line(renderer, pos, rot, &p1, &p2);

    if (pl.thrust) {
        p1 = Vec2 { .x = -player_dx   , .y = -0.5*player_dy };
        p2 = Vec2 { .x = -player_dx-10, .y =  0};
        render_line(renderer, pos, rot, &p1, &p2);
        p1 = Vec2 { .x = -player_dx   , .y =  0.5*player_dy };
        p2 = Vec2 { .x = -player_dx-10, .y =  0};
        render_line(renderer, pos, rot, &p1, &p2);
    }
}

inline fn circloid_point(i: i32, n: i32, r: f32) Vec2 {
    const a = @intToFloat(f32, i) * (2 * std.math.pi / @intToFloat(f32, n));
    return Vec2 {
        .x = r * (1 + @sin(a) * 0.3) * @cos(a),
        .y = r * (1 + @cos(a) * 0.3) * @sin(a),
    };
}

pub fn render_asteroid(renderer: *c.SDL_Renderer, a: asteroid.Asteroid, pos: Vec2, rot: f32, rad: f32) void {
    var i: usize = 0;
    const n = a.edges;
    const maxn = asteroid.edges_max * 2;
    var ps : [maxn]Vec2 = undefined;
    while (i <= n-1) {
        ps[2*i] = circloid_point(@intCast(i32, i), n, rad);
        ps[2*i+1] = circloid_point(@intCast(i32, i+1), n, rad);
        i += 1;
    }
    const k = @intCast(usize, 2*n);
    transform_vecs(pos, rot, maxn, k, &ps);
    render_lines(renderer, maxn, k, ps);
}
