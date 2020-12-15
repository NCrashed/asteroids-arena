const c = @cImport({
    @cInclude("SDL2/SDL.h");
});
const std = @import("std");
const world = @import("world.zig");
const input = @import("input.zig");

extern fn SDL_PollEvent(event: *c.SDL_Event) c_int;

pub fn process_events(input_events: *input.Events) bool {
    var event: c.SDL_Event = undefined;
    while (SDL_PollEvent(&event) != 0) {
        switch (event.@"type") {
            c.SDL_QUIT => {
                return true;
            },
            c.SDL_KEYDOWN => {
                if (event.key.keysym.sym == c.SDLK_ESCAPE) {
                    return true;
                }
            },
            else => {},
        }
    }
    return false;
}

pub fn main() !void {
    if (c.SDL_Init(c.SDL_INIT_VIDEO) != 0) {
        c.SDL_Log("Unable to initialize SDL: %s", c.SDL_GetError());
        return error.SDLInitializationFailed;
    }
    defer c.SDL_Quit();

    const screen = c.SDL_CreateWindow("My Game Window", c.SDL_WINDOWPOS_UNDEFINED, c.SDL_WINDOWPOS_UNDEFINED, world.width, world.height, c.SDL_WINDOW_OPENGL) orelse
        {
        c.SDL_Log("Unable to create window: %s", c.SDL_GetError());
        return error.SDLInitializationFailed;
    };
    defer c.SDL_DestroyWindow(screen);

    const renderer = c.SDL_CreateRenderer(screen, -1, 0) orelse {
        c.SDL_Log("Unable to create renderer: %s", c.SDL_GetError());
        return error.SDLInitializationFailed;
    };
    defer c.SDL_DestroyRenderer(renderer);

    var w = world.World.init() catch |err| {
        c.SDL_Log("Unable to create world: %s", err);
        return error.WorldInitFail;
    };
    defer w.deinit();

    var timer = std.time.Timer.start() catch |_| {
        c.SDL_Log("Unable to start timer");
        return error.TimerInitFail;
    };
    var i : i32 = 1;
    var quit = false;
    var input_events = input.Events.init();
    while (!quit) {
        quit = process_events(&input_events);

        _ = c.SDL_RenderClear(renderer);

        c.SDL_RenderPresent(renderer);

        const dt = @intToFloat(f64, timer.lap()) * 0.000_000_001;
        w.step(dt, &input_events) catch |err| {
            c.SDL_Log("Unable to step world: %s", err);
            return error.WorldStepFail;
        };

        const fps = 1 / dt;
        i += 1;
        if (@mod(i, 1000) == 0) {
            c.SDL_Log("%f", fps);
        }
    }
}
