const c = @import("../sdl.zig").c;
const std = @import("std");
const st = @import("../storage/global.zig");

const Allocator = std.mem.Allocator;

/// Maximum amount of used channels
pub const channels: usize = 3;

/// Cooldown for bang sound
pub const bang_cooldown: f32 = 0.4;
/// Cooldown for fire sound
pub const fire_cooldown: f32 = 0.2;
/// Cooldown for thrust sound
pub const thrust_cooldown: f32 = 0.2;

/// Global component that holds sound files and cooldowns for them
pub const SoundResources = struct {
    bang_medium: *c.Mix_Chunk,
    fire: *c.Mix_Chunk,
    thrust: *c.Mix_Chunk,
    allocator: *Allocator,
    cooldowns: [channels]f32,

    /// Load sound resources
    pub fn init(allocator: *Allocator, dir: []const u8) !SoundResources {
        _ = c.Mix_AllocateChannels(channels);
        _ = c.Mix_Volume(-1, 50);

        const bang_medium = try load_wav(allocator, dir, "bangMedium.wav");
        return SoundResources {
            .bang_medium = bang_medium,
            .fire = try load_wav(allocator, dir, "fire.wav"),
            .thrust = try load_wav(allocator, dir, "thrust.wav"),
            .allocator = allocator,
            .cooldowns = [_]f32{0} ** channels,
        };
    }

    /// Load wav file from given directory, caller owns the result.
    fn load_wav(allocator: *Allocator, dir: []const u8, file: []const u8) !*c.Mix_Chunk {
        const a = [_][]const u8 { dir, file };
        const file_path = try std.fs.path.join(allocator, &a);
        defer allocator.free(file_path);

        const c_file_path = try std.cstr.addNullByte(allocator, file_path);
        defer allocator.free(c_file_path);

        std.log.info("Loading {}", .{file_path});
        const res = c.Mix_LoadWAV(c_file_path);
        if (res == null) {
            std.log.err("Failed load WAV {}\n", .{file_path});
            c.SDL_Log("%s", c.SDL_GetError());
            return error.FailedOpenWav;
        }
        return res;
    }

    /// Free sound resources
    pub fn deinit(self: *SoundResources) void {
        _ = c.Mix_FreeChunk(self.bang_medium);
        _ = c.Mix_FreeChunk(self.fire);
        _ = c.Mix_FreeChunk(self.thrust);
    }
};

/// We store global single component in world
pub const Storage = st.GlobalStorage(SoundResources, SoundResources.deinit);
