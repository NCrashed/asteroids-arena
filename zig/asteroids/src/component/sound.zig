const c = @import("../sdl.zig").c;
const std = @import("std");
const st = @import("../storage/global.zig");

const Allocator = std.mem.Allocator;
const TypeId = std.builtin.TypeId;

/// Sound tag, all known sounds by the game.
pub const Sound = packed enum(u16) {
    bang_medium, fire, thrust
};

/// Maximum amount of used sounds
pub const sounds_count: usize = std.meta.fields(Sound).len;

/// Get unique cooldown time for a sound
pub fn sound_cooldown(s: Sound) f32 {
    return switch (s) {
        .bang_medium => 0.4,
        .fire => 0.2,
        .thrust => 0.2,
    };
}

/// Get unique filename for a sound relative to sounds dir
pub fn sound_file(s: Sound) []const u8 {
    return switch (s) {
        .bang_medium => "bangMedium.wav",
        .fire => "fire.wav",
        .thrust => "thrust.wav",
    };
}

/// Global component that holds sound files and cooldowns for them
pub const SoundResources = struct {
    sounds: [sounds_count]*c.Mix_Chunk,
    cooldowns: [sounds_count]f32,
    allocator: *Allocator,

    /// Load sound resources
    pub fn init(allocator: *Allocator, dir: []const u8) !SoundResources {
        _ = c.Mix_AllocateChannels(sounds_count);
        _ = c.Mix_Volume(-1, 50);

        var sounds: [sounds_count]*c.Mix_Chunk = undefined;
        inline for (std.meta.fields(Sound)) |efield| {
            const s = @intToEnum(Sound, efield.value);
            sounds[@intCast(usize, efield.value)] = try load_wav(allocator, dir, sound_file(s));
        }

        return SoundResources{
            .sounds = sounds,
            .cooldowns = [_]f32{0} ** sounds_count,
            .allocator = allocator,
        };
    }

    /// Load wav file from given directory, caller owns the result.
    fn load_wav(allocator: *Allocator, dir: []const u8, file: []const u8) !*c.Mix_Chunk {
        const a = [_][]const u8{ dir, file };
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
        for (self.sounds) |s| {
            _ = c.Mix_FreeChunk(s);
        }
    }

    /// Update cooldown for sound channels
    pub fn update_cooldowns(self: *SoundResources, dt: f64) void {
        for (self.cooldowns) |*v| {
            if (v.* > 0) {
                v.* -= @floatCast(f32, dt);
            }
        }
    }

    /// Play given sound if cooldown is over
    pub fn play(self: *SoundResources, sound: Sound) void {
        const channel = @enumToInt(sound);
        if (self.cooldowns[channel] <= 0) {
            _ = c.Mix_PlayChannel(-1, self.sounds[channel], 0);
            self.cooldowns[channel] = sound_cooldown(sound);
        }
    }
};

/// We store global single component in world
pub const Storage = st.GlobalStorage(SoundResources, SoundResources.deinit);
