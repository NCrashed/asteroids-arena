const std = @import("std");
const Builder = std.build.Builder;
const builtin = @import("builtin");

pub fn build(b: *Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("asteroids", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);

    // Adjust to your binary installation of SDL2
    if (builtin.os.tag == std.Target.Os.Tag.windows) {
        exe.addIncludeDir("C:\\SDL2-2.0.12\\include"); // I moved all .h files to SDL2 subfolder
        exe.addLibPath("C:\\SDL2-2.0.12\\lib\\x64");
    }

    exe.addIncludeDir("./include");
    // lib.addCSourceFile("src/lib.c", &[_][]const u8{
    //     "-Wall",
    //     "-Wextra",
    //     "-Werror",
    // });

    exe.linkSystemLibrary("SDL2");
    exe.linkSystemLibrary("SDL2_mixer");
    exe.linkSystemLibrary("c");
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
