const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const root_source_file = b.path("src/main.zig");

    const zzz = b.dependency("zzz", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zzz",
        .target = target,
        .optimize = optimize,
        .root_source_file = root_source_file,
        .strip = true,
    });
    exe.root_module.addImport("zzz", zzz.module("zzz"));
    b.installArtifact(exe);
}
