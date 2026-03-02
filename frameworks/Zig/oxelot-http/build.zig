const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const oxelot_dep = b.dependency("oxelot_http", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "techempower",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });

    exe.root_module.addImport("http", oxelot_dep.module("http"));
    exe.root_module.addImport("pg", oxelot_dep.module("pg"));

    // picohttpparser C source (needed by http module)
    exe.root_module.addIncludePath(oxelot_dep.path("lib/picohttpparser"));
    exe.root_module.addCSourceFile(.{
        .file = oxelot_dep.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });

    exe.root_module.link_libc = true;
    exe.root_module.linkSystemLibrary("pq", .{});

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the TechEmpower benchmark server");
    run_step.dependOn(&b.addRunArtifact(exe).step);
}
