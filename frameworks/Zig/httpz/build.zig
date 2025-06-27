const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dep_opts = .{ .target = target, .optimize = optimize };

    const exe = b.addExecutable(.{
        .name = 'httpz',
        .root_source_file = b.path("src/main.zig"),
        // .target = target, // Removed for Zig 0.14.0 compatibility
        .optimize = optimize,
    });

    const httpz_module = b.dependency("httpz", dep_opts).module("httpz");
    const pg_module = b.dependency("pg", dep_opts).module("pg");
    const datetimez_module = b.dependency("datetimez", dep_opts).module("zig-datetime");

    exe.root_module.addImport("httpz", httpz_module);
    exe.root_module.addImport("pg", pg_module);
    exe.root_module.addImport("datetimez", datetimez_module);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
