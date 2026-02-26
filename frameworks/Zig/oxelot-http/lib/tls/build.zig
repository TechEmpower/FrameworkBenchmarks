const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tls_module = b.addModule("tls", .{
        .root_source_file = b.path("src/root.zig"),
    });

    const examples = [_][]const u8{
        "http_get",
        // "http_get_std",
        "server",
        "top_sites",
        "badssl",
        // "std_top_sites",
        "all_ciphers",
        "client_auth",
        "fuzz_server",
        "http_get_nonblock",
        "http_get_resumption",
        "http_get2",

        //"client",
        //"client_std",
        // TODO: build tls zig on Linux
        //"ktls",
    };
    inline for (examples) |path| {
        const source_file = "example/" ++ path ++ ".zig";
        const name = comptime if (std.mem.indexOfScalar(u8, path, '/')) |pos| path[0..pos] else path;
        const exe_mod = b.createModule(.{
            .root_source_file = b.path(source_file),
            .target = target,
            .optimize = optimize,
        });
        const exe = b.addExecutable(.{
            .name = name,
            .root_module = exe_mod,
        });
        exe.root_module.addImport("tls", tls_module);
        setupExample(b, exe, name);
    }

    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const unit_tests = b.addTest(.{
        .root_module = lib_mod,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const integration_mod = b.createModule(.{
        .root_source_file = b.path("example/integration_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    const integration_tests = b.addTest(.{
        .root_module = integration_mod,
    });
    integration_tests.root_module.addImport("tls", tls_module);
    const run_integration_tests = b.addRunArtifact(integration_tests);
    const integration_test_step = b.step("integration", "Run integration tests");
    integration_test_step.dependOn(&run_integration_tests.step);
}

// Copied from: https://github.com/karlseguin/mqttz/blob/master/build.zig
fn setupExample(b: *std.Build, exe: *std.Build.Step.Compile, comptime name: []const u8) void {
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("example_" ++ name, "Run the " ++ name ++ " example");
    run_step.dependOn(&run_cmd.step);
}
