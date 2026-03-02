const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // TLS dependency
    const tls_dep = b.dependency("tls", .{
        .target = target,
        .optimize = optimize,
    });
    const tls_module = tls_dep.module("tls");

    // oxelot-http library module
    const http_module = b.addModule("http", .{
        .root_source_file = b.path("src/http.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "tls", .module = tls_module },
        },
    });

    // Add picohttpparser include path to module
    http_module.addIncludePath(b.path("lib/picohttpparser"));
    http_module.link_libc = true;

    // PostgreSQL module (optional - link libpq when using pg features)
    const pg_module = b.addModule("pg", .{
        .root_source_file = b.path("src/pg.zig"),
        .target = target,
        .optimize = optimize,
    });
    pg_module.link_libc = true;
    pg_module.linkSystemLibrary("pq", .{});

    // Metrics module
    const metrics_module = b.addModule("metrics", .{
        .root_source_file = b.path("src/metrics.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Tracing module
    const tracing_module = b.addModule("tracing", .{
        .root_source_file = b.path("src/tracing.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Unit tests
    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/http.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "tls", .module = tls_module },
            },
        }),
    });
    tests.root_module.addIncludePath(b.path("lib/picohttpparser"));
    tests.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    tests.root_module.link_libc = true;

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);

    // PostgreSQL module tests
    const pg_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/pg.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    pg_tests.root_module.link_libc = true;
    pg_tests.root_module.linkSystemLibrary("pq", .{});

    test_step.dependOn(&b.addRunArtifact(pg_tests).step);

    // Metrics module tests
    const metrics_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/metrics.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_step.dependOn(&b.addRunArtifact(metrics_tests).step);

    // Tracing module tests
    const tracing_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/tracing.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_step.dependOn(&b.addRunArtifact(tracing_tests).step);

    // Example: Simple HTTP server
    const example_simple = b.addExecutable(.{
        .name = "example-simple",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/simple_server.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_simple.root_module.addImport("http", http_module);
    example_simple.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_simple.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_simple.root_module.link_libc = true;
    b.installArtifact(example_simple);

    // Example: JSON API server
    const example_json = b.addExecutable(.{
        .name = "example-json-api",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/json_api.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_json.root_module.addImport("http", http_module);
    example_json.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_json.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_json.root_module.link_libc = true;
    b.installArtifact(example_json);

    // Run example step
    const run_simple = b.addRunArtifact(example_simple);
    const run_step = b.step("run", "Run the simple example server");
    run_step.dependOn(&run_simple.step);

    // Benchmark executable
    const benchmark = b.addExecutable(.{
        .name = "benchmark",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/benchmark_server.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });
    benchmark.root_module.addImport("http", http_module);
    benchmark.root_module.addIncludePath(b.path("lib/picohttpparser"));
    benchmark.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    benchmark.root_module.link_libc = true;
    b.installArtifact(benchmark);

    const run_benchmark = b.addRunArtifact(benchmark);
    const benchmark_step = b.step("benchmark", "Run the benchmark server");
    benchmark_step.dependOn(&run_benchmark.step);

    // Example: Database server (requires libpq)
    const example_db = b.addExecutable(.{
        .name = "example-db-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/db_server.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_db.root_module.addImport("http", http_module);
    example_db.root_module.addImport("pg", pg_module);
    example_db.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_db.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_db.root_module.link_libc = true;
    example_db.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(example_db);

    const run_db = b.addRunArtifact(example_db);
    const db_step = b.step("db-server", "Run the database example server");
    db_step.dependOn(&run_db.step);

    // Example: WebSocket echo server
    const example_ws = b.addExecutable(.{
        .name = "example-websocket",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/websocket_echo.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_ws.root_module.addImport("http", http_module);
    example_ws.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_ws.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_ws.root_module.link_libc = true;
    b.installArtifact(example_ws);

    const run_ws = b.addRunArtifact(example_ws);
    const ws_step = b.step("ws-server", "Run the WebSocket echo server");
    ws_step.dependOn(&run_ws.step);

    // WebSocket Echo Benchmark (optimized, no logging)
    const ws_bench = b.addExecutable(.{
        .name = "ws-echo-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/ws_echo_bench.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });
    ws_bench.root_module.addImport("http", http_module);
    ws_bench.root_module.addIncludePath(b.path("lib/picohttpparser"));
    ws_bench.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    ws_bench.root_module.link_libc = true;
    b.installArtifact(ws_bench);

    const run_ws_bench = b.addRunArtifact(ws_bench);
    const ws_bench_step = b.step("ws-bench", "Run WebSocket echo benchmark server");
    ws_bench_step.dependOn(&run_ws_bench.step);

    // Example: Middleware demonstration
    const example_middleware = b.addExecutable(.{
        .name = "example-middleware",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/middleware.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_middleware.root_module.addImport("http", http_module);
    example_middleware.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_middleware.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_middleware.root_module.link_libc = true;
    b.installArtifact(example_middleware);

    const run_middleware = b.addRunArtifact(example_middleware);
    const middleware_step = b.step("middleware", "Run the middleware example server");
    middleware_step.dependOn(&run_middleware.step);

    // Example: Static file server
    const example_static = b.addExecutable(.{
        .name = "example-static-server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/static_server.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_static.root_module.addImport("http", http_module);
    example_static.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_static.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_static.root_module.link_libc = true;
    b.installArtifact(example_static);

    const run_static = b.addRunArtifact(example_static);
    const static_step = b.step("static-server", "Run the static file server example");
    static_step.dependOn(&run_static.step);

    // Example: Database migrations (requires libpq)
    const example_migrations = b.addExecutable(.{
        .name = "example-db-migrations",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/db_migrations.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_migrations.root_module.addImport("pg", pg_module);
    example_migrations.root_module.link_libc = true;
    example_migrations.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(example_migrations);

    const run_migrations = b.addRunArtifact(example_migrations);
    const migrations_step = b.step("migrations", "Run the database migrations example");
    migrations_step.dependOn(&run_migrations.step);

    // Example: Struct mapping (requires libpq)
    const example_models = b.addExecutable(.{
        .name = "example-db-models",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/db_models.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_models.root_module.addImport("pg", pg_module);
    example_models.root_module.link_libc = true;
    example_models.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(example_models);

    const run_models = b.addRunArtifact(example_models);
    const models_step = b.step("models", "Run the struct mapping example");
    models_step.dependOn(&run_models.step);

    // Example: HTTP client
    const example_client = b.addExecutable(.{
        .name = "example-http-client",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/http_client.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_client.root_module.addImport("http", http_module);
    example_client.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_client.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_client.root_module.link_libc = true;
    b.installArtifact(example_client);

    const run_client = b.addRunArtifact(example_client);
    const client_step = b.step("http-client", "Run the HTTP client example");
    client_step.dependOn(&run_client.step);

    // Example: Observability (metrics + tracing)
    const example_observability = b.addExecutable(.{
        .name = "example-observability",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/observability.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    example_observability.root_module.addImport("http", http_module);
    example_observability.root_module.addImport("metrics", metrics_module);
    example_observability.root_module.addImport("tracing", tracing_module);
    example_observability.root_module.addIncludePath(b.path("lib/picohttpparser"));
    example_observability.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    example_observability.root_module.link_libc = true;
    b.installArtifact(example_observability);

    const run_observability = b.addRunArtifact(example_observability);
    const observability_step = b.step("observability", "Run the observability example server");
    observability_step.dependOn(&run_observability.step);

    // TechEmpower Framework Benchmark server
    const techempower = b.addExecutable(.{
        .name = "techempower",
        .root_module = b.createModule(.{
            .root_source_file = b.path("examples/techempower.zig"),
            .target = target,
            .optimize = .ReleaseFast,
        }),
    });
    techempower.root_module.addImport("http", http_module);
    techempower.root_module.addImport("pg", pg_module);
    techempower.root_module.addIncludePath(b.path("lib/picohttpparser"));
    techempower.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    techempower.root_module.link_libc = true;
    techempower.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(techempower);

    const build_techempower_step = b.step("build-techempower", "Build the TechEmpower benchmark server");
    const install_techempower = b.addInstallArtifact(techempower, .{});
    build_techempower_step.dependOn(&install_techempower.step);

    const run_techempower = b.addRunArtifact(techempower);
    const techempower_step = b.step("techempower", "Run the TechEmpower benchmark server");
    techempower_step.dependOn(&run_techempower.step);

    // PostgreSQL Integration Tests (requires Docker)
    const pg_integration = b.addExecutable(.{
        .name = "pg-integration-test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/pg_integration_test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    pg_integration.root_module.addImport("pg", pg_module);
    pg_integration.root_module.link_libc = true;
    pg_integration.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(pg_integration);

    const run_pg_integration = b.addRunArtifact(pg_integration);
    const pg_integration_step = b.step("pg-integration-test", "Run PostgreSQL integration tests (requires Docker)");
    pg_integration_step.dependOn(&run_pg_integration.step);

    // Observability Integration Tests (requires Docker: PostgreSQL, Prometheus, Jaeger)
    const obs_integration = b.addExecutable(.{
        .name = "observability-integration-test",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tests/observability_integration_test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    obs_integration.root_module.addImport("http", http_module);
    obs_integration.root_module.addImport("metrics", metrics_module);
    obs_integration.root_module.addImport("tracing", tracing_module);
    obs_integration.root_module.addImport("pg", pg_module);
    obs_integration.root_module.addIncludePath(b.path("lib/picohttpparser"));
    obs_integration.root_module.addCSourceFile(.{
        .file = b.path("lib/picohttpparser/picohttpparser.c"),
        .flags = &.{"-O3"},
    });
    obs_integration.root_module.link_libc = true;
    obs_integration.root_module.linkSystemLibrary("pq", .{});
    b.installArtifact(obs_integration);

    const run_obs_integration = b.addRunArtifact(obs_integration);
    const obs_integration_step = b.step("observability-integration-test", "Run observability integration tests (requires Docker: PostgreSQL, Prometheus, Jaeger)");
    obs_integration_step.dependOn(&run_obs_integration.step);

    // Lint: Check that all source files are included in test tree
    const check_imports = b.addSystemCommand(&.{
        "bash",
        "scripts/check_test_imports.sh",
    });
    const lint_step = b.step("lint", "Run linting checks");
    lint_step.dependOn(&check_imports.step);

    // Also run lint before tests
    test_step.dependOn(&check_imports.step);
}
