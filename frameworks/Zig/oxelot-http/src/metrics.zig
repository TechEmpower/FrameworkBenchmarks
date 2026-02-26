// Metrics Module for oxelot-http
//
// Provides comprehensive metrics collection with:
// - Prometheus text exposition format
// - HTTP server request metrics (middleware)
// - HTTP client request metrics (wrapper)
// - PostgreSQL query and pool metrics (wrapper)
//
// Usage:
//
//   // Option 1: Simple setup with existing router
//   const registry = metrics.setup(&router, allocator, "/metrics");
//
//   // Option 2: Dedicated metrics server on separate port
//   var metrics_srv = try metrics.Server.init(allocator, 9090, "/metrics");
//   defer metrics_srv.deinit();
//
//   // Instrument PostgreSQL pool
//   var db = metrics.instrumentPool(&pg_pool, registry);
//
//   // Instrument HTTP client
//   var api = metrics.instrumentClient(&http_client, registry);

const std = @import("std");

// Re-export types
pub const types = @import("metrics/types.zig");
pub const Counter = types.Counter;
pub const Gauge = types.Gauge;
pub const Histogram = types.Histogram;

// Re-export registry
pub const registry = @import("metrics/registry.zig");
pub const Registry = registry.Registry;
pub const LabeledCounter = registry.LabeledCounter;
pub const LabeledHistogram = registry.LabeledHistogram;

// Re-export prometheus exporter
pub const prometheus = @import("metrics/prometheus.zig");

// Re-export middleware
pub const middleware = @import("metrics/middleware.zig");

// Re-export instrumentation wrappers
pub const pg = @import("metrics/pg.zig");
pub const client = @import("metrics/client.zig");

/// Configuration for metrics setup
pub const Config = struct {
    /// Prometheus endpoint path
    endpoint: []const u8 = "/metrics",
    /// Enable process metrics (memory, etc.)
    enable_process_metrics: bool = true,
};

/// Initialize a new metrics registry
pub fn init(allocator: std.mem.Allocator) Registry {
    return Registry.init(allocator);
}

/// Instrument a PostgreSQL pool for metrics collection
pub fn instrumentPool(comptime Pool: type, pool: *Pool, reg: *Registry) pg.InstrumentedPool(Pool) {
    return pg.InstrumentedPool(Pool).init(pool, reg);
}

/// Instrument an HTTP client for metrics collection
pub fn instrumentClient(comptime Client: type, c: *Client, reg: *Registry) client.InstrumentedClient(Client) {
    return client.InstrumentedClient(Client).init(c, reg);
}

// Note: To use the metrics middleware, call:
//   metrics.middleware.setRegistry(&registry);
//   router.useGlobal(metrics.middleware.metrics);

// ============================================================================
// Tests
// ============================================================================

test "metrics module imports" {
    // Verify all submodules can be imported
    _ = types;
    _ = registry;
    _ = prometheus;
    _ = middleware;
    _ = pg;
    _ = client;
}

test "registry creation" {
    const allocator = std.testing.allocator;
    var reg = Registry.init(allocator);
    defer reg.deinit();

    reg.http_requests_total.inc(.{ "GET", "/", "200" });
    try std.testing.expectEqual(@as(u64, 1), reg.http_requests_total.get(.{ "GET", "/", "200" }).get());
}

test {
    _ = @import("metrics/types.zig");
    _ = @import("metrics/registry.zig");
    _ = @import("metrics/prometheus.zig");
    _ = @import("metrics/middleware.zig");
    _ = @import("metrics/pg.zig");
    _ = @import("metrics/client.zig");
}
