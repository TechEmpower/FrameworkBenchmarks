// HTTP Server Metrics Middleware
//
// Provides a simple way to record request metrics.
// Since the metrics module is standalone (not dependent on http module),
// this provides helper functions that can be used from HTTP handlers.
//
// Usage with oxelot-http:
//
//   const metrics = @import("metrics");
//   const http = @import("http");
//
//   // Set up registry
//   var registry = metrics.Registry.init(allocator);
//   metrics.middleware.setRegistry(&registry);
//
//   // Use in a logging-style middleware
//   pub fn metricsMiddleware(ctx: *http.Router.Context, next: http.Next) !void {
//       const timer = metrics.middleware.startRequest();
//       defer metrics.middleware.endRequest(timer, ctx.request.method, ctx.request.path, ctx.response.status.code());
//       return next.call(@ptrCast(ctx));
//   }

const std = @import("std");
const reg = @import("registry.zig");

const Registry = reg.Registry;

/// Module-level registry pointer
/// Must be set via setRegistry() before using the middleware
var global_registry: ?*Registry = null;

/// Set the registry to use for metrics collection
pub fn setRegistry(registry: ?*Registry) void {
    global_registry = registry;
}

/// Get the current registry (if set)
pub fn getRegistry() ?*Registry {
    return global_registry;
}

/// Timer for request duration tracking
pub const Timer = struct {
    start: ?std.time.Instant,

    pub fn elapsed(self: Timer) ?u64 {
        if (self.start) |s| {
            if (std.time.Instant.now()) |end| {
                return end.since(s);
            } else |_| {}
        }
        return null;
    }
};

/// Start timing a request
/// Call this at the beginning of request handling
pub fn startRequest() Timer {
    if (global_registry) |r| {
        r.http_requests_in_flight.inc();
    }
    return .{
        .start = std.time.Instant.now() catch null,
    };
}

/// End timing a request and record metrics
/// Call this at the end of request handling
pub fn endRequest(timer: Timer, method: []const u8, route: []const u8, status_code: u16) void {
    const registry = global_registry orelse return;

    registry.http_requests_in_flight.dec();

    // Record request count
    registry.http_requests_total.inc(.{ method, route, statusToString(status_code) });

    // Record duration
    if (timer.elapsed()) |duration_ns| {
        registry.http_request_duration.observe(.{ method, route }, duration_ns);
    }
}

/// Record a request in one call (for simple use cases)
pub fn recordRequest(method: []const u8, route: []const u8, status_code: u16, duration_ns: u64) void {
    const registry = global_registry orelse return;

    registry.http_requests_total.inc(.{ method, route, statusToString(status_code) });
    registry.http_request_duration.observe(.{ method, route }, duration_ns);
}

/// Convert status code to string for metrics labels
fn statusToString(code: u16) []const u8 {
    return switch (code) {
        200 => "200",
        201 => "201",
        204 => "204",
        301 => "301",
        302 => "302",
        304 => "304",
        400 => "400",
        401 => "401",
        403 => "403",
        404 => "404",
        405 => "405",
        500 => "500",
        502 => "502",
        503 => "503",
        else => "other",
    };
}

// ============================================================================
// Tests
// ============================================================================

test "timer" {
    const timer = Timer{ .start = std.time.Instant.now() catch null };

    // Just verify elapsed() works without crashing
    if (timer.elapsed()) |ns| {
        // Should be a very small value
        try std.testing.expect(ns >= 0);
    }
}

test "record request with registry" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    setRegistry(&registry);
    defer setRegistry(null);

    recordRequest("GET", "/api/test", 200, 1_000_000);

    // Verify counter was incremented
    try std.testing.expectEqual(@as(u64, 1), registry.http_requests_total.get(.{ "GET", "/api/test", "200" }).get());
}

test "start and end request" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    setRegistry(&registry);
    defer setRegistry(null);

    // Check in-flight starts at 0
    try std.testing.expectEqual(@as(i64, 0), registry.http_requests_in_flight.get());

    const timer = startRequest();

    // Check in-flight is now 1
    try std.testing.expectEqual(@as(i64, 1), registry.http_requests_in_flight.get());

    endRequest(timer, "POST", "/api/data", 201);

    // Check in-flight is back to 0
    try std.testing.expectEqual(@as(i64, 0), registry.http_requests_in_flight.get());

    // Verify counter was incremented
    try std.testing.expectEqual(@as(u64, 1), registry.http_requests_total.get(.{ "POST", "/api/data", "201" }).get());
}
