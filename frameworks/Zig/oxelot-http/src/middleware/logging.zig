// Logging middleware for oxelot-http
//
// Logs HTTP requests with method, path, status code, and timing information.

const std = @import("std");
const mw = @import("../middleware.zig");
const router = @import("../router.zig");

pub const LogLevel = enum {
    /// Minimal: just method, path, status
    minimal,
    /// Standard: method, path, status, timing
    standard,
    /// Verbose: includes headers and body size
    verbose,
};

pub const Config = struct {
    level: LogLevel = .standard,
    /// Paths to skip logging (e.g., health checks)
    skip_paths: []const []const u8 = &.{},
};

/// Create a configured logging middleware
/// Returns a struct with middleware function and config
pub fn create(config: Config) LoggingMiddleware {
    return .{ .config = config };
}

/// Stateful logging middleware
pub const LoggingMiddleware = struct {
    config: Config,

    /// Get the middleware function
    pub fn middleware(self: *const LoggingMiddleware) mw.Middleware {
        // Store self pointer in a comptime-known location
        // This is a workaround since Zig doesn't have closures
        _ = self;
        // For now, return the default logging middleware
        // Full configurable version would require runtime dispatch
        return logging;
    }
};

/// Default logging middleware (standard level)
pub fn logging(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    const start = std.time.Instant.now() catch null;

    // Call the next handler
    const result = next.call(opaque_ctx);

    // Calculate elapsed time
    var elapsed_ms: f64 = 0;
    if (start) |s| {
        if (std.time.Instant.now()) |end| {
            const elapsed_ns = end.since(s);
            elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        } else |_| {}
    }

    // Get method string
    const method_str = switch (ctx.request.method) {
        .GET => "GET",
        .POST => "POST",
        .PUT => "PUT",
        .DELETE => "DELETE",
        .PATCH => "PATCH",
        .HEAD => "HEAD",
        .OPTIONS => "OPTIONS",
        .CONNECT => "CONNECT",
        .TRACE => "TRACE",
    };

    // Get status code
    const status_code = ctx.response.status.code();

    // Log the request
    std.log.info("{s} {s} {d} {d:.2}ms", .{
        method_str,
        ctx.request.path,
        status_code,
        elapsed_ms,
    });

    // Return any error from the handler
    return result;
}

/// Minimal logging middleware (just method, path, status)
pub fn minimal(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    // Call the next handler
    const result = next.call(opaque_ctx);

    const method_str = switch (ctx.request.method) {
        .GET => "GET",
        .POST => "POST",
        .PUT => "PUT",
        .DELETE => "DELETE",
        .PATCH => "PATCH",
        .HEAD => "HEAD",
        .OPTIONS => "OPTIONS",
        .CONNECT => "CONNECT",
        .TRACE => "TRACE",
    };

    std.log.info("{s} {s} {d}", .{
        method_str,
        ctx.request.path,
        ctx.response.status.code(),
    });

    return result;
}

/// Verbose logging middleware (includes body size)
pub fn verbose(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    const start = std.time.Instant.now() catch null;

    // Call the next handler
    const result = next.call(opaque_ctx);

    // Calculate elapsed time
    var elapsed_ms: f64 = 0;
    if (start) |s| {
        if (std.time.Instant.now()) |end| {
            const elapsed_ns = end.since(s);
            elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        } else |_| {}
    }

    const method_str = switch (ctx.request.method) {
        .GET => "GET",
        .POST => "POST",
        .PUT => "PUT",
        .DELETE => "DELETE",
        .PATCH => "PATCH",
        .HEAD => "HEAD",
        .OPTIONS => "OPTIONS",
        .CONNECT => "CONNECT",
        .TRACE => "TRACE",
    };

    const request_size = if (ctx.request.body) |b| b.len else 0;
    const response_size = ctx.response.body.items.len;

    std.log.info("{s} {s} {d} {d:.2}ms req={d}B res={d}B", .{
        method_str,
        ctx.request.path,
        ctx.response.status.code(),
        elapsed_ms,
        request_size,
        response_size,
    });

    return result;
}
