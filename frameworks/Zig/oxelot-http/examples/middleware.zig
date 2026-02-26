// Middleware example for zig-http
//
// Demonstrates the middleware system including:
// - Global middleware (logging, CORS)
// - Per-route middleware (authentication)
// - Middleware chaining
//
// Run with: zig build && ./zig-out/bin/example-middleware
// Test with:
//   curl http://localhost:8080/                     # Public route with logging
//   curl http://localhost:8080/api/data             # CORS-enabled API route
//   curl -u admin:secret http://localhost:8080/admin # Protected route

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var router = http.router(allocator);
    defer router.deinit();

    // Global middleware - runs on ALL routes
    // Order matters: logging runs first, then CORS
    _ = router
        .useGlobal(http.logging.logging) // Log all requests
        .useGlobal(http.cors.cors); // Enable CORS for all routes

    // Public routes (only global middleware applies)
    _ = router
        .get("/", handleHome)
        .get("/health", handleHealth);

    // API routes with compression
    _ = router
        .with(http.compression.compression) // Compress large responses
        .get("/api/data", handleApiData);

    // Protected admin routes with basic auth
    // Custom auth middleware that validates credentials
    _ = router
        .with(authMiddleware)
        .get("/admin", handleAdmin)
        .get("/admin/stats", handleAdminStats);

    var server = http.Server.init(allocator, &router, .{ .threads = 4 });
    defer server.deinit();

    std.log.info("Middleware example server running on http://localhost:8080", .{});
    std.log.info("Routes:", .{});
    std.log.info("  GET /           - Public home page", .{});
    std.log.info("  GET /health     - Health check", .{});
    std.log.info("  GET /api/data   - API endpoint with compression", .{});
    std.log.info("  GET /admin      - Protected route (user: admin, pass: secret)", .{});

    try server.run("0.0.0.0", 8080);
}

fn handleHome(ctx: *http.Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>Middleware Example</title></head>
        \\<body>
        \\  <h1>zig-http Middleware Example</h1>
        \\  <ul>
        \\    <li><a href="/health">Health Check</a></li>
        \\    <li><a href="/api/data">API Data (with compression)</a></li>
        \\    <li><a href="/admin">Admin Panel (requires auth)</a></li>
        \\  </ul>
        \\</body>
        \\</html>
    );
}

fn handleHealth(ctx: *http.Context) !void {
    try ctx.sendJson(.{ .status = "ok" });
}

fn handleApiData(ctx: *http.Context) !void {
    // Return a larger response to demonstrate compression
    const data = .{
        .items = .{
            .{ .id = 1, .name = "Item 1", .description = "This is item 1 with a longer description to help demonstrate compression" },
            .{ .id = 2, .name = "Item 2", .description = "This is item 2 with a longer description to help demonstrate compression" },
            .{ .id = 3, .name = "Item 3", .description = "This is item 3 with a longer description to help demonstrate compression" },
            .{ .id = 4, .name = "Item 4", .description = "This is item 4 with a longer description to help demonstrate compression" },
            .{ .id = 5, .name = "Item 5", .description = "This is item 5 with a longer description to help demonstrate compression" },
        },
        .total = 5,
        .message = "This response may be compressed if Accept-Encoding: gzip is sent",
    };
    try ctx.sendJson(data);
}

fn handleAdmin(ctx: *http.Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>Admin Panel</title></head>
        \\<body>
        \\  <h1>Admin Panel</h1>
        \\  <p>Welcome, authenticated user!</p>
        \\  <ul>
        \\    <li><a href="/admin/stats">View Stats</a></li>
        \\  </ul>
        \\</body>
        \\</html>
    );
}

fn handleAdminStats(ctx: *http.Context) !void {
    try ctx.sendJson(.{
        .requests_total = 1234,
        .active_connections = 42,
        .uptime_seconds = 3600,
    });
}

// Custom authentication middleware
fn authMiddleware(opaque_ctx: *http.mw.Context, next: http.Next) anyerror!void {
    const ctx: *http.Context = @ptrCast(@alignCast(opaque_ctx));

    const auth_header = ctx.request.headers.get("Authorization") orelse
        ctx.request.headers.get("authorization") orelse {
        return sendUnauthorized(ctx);
    };

    // Validate using the helper function
    if (!http.basicAuth.validateCredentials(auth_header, "admin", "secret")) {
        return sendUnauthorized(ctx);
    }

    // Credentials valid - continue to handler
    return next.call(opaque_ctx);
}

fn sendUnauthorized(ctx: *http.Context) void {
    _ = ctx.response.setStatus(.unauthorized);
    ctx.response.headers.set("WWW-Authenticate", "Basic realm=\"Admin\"") catch {};
    ctx.response.headers.set("Content-Type", "application/json") catch {};
    ctx.response.body.appendSlice(ctx.allocator, "{\"error\":\"Unauthorized\"}") catch {};
}
