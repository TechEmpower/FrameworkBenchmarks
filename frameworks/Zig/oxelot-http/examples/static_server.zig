// Static File Server Example
//
// This example demonstrates serving static files with zig-http.
//
// Features demonstrated:
// - Serving files from a directory
// - MIME type detection
// - ETag and Last-Modified caching
// - Range requests for resumable downloads
//
// Build and run:
//   zig build-exe examples/static_server.zig -Mroot=src/http.zig && ./static_server

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // Serve static files from ./public directory
    // All requests to /static/* will be served from ./public
    const static_config = http.static.Config{
        .root = "/tmp/public", // Change this to your static files directory
        .prefix = "/static",
        .index = "index.html",
        .max_age = 3600, // Cache for 1 hour
        .etag = true,
        .last_modified = true,
    };

    // Set the static config for the handler
    http.static.setConfig(&static_config);

    // Route for static files (catches all paths under /static/)
    _ = router.get("/static/*", http.static.serve(static_config.root, .{
        .prefix = static_config.prefix,
        .index = static_config.index,
        .max_age = static_config.max_age,
    }));

    // API route example
    _ = router.get("/api/health", health);

    // Home page
    _ = router.get("/", home);

    std.debug.print("Starting server on http://localhost:8080\n", .{});
    std.debug.print("Static files served from: {s}\n", .{static_config.root});
    std.debug.print("\nExample URLs:\n", .{});
    std.debug.print("  http://localhost:8080/\n", .{});
    std.debug.print("  http://localhost:8080/static/style.css\n", .{});
    std.debug.print("  http://localhost:8080/static/script.js\n", .{});
    std.debug.print("  http://localhost:8080/api/health\n", .{});

    // Start server
    var server = http.Server.init(allocator, &router, .{});
    defer server.deinit();
    try server.run("0.0.0.0", 8080);
}

fn health(ctx: *http.Context) !void {
    try ctx.sendJson(.{
        .status = "ok",
        .message = "Server is running",
    });
}

fn home(ctx: *http.Context) !void {
    const html =
        \\<!DOCTYPE html>
        \\<html>
        \\<head>
        \\    <title>Static File Server</title>
        \\    <link rel="stylesheet" href="/static/style.css">
        \\</head>
        \\<body>
        \\    <h1>Static File Server Example</h1>
        \\    <p>This server demonstrates static file serving with zig-http.</p>
        \\    <h2>Features:</h2>
        \\    <ul>
        \\        <li>MIME type detection</li>
        \\        <li>ETag caching</li>
        \\        <li>Last-Modified headers</li>
        \\        <li>Range requests (partial content)</li>
        \\    </ul>
        \\    <p><a href="/api/health">API Health Check</a></p>
        \\    <script src="/static/script.js"></script>
        \\</body>
        \\</html>
    ;
    try ctx.sendHtml(html);
}
