// Observability Example
//
// Demonstrates full-stack observability with:
// - Prometheus metrics endpoint
// - Request metrics middleware
// - Distributed tracing with OTLP export
//
// Run with: zig build observability
// Then visit:
//   http://localhost:8080/         - Hello page
//   http://localhost:8080/api/data - API endpoint
//   http://localhost:8080/metrics  - Prometheus metrics

const std = @import("std");
const http = @import("http");
const metrics = @import("metrics");
const tracing = @import("tracing");

const Context = http.Context;
const Middleware = http.Middleware;
const Next = http.Next;

// Global registry for metrics
var registry: *metrics.Registry = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize metrics registry
    var reg = metrics.Registry.init(allocator);
    defer reg.deinit();
    registry = &reg;

    // Set up metrics middleware
    metrics.middleware.setRegistry(registry);

    // Initialize tracing exporter (optional - requires OTLP collector)
    var exporter = tracing.Exporter.init(allocator, .{
        .endpoint = "http://localhost:4318",
        .service_name = "observability-example",
    });
    defer exporter.deinit();

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // Add metrics middleware to all routes and register handlers
    _ = router
        .useGlobal(metricsMiddleware)
        .get("/", handleIndex)
        .get("/api/data", handleApiData)
        .get("/metrics", handleMetrics);

    // Start server
    std.log.info("Starting server on http://localhost:8080", .{});
    std.log.info("Metrics available at http://localhost:8080/metrics", .{});

    var server = http.Server.init(allocator, &router, .{
        .threads = 4,
    });
    defer server.deinit();

    try server.run("0.0.0.0", 8080);
}

/// Metrics middleware - captures request timing and counts
fn metricsMiddleware(opaque_ctx: *http.mw.Context, next: Next) anyerror!void {
    const ctx: *Context = @ptrCast(@alignCast(opaque_ctx));

    // Start timing
    const timer = metrics.middleware.startRequest();

    // Call the next handler
    const result = next.call(opaque_ctx);

    // Record metrics
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

    metrics.middleware.endRequest(timer, method_str, ctx.request.path, ctx.response.status.code());

    return result;
}

fn handleIndex(ctx: *Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>Observability Example</title></head>
        \\<body>
        \\<h1>Observability Example</h1>
        \\<ul>
        \\<li><a href="/api/data">API Data</a></li>
        \\<li><a href="/metrics">Prometheus Metrics</a></li>
        \\</ul>
        \\</body>
        \\</html>
    );
}

fn handleApiData(ctx: *Context) !void {
    // Return a simple response
    try ctx.sendJson(.{
        .message = "Hello from the API",
    });
}

fn handleMetrics(ctx: *Context) !void {
    // Format metrics as Prometheus text using a buffer
    var buf: [8192]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buf);

    metrics.prometheus.format(registry, &writer) catch |err| {
        std.log.err("Error formatting metrics: {}", .{err});
        try ctx.response.body.appendSlice(ctx.allocator, "Error formatting metrics");
        return;
    };

    ctx.response.headers.set("Content-Type", "text/plain; version=0.0.4; charset=utf-8") catch {};
    try ctx.response.body.appendSlice(ctx.allocator, buf[0..writer.end]);
}

// ============================================================================
// Example: Using instrumented clients (commented out as it requires pg/client)
// ============================================================================

// const pg = @import("pg");
// const client = @import("client");
//
// // Wrap PostgreSQL pool
// var pg_pool = try pg.Pool.init(allocator, .{
//     .database = "mydb",
//     .username = "user",
// });
// var db = metrics.instrumentPool(@TypeOf(pg_pool), &pg_pool, registry);
//
// // Wrap HTTP client
// var http_client = client.Client.init(allocator, .{});
// var api_client = metrics.instrumentClient(@TypeOf(http_client), &http_client, registry);
//
// // Use instrumented clients - metrics are automatically collected
// const result = try db.query("SELECT * FROM users");
// const response = try api_client.get("https://api.example.com/data");
