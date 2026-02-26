// High-performance benchmark server for zig-http
//
// Optimized for TechEmpower-style benchmarks
//
// Run with: zig build benchmark
// Test with: wrk -t12 -c400 -d30s http://localhost:8080/plaintext

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // Benchmark routes
    _ = router
        .get("/plaintext", handlePlaintext)
        .get("/json", handleJson)
        .get("/json-fast", handleJsonFast);

    // Start server with optimized settings
    var server = http.Server.init(allocator, &router, .{
        .threads = null, // Use CPU count
        .ring_size = 4096,
        .cqe_batch_size = 512,
        .cpu_affinity = true,
        .tcp_nodelay = true,
        .max_connections = 8192,
        .read_buffer_size = 4096,
    });
    defer server.deinit();

    std.log.info("Starting benchmark server on http://localhost:8080", .{});
    std.log.info("Endpoints:", .{});
    std.log.info("  /plaintext  - Plain text response", .{});
    std.log.info("  /json       - JSON response (std.json)", .{});
    std.log.info("  /json-fast  - JSON response (SIMD/SWAR)", .{});
    try server.run("0.0.0.0", 8080);
}

fn handlePlaintext(ctx: *http.Context) !void {
    try ctx.sendText("Hello, World!");
}

fn handleJson(ctx: *http.Context) !void {
    try ctx.sendJson(.{ .message = "Hello, World!" });
}

fn handleJsonFast(ctx: *http.Context) !void {
    try ctx.sendJsonFast(.{ .message = "Hello, World!" });
}
