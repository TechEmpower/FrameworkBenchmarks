// Simple HTTP server example using zig-http
//
// Run with: zig build run
// Test with: curl http://localhost:8080/

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // Register routes
    _ = router
        .get("/", handleHome)
        .get("/hello", handleHello)
        .get("/hello/:name", handleHelloName)
        .get("/health", handleHealth)
        .post("/echo", handleEcho);

    // Start server
    var server = http.Server.init(allocator, &router, .{
        .threads = 4,
    });
    defer server.deinit();

    std.log.info("Starting simple server on http://localhost:8080", .{});
    try server.run("0.0.0.0", 8080);
}

fn handleHome(ctx: *http.Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>zig-http</title></head>
        \\<body>
        \\  <h1>Welcome to zig-http!</h1>
        \\  <ul>
        \\    <li><a href="/hello">Hello</a></li>
        \\    <li><a href="/hello/World">Hello World</a></li>
        \\    <li><a href="/health">Health Check</a></li>
        \\  </ul>
        \\</body>
        \\</html>
    );
}

fn handleHello(ctx: *http.Context) !void {
    try ctx.sendText("Hello, World!");
}

fn handleHelloName(ctx: *http.Context) !void {
    const name = ctx.param("name") orelse "stranger";
    try ctx.sendJson(.{
        .message = "Hello!",
        .name = name,
    });
}

fn handleHealth(ctx: *http.Context) !void {
    try ctx.sendJson(.{
        .status = "ok",
        .uptime = "running",
    });
}

fn handleEcho(ctx: *http.Context) !void {
    const body = ctx.request.body orelse "";
    try ctx.response.send(body);
}
