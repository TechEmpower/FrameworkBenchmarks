// WebSocket Echo Benchmark Server
//
// Minimal echo server for benchmarking raw WebSocket throughput.
// No logging, no allocations in hot path.
//
// Usage: ws-echo-bench [threads]
//   threads: number of io_uring worker threads (default: 4)

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // Parse thread count from args
    var args = std.process.args();
    _ = args.next(); // skip program name
    const threads: u16 = if (args.next()) |arg|
        std.fmt.parseInt(u16, arg, 10) catch 4
    else
        4;

    var router = http.router(allocator);
    defer router.deinit();

    // WebSocket echo endpoint
    _ = router.ws("/ws", .{
        .onMessage = onMessage,
    });

    std.debug.print("WebSocket Echo Benchmark Server\n", .{});
    std.debug.print("  Endpoint: ws://127.0.0.1:8080/ws\n", .{});
    std.debug.print("  Threads:  {}\n", .{threads});

    var server = http.Server.init(allocator, &router, .{
        .threads = threads,
    });
    try server.listen("127.0.0.1", 8080);
    server.wait();
}

fn onMessage(conn: *http.websocket.Connection, data: []const u8, opcode: http.websocket.Opcode) void {
    // Pure echo - no logging, no allocations
    if (opcode == .text) {
        conn.sendText(data) catch {};
    } else if (opcode == .binary) {
        conn.sendBinary(data) catch {};
    }
}
