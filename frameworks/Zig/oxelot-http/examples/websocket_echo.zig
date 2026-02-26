// WebSocket Echo Server Example
//
// Demonstrates WebSocket support in zig-http.
// Connect with a WebSocket client and send messages to receive echoes.
//
// Test with:
//   websocat ws://localhost:8080/ws
//   (install with: cargo install websocat)
//
// Or use a browser console:
//   let ws = new WebSocket("ws://localhost:8080/ws");
//   ws.onmessage = (e) => console.log("Received:", e.data);
//   ws.onopen = () => ws.send("Hello!");

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var router = http.router(allocator);
    defer router.deinit();

    // HTTP endpoint for health check
    _ = router.get("/", handleIndex);

    // WebSocket endpoint
    _ = router.ws("/ws", .{
        .onOpen = onOpen,
        .onMessage = onMessage,
        .onClose = onClose,
        .onError = onError,
    });

    std.debug.print("WebSocket Echo Server running on http://localhost:8080\n", .{});
    std.debug.print("Connect to ws://localhost:8080/ws\n", .{});

    var server = http.Server.init(allocator, &router, .{
        .threads = 1,
        .read_buffer_size = 65536, // 64KB for larger WebSocket messages
    });
    defer server.deinit();
    try server.run("0.0.0.0", 8080);
}

fn handleIndex(ctx: *http.Context) !void {
    try ctx.sendHtml(
        \\<!DOCTYPE html>
        \\<html>
        \\<head><title>WebSocket Echo</title></head>
        \\<body>
        \\<h1>WebSocket Echo Server</h1>
        \\<p>Connect to <code>ws://localhost:8080/ws</code></p>
        \\<script>
        \\  const ws = new WebSocket("ws://localhost:8080/ws");
        \\  ws.onopen = () => { console.log("Connected"); ws.send("Hello!"); };
        \\  ws.onmessage = (e) => console.log("Received:", e.data);
        \\  ws.onclose = () => console.log("Disconnected");
        \\</script>
        \\</body>
        \\</html>
    );
}

fn onOpen(conn: *http.WebSocketConnection) void {
    std.debug.print("WebSocket connection opened\n", .{});
    conn.sendText("Welcome to the echo server!") catch {};
}

fn onMessage(conn: *http.WebSocketConnection, data: []const u8, opcode: http.WebSocketOpcode) void {
    std.debug.print("Received ({s}): {s}\n", .{ @tagName(opcode), data });

    // Echo the message back
    if (opcode == .text) {
        conn.sendText(data) catch |err| {
            std.debug.print("Error sending: {}\n", .{err});
        };
    } else if (opcode == .binary) {
        conn.sendBinary(data) catch |err| {
            std.debug.print("Error sending: {}\n", .{err});
        };
    }
}

fn onClose(conn: *http.WebSocketConnection, code: ?http.WebSocketCloseCode, reason: ?[]const u8) void {
    _ = conn;
    std.debug.print("WebSocket connection closed: code={?}, reason={?s}\n", .{ code, reason });
}

fn onError(conn: *http.WebSocketConnection, err: anyerror) void {
    _ = conn;
    std.debug.print("WebSocket error: {}\n", .{err});
}
