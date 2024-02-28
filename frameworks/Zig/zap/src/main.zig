const std = @import("std");
const zap = @import("zap");
const Allocator = std.mem.Allocator;

const Message = struct {
    message: []const u8,
};

pub const Benchmark = struct {
    const Self = @This();

    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return .{ .allocator = allocator };
    }

    pub fn plaintext(self: *Self, req: zap.Request) void {
        _ = self;

        req.setHeader("Server", "Zap") catch return;
        req.setContentType(.TEXT) catch return;

        req.sendBody("Hello, World!") catch return;
    }

    pub fn json(self: *Self, req: zap.Request) void {
        _ = self;

        req.setHeader("Server", "Zap") catch return;
        req.setContentType(.JSON) catch return;

        var message = Message{ .message = "Hello, World!" };

        var buf: [100]u8 = undefined;
        var json_to_send: []const u8 = undefined;
        if (zap.stringifyBuf(&buf, message, .{})) |json_message| {
            json_to_send = json_message;
        } else {
            json_to_send = "null";
        }

        req.sendBody(json_to_send) catch return;
    }
};

fn not_found(req: zap.Request) void {
    std.debug.print("not found handler", .{});

    req.setStatus(.not_found);
    req.sendBody("<html><body><h1>404 - File not found</h1></body></html>") catch return;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};
    var allocator = gpa.allocator();

    var simpleRouter = zap.Router.init(allocator, .{
        .not_found = not_found,
    });
    defer simpleRouter.deinit();

    var benchmark = Benchmark.init(allocator);

    var listener = zap.HttpListener.init(.{
        .port = 3000,
        .on_request = zap.RequestHandler(&simpleRouter, &zap.Router.serve),
        .log = false,
    });
    try listener.listen();

    try simpleRouter.handle_func("/plaintext", zap.RequestHandler(&benchmark, Benchmark.plaintext));
    try simpleRouter.handle_func("/json", zap.RequestHandler(&benchmark, Benchmark.json));

    const threads = @as(i16, @intCast(std.Thread.getCpuCount() catch 1));

    std.debug.print("Listening on 0.0.0.0:3000 on {d} threads\n", .{threads});

    // start worker threads
    zap.start(.{
        .threads = threads,
        .workers = threads,
    });
}


