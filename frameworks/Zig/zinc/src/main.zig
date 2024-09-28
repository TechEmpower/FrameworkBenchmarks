const std = @import("std");
const zinc = @import("zinc");
const Datetime = @import("datetime").datetime.Datetime;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};
    var tsa = std.heap.ThreadSafeAllocator{
        .child_allocator = gpa.allocator(),
    };
    const allocator = tsa.allocator();

    const cpuCount = @as(u8, @intCast(std.Thread.getCpuCount() catch 1));

    var z = try zinc.init(.{
        .port = 8080,
        .allocator = allocator,
        .num_threads = 16 * cpuCount,
    });

    var router = z.getRouter();
    try router.use(&.{setupHeader});

    try router.get("/json", json);
    try router.get("/plaintext", plaintext);

    z.run() catch |err| std.debug.print("Error: {any}\n", .{err});
}

fn plaintext(ctx: *zinc.Context) anyerror!void {
    try ctx.text("Hello, World!", .{});
}

fn json(ctx: *zinc.Context) anyerror!void {
    try ctx.json(.{ .message = "Hello, World!" }, .{});
}

fn setupHeader(ctx: *zinc.Context) anyerror!void {
    try ctx.setHeader("Server", "Zinc");
    try ctx.setHeader("Connection", "keep-alive");

    const now = Datetime.now();
    const now_str = try now.formatHttp(ctx.allocator);
    // The time is now: Fri, 20 Dec 2019 22:03:02 UTC
    try ctx.setHeader("date", now_str);
}
