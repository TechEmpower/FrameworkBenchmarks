const std = @import("std");
const zinc = @import("zinc");
const Datetime = @import("datetime").datetime.Datetime;

pub fn main() !void {
    var z = try zinc.init(.{
        .port = 3000,
        .allocator = std.heap.c_allocator,
        .num_threads = 16 * @as(u8, @intCast(std.Thread.getCpuCount() catch 1)),
    });
    defer z.deinit();

    var router = z.getRouter();
    try router.use(&.{setupHeader});
    try router.get("/json", json);
    try router.get("/plaintext", plaintext);

    try z.run();
}

fn plaintext(ctx: *zinc.Context) anyerror!void {
    try ctx.setHeader("Content-Type", "text/plain; charset=utf-8");
    try ctx.setBody("Hello, world!", .{});
}

fn json(ctx: *zinc.Context) anyerror!void {
    try ctx.json(.{ .message = "Hello, World!" }, .{});
}

fn setupHeader(ctx: *zinc.Context) anyerror!void {
    try ctx.setHeader("Server", "Zinc");

    const now = Datetime.now();
    const now_str = try now.formatHttp(ctx.allocator);
    // defer ctx.allocator.free(now_str);

    // The time is now: Fri, 20 Dec 2019 22:03:02 UTC
    try ctx.setHeader("date", now_str);
}
