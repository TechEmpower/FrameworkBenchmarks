const std = @import("std");
const zinc = @import("zinc");
const Datetime = @import("datetime").datetime.Datetime;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var z = try zinc.init(.{ .port = 8080, .allocator = allocator, .num_threads = 255 });

    var router = z.getRouter();
    try router.use(&.{setupHeader});

    try router.get("/json", json);
    try router.get("/plaintext", plaintext);

    try z.run();
}

fn plaintext(ctx: *zinc.Context) anyerror!void {
    try ctx.text("Hello, World!", .{});
}

fn json(ctx: *zinc.Context) anyerror!void {
    try ctx.json(.{ .message = "Hello, World!" }, .{});
}

fn setupHeader(ctx: *zinc.Context) anyerror!void {
    try ctx.setHeader("Server", "Zinc");
    // try ctx.setHeader("date", "Sun Sep 22 10:01:11 CEST 2024");
    // In UTC
    const now = Datetime.now();
    const now_str = try now.formatHttp(ctx.allocator);
    // defer ctx.allocator.free(now_str);
    // std.debug.warn("The time is now: {}\n", .{now_str});
    // The time is now: Fri, 20 Dec 2019 22:03:02 UTC
    try ctx.setHeader("date", now_str);
}
