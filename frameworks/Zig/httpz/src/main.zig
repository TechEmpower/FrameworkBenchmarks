const std = @import("std");
const builtin = @import("builtin");
const httpz = @import("httpz");
const pg = @import("pg");
const datetimez = @import("datetimez");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");

const RndGen = std.rand.DefaultPrng;
const Allocator = std.mem.Allocator;
const Pool = pg.Pool;

var server: httpz.ServerCtx(*endpoints.Global, *endpoints.Global) = undefined;

pub fn main() !void {
    const cpu_count = try std.Thread.getCpuCount();
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};

    const allocator = gpa.allocator();

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    var prng = std.rand.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp())));

    var global = endpoints.Global{
        .pool = pg_pool,
        .prng = &prng,
    };

    server = try httpz.ServerApp(*endpoints.Global).init(allocator, .{ .port = 3000, .address = "0.0.0.0", .workers = .{
        .count = @truncate(cpu_count),
        .max_conn = 4096,
    }, .thread_pool = .{ .count = @truncate(cpu_count * 2) } }, &global);
    defer server.deinit();

    // now that our server is up, we register our intent to handle SIGINT
    try std.posix.sigaction(std.posix.SIG.INT, &.{
        .handler = .{ .handler = shutdown },
        .mask = std.posix.empty_sigset,
        .flags = 0,
    }, null);

    var router = server.router();
    router.get("/json", endpoints.json);
    router.get("/plaintext", endpoints.plaintext);
    router.get("/db", endpoints.db);
    router.get("/fortunes", endpoints.fortune);

    std.debug.print("Httpz listening at 0.0.0.0:{d}\n", .{3000});

    try server.listen();
}

fn shutdown(_: c_int) callconv(.C) void {
    server.stop();
}

fn notFound(_: *httpz.Request, res: *httpz.Response) !void {
    res.status = 404;
    res.body = "Not Found";
}

fn errorHandler(req: *httpz.Request, res: *httpz.Response, err: anyerror) void {
    res.status = 500;
    res.body = "Internal Server Error";
    std.log.warn("httpz: unhandled exception for request: {s}\nErr: {}", .{ req.url.raw, err });
}
