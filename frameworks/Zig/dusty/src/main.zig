const std = @import("std");
const builtin = @import("builtin");
const dusty = @import("dusty");
const zio = @import("zio");
const pg = @import("pg");
const datetimez = @import("datetimez");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};
    defer {
        if (builtin.mode == .Debug) _ = gpa.deinit();
    }

    const allocator = if (builtin.mode == .Debug) gpa.allocator() else std.heap.smp_allocator;

    var io = try zio.Runtime.init(allocator, .{});
    defer io.deinit();

    var task = try zio.spawn(runServer, .{ allocator, io });
    try task.join();
}

fn runServer(allocator: std.mem.Allocator, io: *zio.Runtime) !void {
    _ = io;

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    const date_thread = try std.Thread.spawn(.{}, struct {
        fn update() !void {
            while (true) {
                const now = datetimez.datetime.Date.now();
                const time = datetimez.datetime.Time.now();

                const TB_DATE_FMT = "{s:0>3}, {d:0>2} {s:0>3} {d:0>4} {d:0>2}:{d:0>2}:{d:0>2} GMT";
                _ = try std.fmt.bufPrint(&endpoints.date_str, TB_DATE_FMT, .{ now.weekdayName()[0..3], now.day, now.monthName()[0..3], now.year, time.hour, time.minute, time.second });
                std.Thread.sleep(std.time.ns_per_ms * 980);
            }
        }
    }.update, .{});

    date_thread.detach();

    var prng: std.Random.DefaultPrng = .init(@as(u64, @bitCast(std.time.milliTimestamp())));

    var rand = prng.random();

    var global = endpoints.Global{
        .pool = pg_pool,
        .rand = &rand,
    };

    const port: u16 = 3000;

    const DustyServer = dusty.Server(endpoints.Global);

    const config: dusty.ServerConfig = .{
        .timeout = .{
            .request = 60 * std.time.ms_per_s,
            .keepalive = 300 * std.time.ms_per_s,
        }
    };

    var server = DustyServer.init(allocator, config, &global);
    defer server.deinit();

    server.router.get("/json", endpoints.json);
    server.router.get("/plaintext", endpoints.plaintext);
    server.router.get("/db", endpoints.db);
    server.router.get("/fortunes", endpoints.fortune);

    std.debug.print("Dusty listening at 0.0.0.0:{d}\n", .{port});

    const addr = try zio.net.IpAddress.parseIp("0.0.0.0", port);

    var listen_task = try zio.spawn(DustyServer.listen, .{ &server, addr });
    defer listen_task.cancel();

    var sigint = try zio.Signal.init(.interrupt);
    defer sigint.deinit();

    var sigterm = try zio.Signal.init(.terminate);
    defer sigterm.deinit();

    const result = try zio.select(.{ .task = &listen_task, .sigint = &sigint, .sigterm = &sigterm });
    switch (result) {
        .task => |r| return r,
        .sigint, .sigterm => {
            listen_task.cancel();
            return;
        },
    }
}
