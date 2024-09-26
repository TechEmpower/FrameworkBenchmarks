const std = @import("std");
const httpz = @import("httpz");
const pg = @import("pg");
const datetimez = @import("datetimez");

const Allocator = std.mem.Allocator;
const Thread = std.Thread;
const Mutex = Thread.Mutex;

pub const Global = struct {
    pool: *pg.Pool,
    prng: *std.rand.DefaultPrng,
    allocator: Allocator,
    mutex: std.Thread.Mutex = .{},
};

const Message = struct {
    message: []const u8,
};

const World = struct {
    id: i32,
    randomNumber: i32,
};

const Fortune = struct {
    id: i32,
    message: []const u8,
};

pub fn plaintext(global: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(global.allocator, res);

    res.content_type = .TEXT;
    res.body = "Hello, World!";
}

pub fn json(global: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(global.allocator, res);

    const message = Message{ .message = "Hello, World!" };

    try res.json(message, .{});
}

pub fn db(global: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(global.allocator, res);

    global.mutex.lock();
    const random_number = 1 + (global.prng.random().uintAtMost(u32, 9999));
    global.mutex.unlock();

    const world = getWorld(global.pool, random_number) catch |err| {
        std.debug.print("Error querying database: {}\n", .{err});
        return;
    };

    try res.json(world, .{});
}

fn getWorld(pool: *pg.Pool, random_number: u32) !World{
    var conn = try pool.acquire();
    defer conn.release();

    const row_result = try conn.row("SELECT id, randomNumber FROM World WHERE id = $1", .{random_number});

    var row = row_result.?;
    defer row.deinit() catch {};

    return World{ .id = row.get(i32, 0), .randomNumber = row.get(i32, 1) };
}

fn setHeaders(allocator: Allocator, res: *httpz.Response) !void {
    res.header("Server", "Httpz");

    const now = datetimez.datetime.Date.now();
    const time = datetimez.datetime.Time.now();

    // Wed, 17 Apr 2013 12:00:00 GMT
    // Return date in ISO format YYYY-MM-DD
    const TB_DATE_FMT = "{s:0>3}, {d:0>2} {s:0>3} {d:0>4} {d:0>2}:{d:0>2}:{d:0>2} GMT";
    const now_str = try std.fmt.allocPrint(allocator, TB_DATE_FMT, .{ now.weekdayName()[0..3], now.day, now.monthName()[0..3], now.year, time.hour, time.minute, time.second });

    //defer allocator.free(now_str);

    res.header("Date", now_str);
}

// fn getFortunes(pool: *pg.Pool) ![]const Fortune {
//     var conn = try pool.acquire();
//     defer conn.release();
//
//     var rows = try conn.query("SELECT id, message FROM Fortune", .{});
//     defer rows.deinit();
//
//     var fortunes = std.ArrayList(Fortune).init(middleware.SharedAllocator.getAllocator());
//     defer fortunes.deinit();
//
//     while (try rows.next()) |row| {
//         const fortune = Fortune{ .id = row.get(i32, 0), .message = row.get([]const u8, 1) };
//         try fortunes.append(fortune);
//     }
//
//     const fortune = Fortune{ .id = 0, .message = "Additional fortune added at request time." };
//     try fortunes.append(fortune);
//
//     const fortunes_slice = try fortunes.toOwnedSlice();
//     std.mem.sort(Fortune, fortunes_slice, {}, cmpFortuneByMessage);
//
//     return fortunes_slice;
// }

