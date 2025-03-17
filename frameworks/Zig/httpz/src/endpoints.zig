const std = @import("std");
const httpz = @import("httpz");
const pg = @import("pg");
const datetimez = @import("datetimez");

pub var date_str: []u8 = "";

pub const Global = struct {
    pool: *pg.Pool,
    rand: *std.rand.Random,
};

const World = struct {
    id: i32,
    randomNumber: i32,
};

const Fortune = struct {
    id: i32,
    message: []const u8,
};

pub fn plaintext(_: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(res.arena, res);

    res.content_type = .TEXT;
    res.body = "Hello, World!";
}

pub fn json(_: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(res.arena, res);

    try res.json(.{ .message = "Hello, World!" }, .{});
}

pub fn db(global: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(res.arena, res);

    const random_number = 1 + (global.rand.uintAtMostBiased(u32, 9999));

    const world = getWorld(global.pool, random_number) catch |err| {
        std.debug.print("Error querying database: {}\n", .{err});
        return;
    };

    try res.json(world, .{});
}

pub fn fortune(global: *Global, _: *httpz.Request, res: *httpz.Response) !void {
    try setHeaders(res.arena, res);

    const fortunes_html = try getFortunesHtml(res.arena, global.pool);

    res.header("content-type", "text/html; charset=utf-8");
    res.body = fortunes_html;
}

fn getWorld(pool: *pg.Pool, random_number: u32) !World {
    var conn = try pool.acquire();
    defer conn.release();

    const row_result = try conn.row("SELECT id, randomNumber FROM World WHERE id = $1", .{random_number});

    var row = row_result.?;
    defer row.deinit() catch {};

    return World{ .id = row.get(i32, 0), .randomNumber = row.get(i32, 1) };
}

fn setHeaders(allocator: std.mem.Allocator, res: *httpz.Response) !void {
    res.header("Server", "Httpz");

    //const now = datetimez.datetime.Date.now();
    //const time = datetimez.datetime.Time.now();

    // Wed, 17 Apr 2013 12:00:00 GMT
    // Return date in ISO format YYYY-MM-DD
    //const TB_DATE_FMT = "{s:0>3}, {d:0>2} {s:0>3} {d:0>4} {d:0>2}:{d:0>2}:{d:0>2} GMT";
    //const now_str = try std.fmt.allocPrint(allocator, TB_DATE_FMT, .{ now.weekdayName()[0..3], now.day, now.monthName()[0..3], now.year, time.hour, time.minute, time.second });

    //defer allocator.free(now_str);

    res.header("Date", try allocator.dupe(u8, date_str));
}

fn getFortunesHtml(allocator: std.mem.Allocator, pool: *pg.Pool) ![]const u8 {
    const fortunes = try getFortunes(allocator, pool);

    var sb = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 0);

    const writer = sb.writer(allocator);
    try sb.appendSlice(allocator, "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

    for (fortunes) |ft| {
        try writer.print("<tr><td>{d}</td><td>{s}</td></tr>", .{
            ft.id,
            try escape_html(allocator, ft.message),
        });
    }

    try sb.appendSlice(allocator, "</table></body></html>");

    return sb.toOwnedSlice(allocator);
}

fn getFortunes(allocator: std.mem.Allocator, pool: *pg.Pool) ![]const Fortune {
    var conn = try pool.acquire();
    defer conn.release();

    var rows = try conn.query("SELECT id, message FROM Fortune", .{});
    defer rows.deinit();

    var fortunes = try std.ArrayListUnmanaged(Fortune).initCapacity(allocator, 0);
    defer fortunes.deinit(allocator);

    while (try rows.next()) |row| {
        const current_fortune = Fortune{ .id = row.get(i32, 0), .message = row.get([]const u8, 1) };
        try fortunes.append(allocator, current_fortune);
    }

    const zero_fortune = Fortune{ .id = 0, .message = "Additional fortune added at request time." };
    try fortunes.append(allocator, zero_fortune);

    const fortunes_slice = try fortunes.toOwnedSlice(allocator);
    std.mem.sort(Fortune, fortunes_slice, {}, cmpFortuneByMessage);

    return fortunes_slice;
}

fn cmpFortuneByMessage(_: void, a: Fortune, b: Fortune) bool {
    return std.mem.order(u8, a.message, b.message).compare(std.math.CompareOperator.lt);
}

fn escape_html(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var output = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 0);
    defer output.deinit(allocator);

    for (input) |char| {
        switch (char) {
            '<' => try output.appendSlice(allocator, "&lt;"),
            '>' => try output.appendSlice(allocator, "&gt;"),
            else => try output.append(allocator, char),
        }
    }

    return output.toOwnedSlice(allocator);
}
