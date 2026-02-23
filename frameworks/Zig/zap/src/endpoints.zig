const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");

const Mustache = zap.Mustache;
const Thread = std.Thread;
const Mutex = Thread.Mutex;

const middleware = @import("middleware.zig");

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

pub const FortunesEndpoint = struct {
    path: []const u8 = "/fortunes",
    mustache: Mustache,
    mutex: Mutex,

    const Self = @This();

    pub fn init() Self {
        const template = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{{#fortunes}}<tr><td>{{id}}</td><td>{{message}}</td></tr>{{/fortunes}}</table></body></html>";
        const mustache = Mustache.fromData(template) catch unreachable;

        return .{
            .mustache = mustache,
            .mutex = Mutex{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.mustache.deinit();
    }

    fn cmpFortuneByMessage(_: void, a: Fortune, b: Fortune) bool {
        return std.mem.order(u8, a.message, b.message).compare(std.math.CompareOperator.lt);
    }

    fn getFortunes(pool: *pg.Pool) ![]const Fortune {
        const alloc = middleware.SharedAllocator.getAllocator();
        var conn = try pool.acquire();
        defer conn.release();

        var rows = try conn.query("SELECT id, message FROM Fortune", .{});
        defer rows.deinit();

        var fortunes: std.ArrayListUnmanaged(Fortune) = .empty;
        defer fortunes.deinit(alloc);

        while (try rows.next()) |row| {
            const fortune = Fortune{ .id = try row.get(i32, 0), .message = try row.get([]const u8, 1) };
            try fortunes.append(alloc, fortune);
        }

        const fortune = Fortune{ .id = 0, .message = "Additional fortune added at request time." };
        try fortunes.append(alloc, fortune);

        const fortunes_slice = try fortunes.toOwnedSlice(alloc);
        std.mem.sort(Fortune, fortunes_slice, {}, cmpFortuneByMessage);

        return fortunes_slice;
    }

    fn getFortunesHtml(self: *Self, pool: *pg.Pool) ![]const u8 {
        const fortunes = try getFortunes(pool);

        self.mutex.lock();
        const ret = self.mustache.build(.{ .fortunes = fortunes });
        defer ret.deinit();
        self.mutex.unlock();

        const raw = ret.str().?;
        const html = try deescapeHtml(raw);

        return html;
    }

    pub fn get(self: *Self, req: zap.Request) !void {
        try req.setHeader("content-type", "text/html; charset=utf-8");

        var pool: *pg.Pool = undefined;

        const maybe_context: ?*middleware.Context = req.getUserContext(middleware.Context);
        if (maybe_context) |context| {
            if (context.pg) |cpg| {
                pool = cpg.pool;
            }
        }

        const fortunes_html = try self.getFortunesHtml(pool);

        try req.sendBody(fortunes_html);
    }
};

pub const DbEndpoint = struct {
    path: []const u8 = "/db",
    mutex: Mutex,

    const Self = @This();

    pub fn init() Self {
        return .{
            .mutex = Mutex{},
        };
    }

    pub fn get(self: *Self, req: zap.Request) !void {
        try req.setContentType(.JSON);

        var random_number: u32 = 0;
        var pool: *pg.Pool = undefined;

        const maybe_context: ?*middleware.Context = req.getUserContext(middleware.Context);
        if (maybe_context) |context| {
            if (context.prng) |prng| {
                if (context.pg) |cpg| {
                    pool = cpg.pool;

                    self.mutex.lock();
                    random_number = 1 + (prng.rnd.random().uintAtMost(u32, 9999));
                    self.mutex.unlock();
                }
            }
        }

        if (random_number == 0) {
            return;
        }

        const json_to_send = try getJson(pool, random_number);

        try req.sendBody(json_to_send);
    }

    fn getJson(pool: *pg.Pool, random_number: u32) ![]const u8 {
        var conn = try pool.acquire();
        defer conn.release();

        const row_result = try conn.row("SELECT id, randomNumber FROM World WHERE id = $1", .{random_number});

        var row = row_result.?;
        defer row.deinit() catch {};

        const world = World{ .id = try row.get(i32, 0), .randomNumber = try row.get(i32, 1) };

        var buf: [100]u8 = undefined;
        const json_to_send = zap.util.stringifyBuf(&buf, world, .{}) catch return "null";
        return json_to_send;
    }
};

pub const PlaintextEndpoint = struct {
    path: []const u8 = "/plaintext",

    const Self = @This();

    pub fn init() Self {
        return .{};
    }

    pub fn get(_: *Self, req: zap.Request) !void {
        try req.setContentType(.TEXT);
        try req.sendBody("Hello, World!");
    }
};

pub const JsonEndpoint = struct {
    path: []const u8 = "/json",

    const Self = @This();

    pub fn init() Self {
        return .{};
    }

    pub fn get(_: *Self, req: zap.Request) !void {
        try req.setContentType(.JSON);

        const message = Message{ .message = "Hello, World!" };

        var buf: [100]u8 = undefined;
        const json_to_send = zap.util.stringifyBuf(&buf, message, .{}) catch "null";
        try req.sendBody(json_to_send);
    }
};

fn deescapeHtml(input: []const u8) ![]const u8 {
    const alloc = middleware.SharedAllocator.getAllocator();
    var output: std.ArrayListUnmanaged(u8) = .empty;
    defer output.deinit(alloc);

    var i: usize = 0;
    while (i < input.len) {
        if (std.mem.startsWith(u8, input[i..], "&#32;")) {
            try output.append(alloc, ' ');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#34;")) {
            try output.append(alloc, '"');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#38;")) {
            try output.append(alloc, '&');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#39;")) {
            try output.append(alloc, '\'');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#40;")) {
            try output.append(alloc, '(');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#41;")) {
            try output.append(alloc, ')');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#43;")) {
            try output.append(alloc, '+');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#44;")) {
            try output.append(alloc, ',');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#46;")) {
            try output.append(alloc, '.');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#47;")) {
            try output.append(alloc, '/');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#58;")) {
            try output.append(alloc, ':');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#59;")) {
            try output.append(alloc, ';');
            i += 5;
        } else {
            try output.append(alloc, input[i]);
            i += 1;
        }
    }

    return output.toOwnedSlice(alloc);
}
