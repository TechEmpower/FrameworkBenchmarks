const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");

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

fn escapeHtml(input: []const u8) ![]const u8 {
    var output = std.ArrayList(u8).init(std.heap.page_allocator);
    defer output.deinit();

    for (input) |c| {
        switch (c) {
            '&' => try output.appendSlice("&amp;"),
            '<' => try output.appendSlice("&lt;"),
            '>' => try output.appendSlice("&gt;"),
            '"' => try output.appendSlice("&quot;"),
            '\'' => try output.appendSlice("&apos;"),
            else => try output.append(c),
        }
    }

    return output.toOwnedSlice();
}

pub const FortunesEndpoint = struct {
    ep: zap.Endpoint = undefined,
    const Self = @This();

    pub fn init() Self {
        return .{
            .ep = zap.Endpoint.init(.{
                .path = "/fortunes",
                .get = get,
            }),
        };
    }

    pub fn endpoint(self: *Self) *zap.Endpoint {
        return &self.ep;
    }

    fn compareStrings(_: void, lhs: []const u8, rhs: []const u8) bool {
        return std.mem.order(u8, lhs, rhs).compare(std.math.CompareOperator.lt);
    }

    fn cmpFortuneByMessage(_: void, a: Fortune, b: Fortune) bool {
        return std.mem.order(u8, a.message, b.message).compare(std.math.CompareOperator.lt);
    }

    fn getFortunes(pool: *pg.Pool) ![]const Fortune {
        var conn = try pool.acquire();
        defer conn.release();

        var rows = try conn.query("SELECT id, message FROM Fortune", .{});
        rows.deinit();

        var fortunes = std.ArrayList(Fortune).init(middleware.SharedAllocator.getAllocator());
        defer fortunes.deinit();

        while (try rows.next()) |row| {
            var fortune = Fortune{ .id = row.get(i32, 0), .message = row.get([]const u8, 1) };
            _ = try fortunes.append(fortune);
        }

        var fortune = Fortune{ .id = 0, .message = "Additional fortune added at request time." };
        _ = try fortunes.append(fortune);

        var fortunes_slice = try fortunes.toOwnedSlice();
        std.mem.sort(Fortune, fortunes_slice, {}, cmpFortuneByMessage);

        return fortunes_slice;
    }

    fn getFortunesHtml(pool: *pg.Pool) ![]const u8 {
        const template_start = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";

        const template_end = "</table></body></html>";

        const allocator = middleware.SharedAllocator.getAllocator();

        var fortunes = try getFortunes(pool);

        var template = std.ArrayList(u8).init(allocator);

        try template.appendSlice(template_start);
        for (fortunes) |fortune| {
            const message = try escapeHtml(fortune.message);
            const fortune_html = try std.fmt.allocPrint(allocator, "<tr><td>{d}</td><td>{s}</td></tr>", .{ fortune.id, message });
            try template.appendSlice(fortune_html);
        }
        try template.appendSlice(template_end);

        return template.toOwnedSlice();
    }

    pub fn get(ep: *zap.Endpoint, req: zap.Request) void {
        const self = @fieldParentPtr(Self, "ep", ep);
        _ = self;

        if (!checkPath(ep, req)) return;

        req.setHeader("content-type", "text/html; charset=utf-8") catch return;

        var pool: *pg.Pool = undefined;

        const maybe_context: ?*middleware.Context = req.getUserContext(middleware.Context);
        if (maybe_context) |context| {
            if (context.pg) |cpg| {
                pool = cpg.pool;
            }
        }

        var fortunes_html = getFortunesHtml(pool) catch return;

        req.sendBody(fortunes_html) catch return;

        return;
    }
};

pub const DbEndpoint = struct {
    ep: zap.Endpoint = undefined,
    mutex: Mutex,
    const Self = @This();

    pub fn init() Self {
        return .{
            .ep = zap.Endpoint.init(.{
                .path = "/db",
                .get = get,
            }),
            .mutex = Mutex{},
        };
    }

    pub fn endpoint(self: *Self) *zap.Endpoint {
        return &self.ep;
    }

    pub fn get(ep: *zap.Endpoint, req: zap.Request) void {
        const self = @fieldParentPtr(Self, "ep", ep);

        if (!checkPath(ep, req)) return;

        req.setContentType(.JSON) catch return;

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

        // std.debug.print("Attempting to return random: {}\n", .{random_number});

        if (random_number == 0) {
            return;
        }

        var conn = pool.acquire() catch return;
        defer conn.release();

        var row_result = conn.row("SELECT id, randomNumber FROM World WHERE id = $1", .{random_number}) catch |err| {
            std.debug.print("Error querying database: {}\n", .{err});
            return;
        };
        var row = row_result.?;
        defer row.deinit();

        var world = World{ .id = row.get(i32, 0), .randomNumber = row.get(i32, 1) };

        var json_to_send = std.ArrayList(u8).init(middleware.SharedAllocator.getAllocator());
        std.json.stringify(world, .{}, json_to_send.writer()) catch |err| {
            std.debug.print("Error generating json: {}\n", .{err});
            return;
        };

        var json_slice = json_to_send.toOwnedSlice() catch |err| {
            std.debug.print("Error generating json: {}\n", .{err});
            return;
        };

        req.sendBody(json_slice) catch return;

        return;
    }
};

pub const PlaintextEndpoint = struct {
    ep: zap.Endpoint = undefined,
    const Self = @This();

    pub fn init() Self {
        return .{
            .ep = zap.Endpoint.init(.{
                .path = "/plaintext",
                .get = get,
            }),
        };
    }

    pub fn endpoint(self: *Self) *zap.Endpoint {
        return &self.ep;
    }

    pub fn get(ep: *zap.Endpoint, req: zap.Request) void {
        const self = @fieldParentPtr(Self, "ep", ep);
        _ = self;

        if (!checkPath(ep, req)) return;

        req.setContentType(.TEXT) catch return;

        req.sendBody("Hello, World!") catch return;
        return;
    }
};

pub const JsonEndpoint = struct {
    ep: zap.Endpoint = undefined,
    const Self = @This();

    pub fn init() Self {
        return .{
            .ep = zap.Endpoint.init(.{
                .path = "/json",
                .get = get,
            }),
        };
    }

    pub fn endpoint(self: *Self) *zap.Endpoint {
        return &self.ep;
    }

    pub fn get(ep: *zap.Endpoint, req: zap.Request) void {
        const self = @fieldParentPtr(Self, "ep", ep);
        _ = self;

        if (!checkPath(ep, req)) return;

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
        return;
    }
};

fn checkPath(ep: *zap.Endpoint, req: zap.Request) bool {
    if (!std.mem.eql(u8, ep.settings.path, req.path.?)) {
        // std.debug.print("Path mismatch: {s} != {s}\n", .{ ep.settings.path, req.path.? });

        return false;
    }

    // std.debug.print("Path match: {s} == {s}\n", .{ ep.settings.path, req.path.? });

    return true;
}
