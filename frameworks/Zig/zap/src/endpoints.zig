const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");

const Mustache = @import("zap").Mustache;
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
    ep: zap.Endpoint = undefined,
    mustache: Mustache,
    mutex: Mutex,

    const Self = @This();

    pub fn init() Self {
        const template = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>{{#fortunes}}<tr><td>{{id}}</td><td>{{message}}</td></tr>{{/fortunes}}</table></body></html>";
        const mustache = Mustache.fromData(template) catch unreachable;

        return .{
            .ep = zap.Endpoint.init(.{
                .path = "/fortunes",
                .get = get,
            }),
            .mustache = mustache,
            .mutex = Mutex{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.mustache.deinit();
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
        defer rows.deinit();

        var fortunes = std.ArrayList(Fortune).init(middleware.SharedAllocator.getAllocator());
        defer fortunes.deinit();

        while (try rows.next()) |row| {
            const fortune = Fortune{ .id = row.get(i32, 0), .message = row.get([]const u8, 1) };
            try fortunes.append(fortune);
        }

        const fortune = Fortune{ .id = 0, .message = "Additional fortune added at request time." };
        try fortunes.append(fortune);

        const fortunes_slice = try fortunes.toOwnedSlice();
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

        // std.debug.print("mustache output {s}\n", .{raw});

        const html = try deescapeHtml(raw);

        // std.debug.print("html output {s}\n", .{html});

        return html;
    }

    pub fn get(ep: *zap.Endpoint, req: zap.Request) void {
        const self: *FortunesEndpoint = @fieldParentPtr("ep", ep);

        if (!checkPath(ep, req)) return;

        req.setHeader("content-type", "text/html; charset=utf-8") catch return;

        var pool: *pg.Pool = undefined;

        const maybe_context: ?*middleware.Context = req.getUserContext(middleware.Context);
        if (maybe_context) |context| {
            if (context.pg) |cpg| {
                pool = cpg.pool;
            }
        }

        const fortunes_html = getFortunesHtml(self, pool) catch return;

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
        const self: *DbEndpoint = @fieldParentPtr("ep", ep);

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

        const row_result = conn.row("SELECT id, randomNumber FROM World WHERE id = $1", .{random_number}) catch |err| {
            std.debug.print("Error querying database: {}\n", .{err});
            return;
        };
        var row = row_result.?;

        const world = World{ .id = row.get(i32, 0), .randomNumber = row.get(i32, 1) };

        var buf: [100]u8 = undefined;
        var json_to_send: []const u8 = undefined;
        if (zap.stringifyBuf(&buf, world, .{})) |json_message| {
            json_to_send = json_message;
        } else {
            json_to_send = "null";
        }

        req.sendBody(json_to_send) catch return;

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
        const self: *PlaintextEndpoint = @fieldParentPtr("ep", ep);
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
        const self: *JsonEndpoint  = @fieldParentPtr("ep", ep);
        _ = self;

        if (!checkPath(ep, req)) return;

        req.setContentType(.JSON) catch return;

        const message = Message{ .message = "Hello, World!" };

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

fn deescapeHtml(input: []const u8) ![]const u8 {
    var output = std.ArrayList(u8).init(middleware.SharedAllocator.getAllocator());
    defer output.deinit();

    var i: usize = 0;
    while (i < input.len) {
        if (std.mem.startsWith(u8, input[i..], "&#32;")) {
            try output.append(' ');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#34;")) {
            try output.append('"');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#38;")) {
            try output.append('&');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#39;")) {
            try output.append('\'');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#40;")) {
            try output.append('(');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#41;")) {
            try output.append(')');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#43;")) {
            try output.append('+');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#44;")) {
            try output.append(',');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#46;")) {
            try output.append('.');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#47;")) {
            try output.append('/');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#58;")) {
            try output.append(':');
            i += 5;
        } else if (std.mem.startsWith(u8, input[i..], "&#59;")) {
            try output.append(';');
            i += 5;
        } else {
            try output.append(input[i]);
            i += 1;
        }
    }

    return output.toOwnedSlice();
}
