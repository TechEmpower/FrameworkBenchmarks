const std = @import("std");
const Status = @import("status.zig").Status;
const Headers = @import("headers.zig").Headers;
const ContentType = @import("headers.zig").ContentType;

/// HTTP Response builder
pub const Response = struct {
    allocator: std.mem.Allocator,
    status: Status,
    headers: Headers,
    body: std.ArrayListUnmanaged(u8),
    sent: bool,
    /// Pre-formatted raw HTTP response bytes (status line + headers + body).
    /// When set, serialize() returns a copy of this instead of building the response.
    raw_response: ?[]const u8 = null,
    /// When true, the handler has submitted an async operation (e.g. DB query)
    /// and the response should not be sent yet.
    async_pending: bool = false,

    pub fn init(allocator: std.mem.Allocator) Response {
        return .{
            .allocator = allocator,
            .status = .ok,
            .headers = Headers.init(allocator),
            .body = .empty,
            .sent = false,
            .raw_response = null,
        };
    }

    pub fn deinit(self: *Response) void {
        self.headers.deinit();
        self.body.deinit(self.allocator);
    }

    /// Reset response for reuse
    pub fn reset(self: *Response) void {
        self.status = .ok;
        // Clear headers
        for (self.headers.entries.keys(), self.headers.entries.values()) |key, value| {
            self.allocator.free(key);
            self.allocator.free(value);
        }
        self.headers.entries.clearRetainingCapacity();
        self.body.clearRetainingCapacity();
        self.sent = false;
        self.raw_response = null;
    }

    /// Set a raw, pre-formatted HTTP response (status line + headers + body).
    /// When set, serialize() returns a copy of this instead of building the response.
    /// The caller must ensure the data remains valid until serialize() is called.
    pub fn setRaw(self: *Response, data: []const u8) void {
        self.raw_response = data;
    }

    /// Set response status
    pub fn setStatus(self: *Response, s: Status) *Response {
        self.status = s;
        return self;
    }

    /// Convenience for common status codes
    pub fn ok(self: *Response) *Response {
        return self.setStatus(.ok);
    }

    pub fn created(self: *Response) *Response {
        return self.setStatus(.created);
    }

    pub fn noContent(self: *Response) *Response {
        return self.setStatus(.no_content);
    }

    pub fn badRequest(self: *Response) *Response {
        return self.setStatus(.bad_request);
    }

    pub fn unauthorized(self: *Response) *Response {
        return self.setStatus(.unauthorized);
    }

    pub fn forbidden(self: *Response) *Response {
        return self.setStatus(.forbidden);
    }

    pub fn notFound(self: *Response) *Response {
        return self.setStatus(.not_found);
    }

    pub fn internalServerError(self: *Response) *Response {
        return self.setStatus(.internal_server_error);
    }

    /// Set a header
    pub fn header(self: *Response, name: []const u8, value: []const u8) *Response {
        self.headers.set(name, value) catch {};
        return self;
    }

    /// Set Content-Type header
    pub fn contentType(self: *Response, ct: []const u8) *Response {
        return self.header("Content-Type", ct);
    }

    /// Send JSON response using std.json.fmt
    pub fn json(self: *Response, value: anytype) !void {
        _ = self.contentType(ContentType.json);
        const json_str = std.fmt.allocPrint(self.allocator, "{f}", .{std.json.fmt(value, .{})}) catch |err| return err;
        defer self.allocator.free(json_str);
        try self.body.appendSlice(self.allocator, json_str);
    }

    /// Send JSON with pretty printing
    pub fn jsonPretty(self: *Response, value: anytype) !void {
        _ = self.contentType(ContentType.json);
        const json_str = std.fmt.allocPrint(self.allocator, "{f}", .{std.json.fmt(value, .{ .whitespace = .indent_2 })}) catch |err| return err;
        defer self.allocator.free(json_str);
        try self.body.appendSlice(self.allocator, json_str);
    }

    /// Send plain text response
    pub fn text(self: *Response, content: []const u8) !void {
        _ = self.contentType(ContentType.text);
        try self.body.appendSlice(self.allocator, content);
    }

    /// Send HTML response
    pub fn html(self: *Response, content: []const u8) !void {
        _ = self.contentType(ContentType.html);
        try self.body.appendSlice(self.allocator, content);
    }

    /// Send raw bytes
    pub fn send(self: *Response, data: []const u8) !void {
        try self.body.appendSlice(self.allocator, data);
    }

    /// Format and write the full HTTP response to a buffer
    pub fn writeTo(self: *Response, buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator) !void {
        // Status line
        try buffer.print(allocator, "HTTP/1.1 {} {s}\r\n", .{ self.status.code(), self.status.phrase() });

        // Content-Length (if we have a body)
        if (self.body.items.len > 0) {
            try buffer.print(allocator, "Content-Length: {}\r\n", .{self.body.items.len});
        }

        // Headers
        var iter = self.headers.iterator();
        while (iter.next()) |h| {
            try buffer.print(allocator, "{s}: {s}\r\n", .{ h.name, h.value });
        }

        // End of headers
        try buffer.appendSlice(allocator, "\r\n");

        // Body
        if (self.body.items.len > 0) {
            try buffer.appendSlice(allocator, self.body.items);
        }

        self.sent = true;
    }

    /// Serialize response to a buffer
    pub fn serialize(self: *Response, allocator: std.mem.Allocator) ![]u8 {
        if (self.raw_response) |raw| {
            const buf = try allocator.alloc(u8, raw.len);
            @memcpy(buf, raw);
            return buf;
        }
        var buffer: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buffer.deinit(allocator);
        try buffer.ensureTotalCapacity(allocator, self.body.items.len + 256);
        try self.writeTo(&buffer, allocator);
        return buffer.toOwnedSlice(allocator);
    }
};

/// Pre-built common responses
pub const CommonResponses = struct {
    pub fn notFound(allocator: std.mem.Allocator) ![]u8 {
        var res = Response.init(allocator);
        defer res.deinit();
        _ = res.notFound();
        try res.json(.{ .@"error" = "Not Found" });
        return res.serialize(allocator);
    }

    pub fn methodNotAllowed(allocator: std.mem.Allocator) ![]u8 {
        var res = Response.init(allocator);
        defer res.deinit();
        _ = res.setStatus(.method_not_allowed);
        try res.json(.{ .@"error" = "Method Not Allowed" });
        return res.serialize(allocator);
    }

    pub fn internalError(allocator: std.mem.Allocator) ![]u8 {
        var res = Response.init(allocator);
        defer res.deinit();
        _ = res.internalServerError();
        try res.json(.{ .@"error" = "Internal Server Error" });
        return res.serialize(allocator);
    }

    pub fn badRequest(allocator: std.mem.Allocator, message: []const u8) ![]u8 {
        var res = Response.init(allocator);
        defer res.deinit();
        _ = res.badRequest();
        try res.json(.{ .@"error" = message });
        return res.serialize(allocator);
    }
};

test "response building" {
    const allocator = std.testing.allocator;

    var res = Response.init(allocator);
    defer res.deinit();

    _ = res.ok().header("X-Custom", "value");
    try res.json(.{ .message = "Hello", .count = 42 });

    const output = try res.serialize(allocator);
    defer allocator.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "200 OK") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "application/json") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "Hello") != null);
}
