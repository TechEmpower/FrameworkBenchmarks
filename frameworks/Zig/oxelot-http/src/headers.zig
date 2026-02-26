const std = @import("std");

/// HTTP Headers collection
pub const Headers = struct {
    allocator: std.mem.Allocator,
    entries: std.StringArrayHashMapUnmanaged([]const u8),

    pub fn init(allocator: std.mem.Allocator) Headers {
        return .{
            .allocator = allocator,
            .entries = .{},
        };
    }

    pub fn deinit(self: *Headers) void {
        // Free all keys and values that we own
        for (self.entries.keys(), self.entries.values()) |key, value| {
            self.allocator.free(key);
            self.allocator.free(value);
        }
        self.entries.deinit(self.allocator);
    }

    /// Get a header value (case-insensitive lookup)
    pub fn get(self: *const Headers, name: []const u8) ?[]const u8 {
        // First try exact match
        if (self.entries.get(name)) |v| return v;

        // Try case-insensitive match
        for (self.entries.keys(), self.entries.values()) |key, value| {
            if (std.ascii.eqlIgnoreCase(key, name)) {
                return value;
            }
        }
        return null;
    }

    /// Set a header (overwrites existing)
    pub fn set(self: *Headers, name: []const u8, value: []const u8) !void {
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);
        const owned_value = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(owned_value);

        // Check for existing (case-insensitive)
        var found_key: ?[]const u8 = null;
        for (self.entries.keys()) |key| {
            if (std.ascii.eqlIgnoreCase(key, name)) {
                found_key = key;
                break;
            }
        }

        if (found_key) |existing_key| {
            // Free old value and key
            if (self.entries.fetchSwapRemove(existing_key)) |kv| {
                self.allocator.free(kv.key);
                self.allocator.free(kv.value);
            }
        }

        try self.entries.put(self.allocator, owned_name, owned_value);
    }

    /// Add a header (allows duplicates - appends with comma)
    pub fn add(self: *Headers, name: []const u8, value: []const u8) !void {
        if (self.get(name)) |existing| {
            // Append with comma
            const new_value = try std.fmt.allocPrint(self.allocator, "{s}, {s}", .{ existing, value });
            try self.set(name, new_value);
            self.allocator.free(new_value);
        } else {
            try self.set(name, value);
        }
    }

    /// Remove a header
    pub fn remove(self: *Headers, name: []const u8) bool {
        // Find case-insensitive match
        var found_key: ?[]const u8 = null;
        for (self.entries.keys()) |key| {
            if (std.ascii.eqlIgnoreCase(key, name)) {
                found_key = key;
                break;
            }
        }

        if (found_key) |existing_key| {
            if (self.entries.fetchSwapRemove(existing_key)) |kv| {
                self.allocator.free(kv.key);
                self.allocator.free(kv.value);
                return true;
            }
        }
        return false;
    }

    /// Check if header exists
    pub fn contains(self: *const Headers, name: []const u8) bool {
        return self.get(name) != null;
    }

    /// Get Content-Type header
    pub fn contentType(self: *const Headers) ?[]const u8 {
        return self.get("Content-Type");
    }

    /// Get Content-Length header as integer
    pub fn contentLength(self: *const Headers) ?usize {
        const value = self.get("Content-Length") orelse return null;
        return std.fmt.parseInt(usize, value, 10) catch null;
    }

    /// Iterator over headers
    pub fn iterator(self: *const Headers) Iterator {
        return .{
            .keys = self.entries.keys(),
            .values = self.entries.values(),
            .index = 0,
        };
    }

    pub const HeaderEntry = struct {
        name: []const u8,
        value: []const u8,
    };

    pub const Iterator = struct {
        keys: []const []const u8,
        values: []const []const u8,
        index: usize,

        pub fn next(self: *Iterator) ?HeaderEntry {
            if (self.index >= self.keys.len) return null;
            const result = HeaderEntry{
                .name = self.keys[self.index],
                .value = self.values[self.index],
            };
            self.index += 1;
            return result;
        }
    };

    /// Write headers to a writer in HTTP format
    pub fn write(self: *const Headers, writer: anytype) !void {
        var iter = self.iterator();
        while (iter.next()) |header| {
            try writer.print("{s}: {s}\r\n", .{ header.name, header.value });
        }
    }
};

/// Common header names
pub const CommonHeaders = struct {
    pub const content_type = "Content-Type";
    pub const content_length = "Content-Length";
    pub const connection = "Connection";
    pub const host = "Host";
    pub const user_agent = "User-Agent";
    pub const accept = "Accept";
    pub const accept_encoding = "Accept-Encoding";
    pub const accept_language = "Accept-Language";
    pub const authorization = "Authorization";
    pub const cache_control = "Cache-Control";
    pub const cookie = "Cookie";
    pub const set_cookie = "Set-Cookie";
    pub const location = "Location";
    pub const server = "Server";
    pub const date = "Date";
    pub const transfer_encoding = "Transfer-Encoding";
    pub const upgrade = "Upgrade";
    pub const sec_websocket_key = "Sec-WebSocket-Key";
    pub const sec_websocket_accept = "Sec-WebSocket-Accept";
    pub const sec_websocket_version = "Sec-WebSocket-Version";
};

/// Common content types
pub const ContentType = struct {
    pub const json = "application/json";
    pub const html = "text/html; charset=utf-8";
    pub const text = "text/plain; charset=utf-8";
    pub const xml = "application/xml";
    pub const form = "application/x-www-form-urlencoded";
    pub const multipart = "multipart/form-data";
    pub const octet_stream = "application/octet-stream";
};

test "headers basic operations" {
    const allocator = std.testing.allocator;
    var headers = Headers.init(allocator);
    defer headers.deinit();

    try headers.set("Content-Type", "application/json");
    try std.testing.expectEqualStrings("application/json", headers.get("Content-Type").?);
    try std.testing.expectEqualStrings("application/json", headers.get("content-type").?);

    try headers.set("Content-Length", "42");
    try std.testing.expectEqual(@as(?usize, 42), headers.contentLength());
}
