const std = @import("std");

/// JSON utility functions for common operations
pub const json = struct {
    /// Parse JSON string into a type
    pub fn parse(comptime T: type, allocator: std.mem.Allocator, input: []const u8) !std.json.Parsed(T) {
        return std.json.parseFromSlice(T, allocator, input, .{});
    }

    /// Parse JSON string, returning just the value (caller must manage memory)
    pub fn parseLeaky(comptime T: type, allocator: std.mem.Allocator, input: []const u8) !T {
        const parsed = try std.json.parseFromSlice(T, allocator, input, .{});
        return parsed.value;
    }

    /// Stringify a value to JSON
    pub fn stringify(value: anytype, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{f}", .{std.json.fmt(value, .{})});
    }

    /// Stringify with pretty printing
    pub fn stringifyPretty(value: anytype, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{f}", .{std.json.fmt(value, .{ .whitespace = .indent_2 })});
    }
};

/// JSON Object builder for dynamic JSON construction
pub const ObjectBuilder = struct {
    allocator: std.mem.Allocator,
    buffer: std.ArrayListUnmanaged(u8),
    field_count: usize,

    pub fn init(allocator: std.mem.Allocator) ObjectBuilder {
        var builder = ObjectBuilder{
            .allocator = allocator,
            .buffer = .empty,
            .field_count = 0,
        };
        builder.buffer.append(allocator, '{') catch {};
        return builder;
    }

    pub fn deinit(self: *ObjectBuilder) void {
        self.buffer.deinit(self.allocator);
    }

    /// Add a string field
    pub fn string(self: *ObjectBuilder, key: []const u8, str_val: []const u8) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        try self.buffer.append(self.allocator, '"');
        try self.writeEscapedString(str_val);
        try self.buffer.append(self.allocator, '"');
        return self;
    }

    /// Add an integer field
    pub fn int(self: *ObjectBuilder, key: []const u8, int_val: anytype) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        const num_str = try std.fmt.allocPrint(self.allocator, "{d}", .{int_val});
        defer self.allocator.free(num_str);
        try self.buffer.appendSlice(self.allocator, num_str);
        return self;
    }

    /// Add a float field
    pub fn float(self: *ObjectBuilder, key: []const u8, float_val: anytype) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        const num_str = try std.fmt.allocPrint(self.allocator, "{d}", .{float_val});
        defer self.allocator.free(num_str);
        try self.buffer.appendSlice(self.allocator, num_str);
        return self;
    }

    /// Add a boolean field
    pub fn boolean(self: *ObjectBuilder, key: []const u8, bool_val: bool) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        try self.buffer.appendSlice(self.allocator, if (bool_val) "true" else "false");
        return self;
    }

    /// Add a null field
    pub fn nil(self: *ObjectBuilder, key: []const u8) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        try self.buffer.appendSlice(self.allocator, "null");
        return self;
    }

    /// Add a pre-serialized JSON value
    pub fn raw(self: *ObjectBuilder, key: []const u8, json_value: []const u8) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        try self.buffer.appendSlice(self.allocator, json_value);
        return self;
    }

    /// Add any serializable value
    pub fn value(self: *ObjectBuilder, key: []const u8, val: anytype) !*ObjectBuilder {
        try self.writeComma();
        try self.writeKey(key);
        const json_str = try std.fmt.allocPrint(self.allocator, "{f}", .{std.json.fmt(val, .{})});
        defer self.allocator.free(json_str);
        try self.buffer.appendSlice(self.allocator, json_str);
        return self;
    }

    fn writeComma(self: *ObjectBuilder) !void {
        if (self.field_count > 0) {
            try self.buffer.append(self.allocator, ',');
        }
        self.field_count += 1;
    }

    fn writeKey(self: *ObjectBuilder, key: []const u8) !void {
        try self.buffer.append(self.allocator, '"');
        try self.buffer.appendSlice(self.allocator, key);
        try self.buffer.appendSlice(self.allocator, "\":");
    }

    fn writeEscapedString(self: *ObjectBuilder, s: []const u8) !void {
        for (s) |c| {
            switch (c) {
                '"' => try self.buffer.appendSlice(self.allocator, "\\\""),
                '\\' => try self.buffer.appendSlice(self.allocator, "\\\\"),
                '\n' => try self.buffer.appendSlice(self.allocator, "\\n"),
                '\r' => try self.buffer.appendSlice(self.allocator, "\\r"),
                '\t' => try self.buffer.appendSlice(self.allocator, "\\t"),
                else => {
                    if (c < 0x20) {
                        const escape_str = try std.fmt.allocPrint(self.allocator, "\\u{x:0>4}", .{c});
                        defer self.allocator.free(escape_str);
                        try self.buffer.appendSlice(self.allocator, escape_str);
                    } else {
                        try self.buffer.append(self.allocator, c);
                    }
                },
            }
        }
    }

    /// Finalize and return the JSON string
    pub fn build(self: *ObjectBuilder) ![]u8 {
        try self.buffer.append(self.allocator, '}');
        return self.buffer.toOwnedSlice(self.allocator);
    }
};

/// JSON Array builder
pub const ArrayBuilder = struct {
    allocator: std.mem.Allocator,
    buffer: std.ArrayListUnmanaged(u8),
    item_count: usize,

    pub fn init(allocator: std.mem.Allocator) ArrayBuilder {
        var builder = ArrayBuilder{
            .allocator = allocator,
            .buffer = .empty,
            .item_count = 0,
        };
        builder.buffer.append(allocator, '[') catch {};
        return builder;
    }

    pub fn deinit(self: *ArrayBuilder) void {
        self.buffer.deinit(self.allocator);
    }

    /// Add a value to the array
    pub fn push(self: *ArrayBuilder, val: anytype) !*ArrayBuilder {
        if (self.item_count > 0) {
            try self.buffer.append(self.allocator, ',');
        }
        self.item_count += 1;
        const json_str = try std.fmt.allocPrint(self.allocator, "{f}", .{std.json.fmt(val, .{})});
        defer self.allocator.free(json_str);
        try self.buffer.appendSlice(self.allocator, json_str);
        return self;
    }

    /// Add a raw JSON value
    pub fn pushRaw(self: *ArrayBuilder, json_value: []const u8) !*ArrayBuilder {
        if (self.item_count > 0) {
            try self.buffer.append(self.allocator, ',');
        }
        self.item_count += 1;
        try self.buffer.appendSlice(self.allocator, json_value);
        return self;
    }

    /// Finalize and return the JSON array string
    pub fn build(self: *ArrayBuilder) ![]u8 {
        try self.buffer.append(self.allocator, ']');
        return self.buffer.toOwnedSlice(self.allocator);
    }
};

test "json object builder" {
    const allocator = std.testing.allocator;

    var builder = ObjectBuilder.init(allocator);
    defer builder.deinit();

    _ = try builder.string("name", "John");
    _ = try builder.int("age", 30);
    _ = try builder.boolean("active", true);

    const result = try builder.build();
    defer allocator.free(result);

    try std.testing.expectEqualStrings("{\"name\":\"John\",\"age\":30,\"active\":true}", result);
}

test "json array builder" {
    const allocator = std.testing.allocator;

    var builder = ArrayBuilder.init(allocator);
    defer builder.deinit();

    _ = try builder.push("hello");
    _ = try builder.push(@as(i32, 42));
    _ = try builder.push(true);

    const result = try builder.build();
    defer allocator.free(result);

    try std.testing.expectEqualStrings("[\"hello\",42,true]", result);
}
