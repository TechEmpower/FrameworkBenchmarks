// SIMD-accelerated JSON serialization
//
// Serialization uses SWAR (SIMD Within A Register) for fast string escaping
// Based on V8's JSON.stringify optimizations
//
// Performance targets:
// - Minimize allocations by writing directly to buffers
// - Batch escape character detection using SWAR techniques
// - Zero-copy for strings without escape characters
//
// For parsing, use std.json which provides good performance for most use cases.
// When zimdjson adds Zig 0.16 support, we can integrate it for SIMD parsing.

const std = @import("std");

/// SWAR constants for escape character detection
/// These magic numbers help detect characters that need JSON escaping
const SWAR = struct {
    // Characters that need escaping: " (0x22), \ (0x5C), and control chars (0x00-0x1F)
    const QUOTE: u64 = 0x2222222222222222;
    const BACKSLASH: u64 = 0x5C5C5C5C5C5C5C5C;
    const LOW_NIBBLE_MASK: u64 = 0x0F0F0F0F0F0F0F0F;
    const HIGH_BIT_MASK: u64 = 0x8080808080808080;
    const CONTROL_CHAR_LIMIT: u64 = 0x2020202020202020; // Space (0x20) - anything below needs escaping

    /// Check if a u64 contains any bytes that need JSON escaping
    /// Returns true if any byte is: < 0x20, == 0x22 ("), or == 0x5C (\)
    inline fn hasEscapeChar(chunk: u64) bool {
        // Check for control characters (< 0x20)
        // If (byte - 0x20) has high bit set, byte was < 0x20
        const ctrl_check = chunk -% CONTROL_CHAR_LIMIT;
        const has_ctrl = (ctrl_check & HIGH_BIT_MASK) != 0;

        // Check for quote (0x22)
        const xor_quote = chunk ^ QUOTE;
        // If any byte is 0x22, XOR makes it 0x00
        // (x - 0x01) & ~x & 0x80 detects zero bytes
        const has_quote = ((xor_quote -% 0x0101010101010101) & ~xor_quote & HIGH_BIT_MASK) != 0;

        // Check for backslash (0x5C)
        const xor_backslash = chunk ^ BACKSLASH;
        const has_backslash = ((xor_backslash -% 0x0101010101010101) & ~xor_backslash & HIGH_BIT_MASK) != 0;

        return has_ctrl or has_quote or has_backslash;
    }
};

/// Escape table for control characters
const escape_table: [32][]const u8 = init_escape_table();

fn init_escape_table() [32][]const u8 {
    var table: [32][]const u8 = undefined;
    for (0..32) |i| {
        table[i] = switch (i) {
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            0x08 => "\\b", // backspace
            0x0C => "\\f", // form feed
            else => "\\u0000", // will be filled with actual hex
        };
    }
    return table;
}

/// Fast JSON string writer that writes directly to a buffer
pub const JsonWriter = struct {
    buffer: []u8,
    pos: usize,

    const Self = @This();

    pub fn init(buffer: []u8) Self {
        return .{ .buffer = buffer, .pos = 0 };
    }

    /// Get the written portion of the buffer
    pub fn getWritten(self: *const Self) []const u8 {
        return self.buffer[0..self.pos];
    }

    /// Remaining capacity
    pub fn remaining(self: *const Self) usize {
        return self.buffer.len - self.pos;
    }

    /// Write a raw byte
    pub inline fn writeByte(self: *Self, b: u8) !void {
        if (self.pos >= self.buffer.len) return error.BufferOverflow;
        self.buffer[self.pos] = b;
        self.pos += 1;
    }

    /// Write raw bytes without escaping
    pub inline fn writeRaw(self: *Self, bytes: []const u8) !void {
        if (self.pos + bytes.len > self.buffer.len) return error.BufferOverflow;
        @memcpy(self.buffer[self.pos..][0..bytes.len], bytes);
        self.pos += bytes.len;
    }

    /// Write a JSON string with SWAR-accelerated escaping
    pub fn writeString(self: *Self, s: []const u8) !void {
        try self.writeByte('"');

        var i: usize = 0;
        const len = s.len;

        // Process 8 bytes at a time using SWAR
        while (i + 8 <= len) {
            const chunk = std.mem.readInt(u64, s[i..][0..8], .little);

            if (!SWAR.hasEscapeChar(chunk)) {
                // Fast path: no escaping needed, copy 8 bytes
                try self.writeRaw(s[i..][0..8]);
                i += 8;
            } else {
                // Slow path: process byte by byte for this chunk
                const end = i + 8;
                while (i < end) {
                    try self.writeEscapedByte(s[i]);
                    i += 1;
                }
            }
        }

        // Handle remaining bytes
        while (i < len) {
            try self.writeEscapedByte(s[i]);
            i += 1;
        }

        try self.writeByte('"');
    }

    /// Write a single byte, escaping if necessary
    inline fn writeEscapedByte(self: *Self, c: u8) !void {
        switch (c) {
            '"' => try self.writeRaw("\\\""),
            '\\' => try self.writeRaw("\\\\"),
            '\n' => try self.writeRaw("\\n"),
            '\r' => try self.writeRaw("\\r"),
            '\t' => try self.writeRaw("\\t"),
            0x00...0x07, 0x0B, 0x0E...0x1F => {
                // Control characters need \uXXXX encoding
                var hex_buf: [6]u8 = "\\u0000".*;
                const hex_chars = "0123456789abcdef";
                hex_buf[4] = hex_chars[c >> 4];
                hex_buf[5] = hex_chars[c & 0x0F];
                try self.writeRaw(&hex_buf);
            },
            0x08 => try self.writeRaw("\\b"), // backspace
            0x0C => try self.writeRaw("\\f"), // form feed
            else => try self.writeByte(c),
        }
    }

    /// Write an integer
    pub fn writeInt(self: *Self, value: anytype) !void {
        const T = @TypeOf(value);
        const info = @typeInfo(T);

        if (info == .comptime_int) {
            // Handle comptime_int by converting to i64 or u64
            if (value < 0) {
                const buf = std.fmt.bufPrint(self.buffer[self.pos..], "{d}", .{@as(i64, value)}) catch return error.BufferOverflow;
                self.pos += buf.len;
            } else {
                const buf = std.fmt.bufPrint(self.buffer[self.pos..], "{d}", .{@as(u64, value)}) catch return error.BufferOverflow;
                self.pos += buf.len;
            }
        } else if (info == .int) {
            const buf = std.fmt.bufPrint(self.buffer[self.pos..], "{d}", .{value}) catch return error.BufferOverflow;
            self.pos += buf.len;
        } else {
            @compileError("writeInt requires an integer type");
        }
    }

    /// Write a float
    pub fn writeFloat(self: *Self, value: anytype) !void {
        const buf = std.fmt.bufPrint(self.buffer[self.pos..], "{d}", .{value}) catch return error.BufferOverflow;
        self.pos += buf.len;
    }

    /// Write a boolean
    pub fn writeBool(self: *Self, value: bool) !void {
        if (value) {
            try self.writeRaw("true");
        } else {
            try self.writeRaw("false");
        }
    }

    /// Write null
    pub fn writeNull(self: *Self) !void {
        try self.writeRaw("null");
    }

    /// Serialize any Zig value to JSON
    pub fn writeValue(self: *Self, value: anytype) !void {
        const T = @TypeOf(value);
        const info = @typeInfo(T);

        switch (info) {
            .null => try self.writeNull(),
            .bool => try self.writeBool(value),
            .int, .comptime_int => try self.writeInt(value),
            .float, .comptime_float => try self.writeFloat(value),
            .optional => {
                if (value) |v| {
                    try self.writeValue(v);
                } else {
                    try self.writeNull();
                }
            },
            .@"enum" => {
                try self.writeString(@tagName(value));
            },
            .@"union" => |u| {
                if (u.tag_type) |_| {
                    inline for (u.fields) |field| {
                        if (value == @field(T, field.name)) {
                            try self.writeValue(@field(value, field.name));
                            return;
                        }
                    }
                } else {
                    @compileError("Cannot serialize untagged union");
                }
            },
            .@"struct" => |s| {
                if (s.is_tuple) {
                    // Tuple -> JSON array
                    try self.writeByte('[');
                    inline for (s.fields, 0..) |field, i| {
                        if (i > 0) try self.writeByte(',');
                        try self.writeValue(@field(value, field.name));
                    }
                    try self.writeByte(']');
                } else {
                    // Struct -> JSON object
                    try self.writeByte('{');
                    var first = true;
                    inline for (s.fields) |field| {
                        // Skip void fields
                        if (@typeInfo(field.type) == .void) continue;

                        if (!first) try self.writeByte(',');
                        first = false;

                        try self.writeString(field.name);
                        try self.writeByte(':');
                        try self.writeValue(@field(value, field.name));
                    }
                    try self.writeByte('}');
                }
            },
            .array => |arr| {
                if (arr.child == u8) {
                    // u8 array -> string
                    try self.writeString(&value);
                } else {
                    // Other arrays -> JSON array
                    try self.writeByte('[');
                    for (value, 0..) |item, i| {
                        if (i > 0) try self.writeByte(',');
                        try self.writeValue(item);
                    }
                    try self.writeByte(']');
                }
            },
            .pointer => |ptr| {
                switch (ptr.size) {
                    .slice => {
                        if (ptr.child == u8) {
                            // []const u8 -> string
                            try self.writeString(value);
                        } else {
                            // Other slices -> JSON array
                            try self.writeByte('[');
                            for (value, 0..) |item, i| {
                                if (i > 0) try self.writeByte(',');
                                try self.writeValue(item);
                            }
                            try self.writeByte(']');
                        }
                    },
                    .one => {
                        try self.writeValue(value.*);
                    },
                    else => @compileError("Unsupported pointer type"),
                }
            },
            else => @compileError("Unsupported type for JSON serialization: " ++ @typeName(T)),
        }
    }
};

/// Convenience function to serialize a value to a buffer
pub fn stringify(buffer: []u8, value: anytype) ![]const u8 {
    var writer = JsonWriter.init(buffer);
    try writer.writeValue(value);
    return writer.getWritten();
}

/// Serialize to an allocated string
pub fn stringifyAlloc(allocator: std.mem.Allocator, value: anytype) ![]u8 {
    // Estimate initial size
    var size_estimate: usize = 256;

    while (true) {
        const buffer = try allocator.alloc(u8, size_estimate);
        errdefer allocator.free(buffer);

        var writer = JsonWriter.init(buffer);
        writer.writeValue(value) catch |err| {
            if (err == error.BufferOverflow) {
                allocator.free(buffer);
                size_estimate *= 2;
                continue;
            }
            return err;
        };

        // Shrink to actual size
        const result = try allocator.realloc(buffer, writer.pos);
        return result;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "SWAR escape detection" {
    // Normal ASCII - no escaping needed
    const normal: u64 = 0x6162636465666768; // "abcdefgh"
    try std.testing.expect(!SWAR.hasEscapeChar(normal));

    // Contains quote
    const with_quote: u64 = 0x6162632264656667; // "abc"defg"
    try std.testing.expect(SWAR.hasEscapeChar(with_quote));

    // Contains backslash
    const with_backslash: u64 = 0x616263645C656667; // "abcd\efg"
    try std.testing.expect(SWAR.hasEscapeChar(with_backslash));

    // Contains newline
    const with_newline: u64 = 0x616263640A656667; // "abcd\nefg"
    try std.testing.expect(SWAR.hasEscapeChar(with_newline));
}

test "simple object serialization" {
    var buffer: [256]u8 = undefined;
    const result = try stringify(&buffer, .{ .message = "Hello, World!" });
    try std.testing.expectEqualStrings("{\"message\":\"Hello, World!\"}", result);
}

test "nested object serialization" {
    var buffer: [256]u8 = undefined;
    const result = try stringify(&buffer, .{
        .name = "John",
        .age = @as(u32, 30),
        .active = true,
    });
    try std.testing.expectEqualStrings("{\"name\":\"John\",\"age\":30,\"active\":true}", result);
}

test "array serialization" {
    var buffer: [256]u8 = undefined;
    const items = [_]i32{ 1, 2, 3 };
    const result = try stringify(&buffer, items);
    try std.testing.expectEqualStrings("[1,2,3]", result);
}

test "string escaping" {
    var buffer: [256]u8 = undefined;

    // Test quote escaping
    const quote_result = try stringify(&buffer, .{ .text = "say \"hello\"" });
    try std.testing.expectEqualStrings("{\"text\":\"say \\\"hello\\\"\"}", quote_result);

    // Test newline escaping
    const newline_result = try stringify(&buffer, .{ .text = "line1\nline2" });
    try std.testing.expectEqualStrings("{\"text\":\"line1\\nline2\"}", newline_result);
}

test "null and optional" {
    var buffer: [256]u8 = undefined;

    const opt_null: ?i32 = null;
    const null_result = try stringify(&buffer, .{ .value = opt_null });
    try std.testing.expectEqualStrings("{\"value\":null}", null_result);

    const opt_value: ?i32 = 42;
    const value_result = try stringify(&buffer, .{ .value = opt_value });
    try std.testing.expectEqualStrings("{\"value\":42}", value_result);
}
