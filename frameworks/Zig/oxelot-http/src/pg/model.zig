// PostgreSQL struct mapping for oxelot-http
//
// Provides comptime-based mapping between Zig structs and PostgreSQL rows:
// - Zero-copy parsing: string fields point directly into PGresult memory
// - Comptime SQL generation: INSERT statements built at compile time
//
// Note: Parsed structs are only valid while the QueryResult exists.
// String slices point into PGresult memory and are invalidated on deinit().

const std = @import("std");
const pg = @import("../pg.zig");

pub const ModelError = error{
    /// Column not found in query result
    ColumnNotFound,
    /// Expected a value but got NULL
    UnexpectedNull,
    /// Failed to parse value to target type
    ParseError,
    /// No rows in result
    NoRows,
};

/// Parse a single value from PostgreSQL text format to Zig type T.
/// Handles optionals, integers, floats, booleans, and string slices.
pub fn parseValue(comptime T: type, value: ?[]const u8) ModelError!T {
    const info = @typeInfo(T);

    // Handle optionals - null value maps to null
    if (info == .optional) {
        if (value == null) return null;
        return try parseValue(info.optional.child, value);
    }

    // Non-optional types require a value
    const v = value orelse return ModelError.UnexpectedNull;

    return switch (info) {
        .int => std.fmt.parseInt(T, v, 10) catch return ModelError.ParseError,
        .float => std.fmt.parseFloat(T, v) catch return ModelError.ParseError,
        .bool => v.len > 0 and v[0] == 't',
        .pointer => |p| blk: {
            if (p.size == .slice and p.child == u8) {
                // []const u8 - return slice directly (zero-copy)
                break :blk v;
            } else {
                @compileError("Unsupported pointer type for parsing: " ++ @typeName(T));
            }
        },
        .@"enum" => |e| blk: {
            // Try to parse enum from string name
            inline for (e.fields) |field| {
                if (std.mem.eql(u8, v, field.name)) {
                    break :blk @enumFromInt(field.value);
                }
            }
            return ModelError.ParseError;
        },
        else => @compileError("Unsupported type for parsing: " ++ @typeName(T)),
    };
}

/// Parse a Row into a struct T.
/// Field names must match column names exactly.
/// Returns ColumnNotFound if a struct field doesn't exist in the query result.
pub fn parseRow(comptime T: type, row: pg.Row) ModelError!T {
    const info = @typeInfo(T);
    if (info != .@"struct") {
        @compileError("parseRow requires a struct type, got: " ++ @typeName(T));
    }

    var instance: T = undefined;

    inline for (info.@"struct".fields) |field| {
        // First check if column exists
        const col_index = row.columnIndex(field.name) orelse {
            return ModelError.ColumnNotFound;
        };
        // Then get the value (may be null for NULL values)
        const value = row.get(col_index);
        @field(instance, field.name) = try parseValue(field.type, value);
    }

    return instance;
}

/// Generate INSERT SQL at comptime from struct type.
/// Example: insertSql("users", User) -> "INSERT INTO users (id, email, name) VALUES ($1, $2, $3)"
pub fn insertSql(comptime table: []const u8, comptime T: type) []const u8 {
    const info = @typeInfo(T);
    if (info != .@"struct") {
        @compileError("insertSql requires a struct type, got: " ++ @typeName(T));
    }

    const fields = info.@"struct".fields;
    comptime var sql: []const u8 = "INSERT INTO " ++ table ++ " (";
    comptime var params: []const u8 = ") VALUES (";

    inline for (fields, 0..) |field, i| {
        if (i > 0) {
            sql = sql ++ ", ";
            params = params ++ ", ";
        }
        sql = sql ++ field.name;
        params = params ++ "$" ++ comptime intToString(i + 1);
    }

    return sql ++ params ++ ")";
}

/// Generate INSERT SQL with RETURNING clause
pub fn insertSqlReturning(comptime table: []const u8, comptime T: type, comptime returning: []const u8) []const u8 {
    const base = comptime insertSql(table, T);
    return base ++ " RETURNING " ++ returning;
}

/// Generate UPDATE SQL at comptime from struct type.
/// Example: updateSql("users", User, "WHERE id = $4") -> "UPDATE users SET email = $1, name = $2, active = $3 WHERE id = $4"
pub fn updateSql(comptime table: []const u8, comptime T: type, comptime where: []const u8) []const u8 {
    const info = @typeInfo(T);
    if (info != .@"struct") {
        @compileError("updateSql requires a struct type, got: " ++ @typeName(T));
    }

    const fields = info.@"struct".fields;
    comptime var sql: []const u8 = "UPDATE " ++ table ++ " SET ";

    inline for (fields, 0..) |field, i| {
        if (i > 0) {
            sql = sql ++ ", ";
        }
        sql = sql ++ field.name ++ " = $" ++ comptime intToString(i + 1);
    }

    return sql ++ " " ++ where;
}

/// Convert integer to string at comptime
fn intToString(comptime val: usize) []const u8 {
    if (val == 0) return "0";

    comptime var result: []const u8 = "";
    comptime var n = val;

    inline while (n > 0) {
        const digit = @as(u8, @intCast(n % 10)) + '0';
        result = &[_]u8{digit} ++ result;
        n /= 10;
    }

    return result;
}

/// Struct iterator for parsing rows
pub fn StructIterator(comptime T: type) type {
    return struct {
        result: *const pg.QueryResult,
        current: usize,
        total: usize,

        const Self = @This();

        pub fn next(self: *Self) ?T {
            if (self.current >= self.total) return null;
            const row = pg.Row{
                .result = self.result,
                .index = self.current,
            };
            self.current += 1;
            return parseRow(T, row) catch null;
        }

        /// Reset iterator to beginning
        pub fn reset(self: *Self) void {
            self.current = 0;
        }
    };
}

/// Buffer for formatting values to strings for INSERT/UPDATE
pub const ParamBuffer = struct {
    bufs: [32][64]u8,
    count: usize,

    pub fn init() ParamBuffer {
        return .{
            .bufs = undefined,
            .count = 0,
        };
    }

    /// Format a value to string, storing in internal buffer
    pub fn format(self: *ParamBuffer, comptime T: type, value: T) ?[]const u8 {
        if (self.count >= 32) return null;

        const result = formatValueInternal(T, value, &self.bufs[self.count]);
        if (result) |_| {
            self.count += 1;
        }
        return result;
    }
};

/// Format a value to string for use as a query parameter
fn formatValueInternal(comptime T: type, value: T, buf: *[64]u8) ?[]const u8 {
    const info = @typeInfo(T);

    return switch (info) {
        .int => std.fmt.bufPrint(buf, "{d}", .{value}) catch null,
        .float => std.fmt.bufPrint(buf, "{d}", .{value}) catch null,
        .bool => if (value) "t" else "f",
        .optional => if (value) |v| formatValueInternal(@TypeOf(v), v, buf) else null,
        .pointer => |p| blk: {
            if (p.size == .slice and p.child == u8) {
                break :blk value;
            } else {
                @compileError("Unsupported pointer type for formatting: " ++ @typeName(T));
            }
        },
        .@"enum" => @tagName(value),
        else => @compileError("Unsupported type for formatting: " ++ @typeName(T)),
    };
}

/// Build parameter array from struct fields
pub fn structToParams(
    comptime T: type,
    value: T,
    param_buf: *ParamBuffer,
    out_params: []?[]const u8,
) void {
    const info = @typeInfo(T);
    if (info != .@"struct") {
        @compileError("structToParams requires a struct type");
    }

    inline for (info.@"struct".fields, 0..) |field, i| {
        if (i < out_params.len) {
            out_params[i] = param_buf.format(field.type, @field(value, field.name));
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "parseValue - integers" {
    try std.testing.expectEqual(@as(i32, 42), try parseValue(i32, "42"));
    try std.testing.expectEqual(@as(i64, -123), try parseValue(i64, "-123"));
    try std.testing.expectEqual(@as(i16, 0), try parseValue(i16, "0"));
}

test "parseValue - floats" {
    try std.testing.expectApproxEqAbs(@as(f64, 3.14), try parseValue(f64, "3.14"), 0.001);
    try std.testing.expectApproxEqAbs(@as(f32, -2.5), try parseValue(f32, "-2.5"), 0.001);
}

test "parseValue - booleans" {
    try std.testing.expectEqual(true, try parseValue(bool, "t"));
    try std.testing.expectEqual(false, try parseValue(bool, "f"));
    try std.testing.expectEqual(false, try parseValue(bool, "false"));
}

test "parseValue - strings" {
    const s = try parseValue([]const u8, "hello");
    try std.testing.expectEqualStrings("hello", s);
}

test "parseValue - optionals" {
    try std.testing.expectEqual(@as(?i32, null), try parseValue(?i32, null));
    try std.testing.expectEqual(@as(?i32, 42), try parseValue(?i32, "42"));
    try std.testing.expectEqual(@as(?[]const u8, null), try parseValue(?[]const u8, null));
}

test "parseValue - null error" {
    try std.testing.expectError(ModelError.UnexpectedNull, parseValue(i32, null));
}

test "parseValue - parse error" {
    try std.testing.expectError(ModelError.ParseError, parseValue(i32, "not a number"));
}

test "insertSql generation" {
    const TestStruct = struct {
        id: i64,
        name: []const u8,
        active: bool,
    };

    const sql = insertSql("users", TestStruct);
    try std.testing.expectEqualStrings("INSERT INTO users (id, name, active) VALUES ($1, $2, $3)", sql);
}

test "insertSqlReturning generation" {
    const TestStruct = struct {
        email: []const u8,
        name: []const u8,
    };

    const sql = insertSqlReturning("users", TestStruct, "id");
    try std.testing.expectEqualStrings("INSERT INTO users (email, name) VALUES ($1, $2) RETURNING id", sql);
}

test "updateSql generation" {
    const TestStruct = struct {
        email: []const u8,
        name: []const u8,
    };

    const sql = updateSql("users", TestStruct, "WHERE id = $3");
    try std.testing.expectEqualStrings("UPDATE users SET email = $1, name = $2 WHERE id = $3", sql);
}

test "intToString" {
    try std.testing.expectEqualStrings("0", intToString(0));
    try std.testing.expectEqualStrings("1", intToString(1));
    try std.testing.expectEqualStrings("42", intToString(42));
    try std.testing.expectEqualStrings("123", intToString(123));
    try std.testing.expectEqualStrings("4320", intToString(4320));
}

test "formatValue" {
    var buf = ParamBuffer.init();

    try std.testing.expectEqualStrings("42", buf.format(i32, 42).?);
    try std.testing.expectEqualStrings("t", buf.format(bool, true).?);
    try std.testing.expectEqualStrings("f", buf.format(bool, false).?);
    try std.testing.expectEqualStrings("hello", buf.format([]const u8, "hello").?);
    try std.testing.expectEqual(@as(?[]const u8, null), buf.format(?i32, null));
}
