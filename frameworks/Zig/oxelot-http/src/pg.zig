// PostgreSQL client with connection pooling
//
// Features:
// - Connection pool with configurable size
// - Synchronous query execution (for simplicity in request handlers)
// - Result to JSON conversion
// - Parameterized queries to prevent SQL injection
// - SQL-based migrations with up/down support
//
// For async operations, the caller can run queries in a separate thread
// and use the io_uring event loop for other work.

const std = @import("std");

// Per-thread dedicated connection (for benchmarks / threadlocal usage)
pub const connection = @import("pg/connection.zig");
pub const Connection = connection.Connection;
pub const ConnConfig = connection.ConnConfig;

// Async connection pool (for io_uring integration)
pub const async_pool = @import("pg/async_pool.zig");
pub const DbConnPool = async_pool.DbConnPool;
pub const DbPoolConfig = async_pool.DbPoolConfig;
pub const DbRequest = async_pool.DbRequest;
pub const DbResult = async_pool.DbResult;
pub const DbOperation = async_pool.DbOperation;
pub const DbCallback = async_pool.DbCallback;
pub const PreparedStmt = async_pool.PreparedStmt;
pub const ResponseList = async_pool.ResponseList;

// Migrations module
pub const migrations = @import("pg/migrations.zig");
pub const Migrations = migrations.Migrations;
pub const MigrationStatus = migrations.MigrationStatus;
pub const MigrationError = migrations.MigrationError;
pub const createMigrationFile = migrations.createMigrationFile;
pub const generateMigrationName = migrations.generateMigrationName;

// Model/struct mapping module
pub const model = @import("pg/model.zig");
pub const ModelError = model.ModelError;
pub const parseValue = model.parseValue;
pub const parseRow = model.parseRow;
pub const insertSql = model.insertSql;
pub const updateSql = model.updateSql;
pub const StructIterator = model.StructIterator;
pub const ParamBuffer = model.ParamBuffer;

// Import libpq C library
pub const c = @cImport({
    @cInclude("libpq-fe.h");
});

/// PostgreSQL connection pool configuration
pub const PoolConfig = struct {
    /// PostgreSQL host
    host: []const u8 = "localhost",
    /// PostgreSQL port
    port: u16 = 5432,
    /// Database name
    database: []const u8,
    /// Username
    username: []const u8,
    /// Password
    password: []const u8 = "",
    /// Number of connections in the pool
    pool_size: u16 = 10,
    /// Connection timeout in milliseconds
    connect_timeout_ms: u32 = 5000,
    /// Query timeout in milliseconds
    query_timeout_ms: u32 = 30000,
};

/// A pooled PostgreSQL connection
const PooledConnection = struct {
    conn: *c.PGconn,
    in_use: std.atomic.Value(bool),
};

/// PostgreSQL connection pool
pub const Pool = struct {
    connections: []PooledConnection,
    allocator: std.mem.Allocator,
    config: PoolConfig,

    /// Initialize the connection pool
    pub fn init(allocator: std.mem.Allocator, config: PoolConfig) !Pool {
        const connections = try allocator.alloc(PooledConnection, config.pool_size);
        errdefer allocator.free(connections);

        // Build connection string
        const conn_str = try std.fmt.allocPrintSentinel(
            allocator,
            "host={s} port={d} dbname={s} user={s} password={s} connect_timeout={d}",
            .{
                config.host,
                config.port,
                config.database,
                config.username,
                config.password,
                config.connect_timeout_ms / 1000,
            },
            0,
        );
        defer allocator.free(conn_str);

        // Create connections
        var created: usize = 0;
        errdefer {
            for (connections[0..created]) |*pc| {
                c.PQfinish(pc.conn);
            }
        }

        for (connections) |*pc| {
            const pg_conn = c.PQconnectdb(conn_str.ptr) orelse {
                return error.ConnectionFailed;
            };

            if (c.PQstatus(pg_conn) != c.CONNECTION_OK) {
                const err_msg = c.PQerrorMessage(pg_conn);
                std.log.err("PostgreSQL connection failed: {s}", .{std.mem.span(err_msg)});
                c.PQfinish(pg_conn);
                return error.ConnectionFailed;
            }

            pc.* = .{
                .conn = pg_conn,
                .in_use = std.atomic.Value(bool).init(false),
            };
            created += 1;
        }

        std.log.info("PostgreSQL pool initialized with {d} connections to {s}:{d}/{s}", .{
            config.pool_size,
            config.host,
            config.port,
            config.database,
        });

        return .{
            .connections = connections,
            .allocator = allocator,
            .config = config,
        };
    }

    /// Deinitialize the pool and close all connections
    pub fn deinit(self: *Pool) void {
        for (self.connections) |*pc| {
            c.PQfinish(pc.conn);
        }
        self.allocator.free(self.connections);
    }

    /// Acquire a connection from the pool
    pub fn acquire(self: *Pool) !*c.PGconn {
        // Try to find an available connection
        for (self.connections) |*pc| {
            if (pc.in_use.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
                // Check if connection is still valid
                if (c.PQstatus(pc.conn) != c.CONNECTION_OK) {
                    // Try to reset the connection
                    c.PQreset(pc.conn);
                    if (c.PQstatus(pc.conn) != c.CONNECTION_OK) {
                        pc.in_use.store(false, .release);
                        continue;
                    }
                }
                return pc.conn;
            }
        }

        return error.NoAvailableConnections;
    }

    /// Release a connection back to the pool
    pub fn release(self: *Pool, conn: *c.PGconn) void {
        for (self.connections) |*pc| {
            if (pc.conn == conn) {
                pc.in_use.store(false, .release);
                return;
            }
        }
    }

    /// Execute a query and return results
    pub fn query(self: *Pool, sql: []const u8) !QueryResult {
        const conn = try self.acquire();
        defer self.release(conn);

        return executeQuery(self.allocator, conn, sql);
    }

    /// Execute a parameterized query
    pub fn queryParams(self: *Pool, sql: []const u8, params: []const []const u8) !QueryResult {
        const conn = try self.acquire();
        defer self.release(conn);

        return executeQueryParams(self.allocator, conn, sql, params);
    }

    /// Execute a query and return results as JSON
    pub fn queryJson(self: *Pool, sql: []const u8) ![]u8 {
        var result = try self.query(sql);
        defer result.deinit();
        return result.toJson(self.allocator);
    }

    /// Execute a parameterized query and return results as JSON
    pub fn queryParamsJson(self: *Pool, sql: []const u8, params: []const []const u8) ![]u8 {
        var result = try self.queryParams(sql, params);
        defer result.deinit();
        return result.toJson(self.allocator);
    }

    /// Insert a struct into a table.
    /// SQL is generated at comptime from the struct fields.
    /// Example: pool.insert("users", user)
    pub fn insert(self: *Pool, comptime table: []const u8, value: anytype) !void {
        const T = @TypeOf(value);
        const sql = comptime model.insertSql(table, T);
        const fields = @typeInfo(T).@"struct".fields;

        // Format values to strings
        var param_buf = model.ParamBuffer.init();
        var params: [fields.len]?[]const u8 = undefined;
        model.structToParams(T, value, &param_buf, &params);

        var result = try self.queryParamsNullable(sql, &params);
        result.deinit();
    }

    /// Insert a struct and return a parsed result (e.g., for RETURNING clause).
    /// Example: const user = try pool.insertReturning("users", new_user, User, "id, created_at");
    pub fn insertReturning(
        self: *Pool,
        comptime table: []const u8,
        value: anytype,
        comptime ReturnT: type,
        comptime returning: []const u8,
    ) !ReturnT {
        const T = @TypeOf(value);
        const sql = comptime model.insertSqlReturning(table, T, returning);
        const fields = @typeInfo(T).@"struct".fields;

        // Format values to strings
        var param_buf = model.ParamBuffer.init();
        var params: [fields.len]?[]const u8 = undefined;
        model.structToParams(T, value, &param_buf, &params);

        var result = try self.queryParamsNullable(sql, &params);
        defer result.deinit();

        return result.parse(ReturnT);
    }

    /// Execute a parameterized query with nullable parameters (for INSERT with NULL values)
    pub fn queryParamsNullable(self: *Pool, sql: []const u8, params: []const ?[]const u8) !QueryResult {
        const conn = try self.acquire();
        defer self.release(conn);

        return executeQueryParamsNullable(self.allocator, conn, sql, params);
    }
};

/// Execute a query on a connection
fn executeQuery(allocator: std.mem.Allocator, conn: *c.PGconn, sql: []const u8) !QueryResult {
    // Null-terminate for libpq
    const sql_z = try allocator.allocSentinel(u8, sql.len, 0);
    defer allocator.free(sql_z);
    @memcpy(sql_z, sql);

    const result = c.PQexec(conn, sql_z.ptr) orelse {
        return error.QueryFailed;
    };

    const status = c.PQresultStatus(result);
    if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
        const err_msg = c.PQerrorMessage(conn);
        std.log.err("Query failed: {s}", .{std.mem.span(err_msg)});
        c.PQclear(result);
        return error.QueryFailed;
    }

    return QueryResult{
        .result = result,
        .allocator = allocator,
    };
}

/// Execute a parameterized query on a connection
fn executeQueryParams(
    allocator: std.mem.Allocator,
    conn: *c.PGconn,
    sql: []const u8,
    params: []const []const u8,
) !QueryResult {
    // Null-terminate SQL
    const sql_z = try allocator.allocSentinel(u8, sql.len, 0);
    defer allocator.free(sql_z);
    @memcpy(sql_z, sql);

    // Convert params to C format
    const param_values = try allocator.alloc([*c]const u8, params.len);
    defer allocator.free(param_values);

    const param_copies = try allocator.alloc([:0]u8, params.len);
    defer {
        for (param_copies) |copy| {
            allocator.free(copy);
        }
        allocator.free(param_copies);
    }

    for (params, 0..) |param, i| {
        param_copies[i] = try allocator.allocSentinel(u8, param.len, 0);
        @memcpy(param_copies[i], param);
        param_values[i] = param_copies[i].ptr;
    }

    const result = c.PQexecParams(
        conn,
        sql_z.ptr,
        @intCast(params.len),
        null, // Let PostgreSQL infer types
        @ptrCast(param_values.ptr),
        null, // Text format for all
        null, // Text format for all
        0, // Text result format
    ) orelse {
        return error.QueryFailed;
    };

    const status = c.PQresultStatus(result);
    if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
        const err_msg = c.PQerrorMessage(conn);
        std.log.err("Query failed: {s}", .{std.mem.span(err_msg)});
        c.PQclear(result);
        return error.QueryFailed;
    }

    return QueryResult{
        .result = result,
        .allocator = allocator,
    };
}

/// Execute a parameterized query with nullable parameters
fn executeQueryParamsNullable(
    allocator: std.mem.Allocator,
    conn: *c.PGconn,
    sql: []const u8,
    params: []const ?[]const u8,
) !QueryResult {
    // Null-terminate SQL
    const sql_z = try allocator.allocSentinel(u8, sql.len, 0);
    defer allocator.free(sql_z);
    @memcpy(sql_z, sql);

    // Convert params to C format, preserving nulls
    // Use [*c]const u8 (non-optional) since PQexecParams expects null as a valid value
    const param_values = try allocator.alloc([*c]const u8, params.len);
    defer allocator.free(param_values);

    // Allocate space for null-terminated copies
    const param_copies = try allocator.alloc(?[:0]u8, params.len);
    defer {
        for (param_copies) |maybe_copy| {
            if (maybe_copy) |copy| {
                allocator.free(copy);
            }
        }
        allocator.free(param_copies);
    }

    for (params, 0..) |param, i| {
        if (param) |p| {
            const copy = try allocator.allocSentinel(u8, p.len, 0);
            @memcpy(copy, p);
            param_copies[i] = copy;
            param_values[i] = copy.ptr;
        } else {
            param_copies[i] = null;
            param_values[i] = null; // NULL parameter for PostgreSQL
        }
    }

    const result = c.PQexecParams(
        conn,
        sql_z.ptr,
        @intCast(params.len),
        null, // Let PostgreSQL infer types
        @ptrCast(param_values.ptr),
        null, // Text format for all
        null, // Text format for all
        0, // Text result format
    ) orelse {
        return error.QueryFailed;
    };

    const status = c.PQresultStatus(result);
    if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
        const err_msg = c.PQerrorMessage(conn);
        std.log.err("Query failed: {s}", .{std.mem.span(err_msg)});
        c.PQclear(result);
        return error.QueryFailed;
    }

    return QueryResult{
        .result = result,
        .allocator = allocator,
    };
}

/// Query result wrapper
pub const QueryResult = struct {
    result: *c.PGresult,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *QueryResult) void {
        c.PQclear(self.result);
    }

    /// Number of rows in the result
    pub fn rowCount(self: *const QueryResult) usize {
        return @intCast(c.PQntuples(self.result));
    }

    /// Number of columns in the result
    pub fn columnCount(self: *const QueryResult) usize {
        return @intCast(c.PQnfields(self.result));
    }

    /// Get column name
    pub fn columnName(self: *const QueryResult, col: usize) []const u8 {
        const name = c.PQfname(self.result, @intCast(col));
        return std.mem.span(name);
    }

    /// Get value at row, column
    pub fn getValue(self: *const QueryResult, row: usize, col: usize) ?[]const u8 {
        if (c.PQgetisnull(self.result, @intCast(row), @intCast(col)) == 1) {
            return null;
        }
        const value = c.PQgetvalue(self.result, @intCast(row), @intCast(col));
        return std.mem.span(value);
    }

    /// Check if value is null
    pub fn isNull(self: *const QueryResult, row: usize, col: usize) bool {
        return c.PQgetisnull(self.result, @intCast(row), @intCast(col)) == 1;
    }

    /// Get column type OID
    pub fn columnType(self: *const QueryResult, col: usize) u32 {
        return c.PQftype(self.result, @intCast(col));
    }

    /// Convert result to JSON array
    pub fn toJson(self: *const QueryResult, allocator: std.mem.Allocator) ![]u8 {
        const num_rows = self.rowCount();
        const num_cols = self.columnCount();

        // Estimate buffer size
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(allocator);

        try buf.append(allocator, '[');

        for (0..num_rows) |row| {
            if (row > 0) try buf.append(allocator, ',');
            try buf.append(allocator, '{');

            for (0..num_cols) |col| {
                if (col > 0) try buf.append(allocator, ',');

                // Write column name
                try buf.append(allocator, '"');
                try buf.appendSlice(allocator, self.columnName(col));
                try buf.appendSlice(allocator, "\":");

                // Write value
                if (self.isNull(row, col)) {
                    try buf.appendSlice(allocator, "null");
                } else {
                    const value = self.getValue(row, col).?;
                    const col_type = self.columnType(col);

                    // Check if numeric type
                    if (isNumericType(col_type)) {
                        try buf.appendSlice(allocator, value);
                    } else if (isBoolType(col_type)) {
                        // PostgreSQL returns 't' or 'f' for booleans
                        if (value.len > 0 and value[0] == 't') {
                            try buf.appendSlice(allocator, "true");
                        } else {
                            try buf.appendSlice(allocator, "false");
                        }
                    } else {
                        // String - escape and quote
                        try buf.append(allocator, '"');
                        try escapeJsonString(&buf, allocator, value);
                        try buf.append(allocator, '"');
                    }
                }
            }

            try buf.append(allocator, '}');
        }

        try buf.append(allocator, ']');

        return try buf.toOwnedSlice(allocator);
    }

    /// Iterator for rows
    pub fn rows(self: *const QueryResult) RowIterator {
        return RowIterator{
            .result = self,
            .current = 0,
            .total = self.rowCount(),
        };
    }

    /// Parse first row into struct T.
    /// String fields point into PGresult memory (zero-copy).
    /// Returns error.NoRows if result is empty.
    pub fn parse(self: *const QueryResult, comptime T: type) !T {
        if (self.rowCount() == 0) return ModelError.NoRows;
        const row = Row{ .result = self, .index = 0 };
        return model.parseRow(T, row);
    }

    /// Parse all rows into a slice of struct T.
    /// String fields point into PGresult memory (zero-copy).
    /// Caller must free the returned slice (but not the strings within).
    pub fn parseAll(self: *const QueryResult, comptime T: type, allocator: std.mem.Allocator) ![]T {
        const count = self.rowCount();
        const items = try allocator.alloc(T, count);
        errdefer allocator.free(items);

        for (0..count) |i| {
            const row = Row{ .result = self, .index = i };
            items[i] = try model.parseRow(T, row);
        }

        return items;
    }

    /// Iterator that parses rows into struct T.
    /// String fields point into PGresult memory (zero-copy).
    pub fn iter(self: *const QueryResult, comptime T: type) model.StructIterator(T) {
        return .{
            .result = self,
            .current = 0,
            .total = self.rowCount(),
        };
    }
};

/// Row iterator
pub const RowIterator = struct {
    result: *const QueryResult,
    current: usize,
    total: usize,

    pub fn next(self: *RowIterator) ?Row {
        if (self.current >= self.total) return null;
        const row = Row{
            .result = self.result,
            .index = self.current,
        };
        self.current += 1;
        return row;
    }
};

/// Single row accessor
pub const Row = struct {
    result: *const QueryResult,
    index: usize,

    pub fn get(self: Row, col: usize) ?[]const u8 {
        return self.result.getValue(self.index, col);
    }

    /// Find column index by name. Returns null if column doesn't exist.
    pub fn columnIndex(self: Row, name: []const u8) ?usize {
        const num_cols = self.result.columnCount();
        for (0..num_cols) |col| {
            if (std.mem.eql(u8, self.result.columnName(col), name)) {
                return col;
            }
        }
        return null;
    }

    pub fn getByName(self: Row, name: []const u8) ?[]const u8 {
        const col = self.columnIndex(name) orelse return null;
        return self.get(col);
    }

    pub fn getInt(self: Row, col: usize) ?i64 {
        const value = self.get(col) orelse return null;
        return std.fmt.parseInt(i64, value, 10) catch null;
    }

    pub fn getFloat(self: Row, col: usize) ?f64 {
        const value = self.get(col) orelse return null;
        return std.fmt.parseFloat(f64, value) catch null;
    }

    pub fn getBool(self: Row, col: usize) ?bool {
        const value = self.get(col) orelse return null;
        if (value.len == 0) return null;
        return value[0] == 't';
    }
};

// PostgreSQL type OIDs
const BOOLOID: u32 = 16;
const INT2OID: u32 = 21;
const INT4OID: u32 = 23;
const INT8OID: u32 = 20;
const FLOAT4OID: u32 = 700;
const FLOAT8OID: u32 = 701;
const NUMERICOID: u32 = 1700;

fn isNumericType(oid: u32) bool {
    return oid == INT2OID or oid == INT4OID or oid == INT8OID or
        oid == FLOAT4OID or oid == FLOAT8OID or oid == NUMERICOID;
}

fn isBoolType(oid: u32) bool {
    return oid == BOOLOID;
}

fn escapeJsonString(buf: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, str: []const u8) !void {
    for (str) |ch| {
        switch (ch) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            '\r' => try buf.appendSlice(allocator, "\\r"),
            '\t' => try buf.appendSlice(allocator, "\\t"),
            0x00...0x08, 0x0B, 0x0C, 0x0E...0x1F => {
                // Other control characters (excluding \t=0x09, \n=0x0A, \r=0x0D)
                var hex_buf: [6]u8 = "\\u0000".*;
                const hex_chars = "0123456789abcdef";
                hex_buf[4] = hex_chars[ch >> 4];
                hex_buf[5] = hex_chars[ch & 0x0F];
                try buf.appendSlice(allocator, &hex_buf);
            },
            else => try buf.append(allocator, ch),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "pool config defaults" {
    const config = PoolConfig{
        .database = "test",
        .username = "postgres",
    };
    try std.testing.expectEqual(@as(u16, 5432), config.port);
    try std.testing.expectEqual(@as(u16, 10), config.pool_size);
    try std.testing.expectEqualStrings("localhost", config.host);
}

// Include tests from submodules
test {
    _ = @import("pg/connection.zig");
    _ = @import("pg/model.zig");
    _ = @import("pg/migrations.zig");
    _ = @import("pg/async_pool.zig");
}
