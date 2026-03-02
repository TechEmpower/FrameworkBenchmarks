// Per-thread dedicated PostgreSQL connection with prepared statements and pipeline mode.
//
// Designed for the threadlocal-per-worker-thread pattern used in high-performance
// benchmarks. Wraps a single PGconn (not a pool) to eliminate contention.
//
// Features:
// - Prepared statement support (PQprepare / PQexecPrepared)
// - Pipeline mode for batching queries (libpq 14+)
// - Zero-allocation param passing via C-native pointer format

const std = @import("std");
const c = @import("../pg.zig").c;

/// Connection configuration
pub const ConnConfig = struct {
    host: [:0]const u8 = "localhost",
    port: u16 = 5432,
    database: [:0]const u8,
    username: [:0]const u8,
    password: [:0]const u8 = "",
};

/// Query result wrapper (same interface as pg.QueryResult but without allocator)
pub const QueryResult = struct {
    result: *c.PGresult,

    pub fn deinit(self: *QueryResult) void {
        c.PQclear(self.result);
    }

    pub fn rowCount(self: *const QueryResult) usize {
        return @intCast(c.PQntuples(self.result));
    }

    pub fn columnCount(self: *const QueryResult) usize {
        return @intCast(c.PQnfields(self.result));
    }

    pub fn getValue(self: *const QueryResult, row: usize, col: usize) ?[]const u8 {
        if (c.PQgetisnull(self.result, @intCast(row), @intCast(col)) == 1) {
            return null;
        }
        const value = c.PQgetvalue(self.result, @intCast(row), @intCast(col));
        return std.mem.span(value);
    }

    pub fn getInt(self: *const QueryResult, row: usize, col: usize) ?i32 {
        const value = self.getValue(row, col) orelse return null;
        return std.fmt.parseInt(i32, value, 10) catch null;
    }
};

/// Per-thread dedicated PostgreSQL connection
pub const Connection = struct {
    conn: *c.PGconn,

    pub fn connect(config: ConnConfig) !Connection {
        var conn_buf: [512]u8 = undefined;
        const conn_str = std.fmt.bufPrintZ(&conn_buf, "host={s} port={d} dbname={s} user={s} password={s}", .{
            config.host,
            config.port,
            config.database,
            config.username,
            config.password,
        }) catch return error.ConnectionStringTooLong;

        const pg_conn = c.PQconnectdb(conn_str.ptr) orelse {
            return error.ConnectionFailed;
        };

        if (c.PQstatus(pg_conn) != c.CONNECTION_OK) {
            const err_msg = c.PQerrorMessage(pg_conn);
            std.log.err("PostgreSQL connection failed: {s}", .{std.mem.span(err_msg)});
            c.PQfinish(pg_conn);
            return error.ConnectionFailed;
        }

        return .{ .conn = pg_conn };
    }

    pub fn deinit(self: *Connection) void {
        c.PQfinish(self.conn);
    }

    // ========================================================================
    // Prepared statements
    // ========================================================================

    pub fn prepare(self: *Connection, name: [*:0]const u8, sql: [*:0]const u8, n_params: c_int) !void {
        const result = c.PQprepare(self.conn, name, sql, n_params, null) orelse {
            return error.PrepareFailed;
        };
        defer c.PQclear(result);

        if (c.PQresultStatus(result) != c.PGRES_COMMAND_OK) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.log.err("Prepare '{s}' failed: {s}", .{ std.mem.span(name), std.mem.span(err_msg) });
            return error.PrepareFailed;
        }
    }

    pub fn execPrepared(self: *Connection, name: [*:0]const u8, params: []const ?[*:0]const u8, n_params: c_int) !QueryResult {
        const result = c.PQexecPrepared(
            self.conn,
            name,
            n_params,
            @ptrCast(params.ptr),
            null,
            null,
            0,
        ) orelse {
            return error.QueryFailed;
        };

        const status = c.PQresultStatus(result);
        if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.log.err("execPrepared '{s}' failed: {s}", .{ std.mem.span(name), std.mem.span(err_msg) });
            c.PQclear(result);
            return error.QueryFailed;
        }

        return .{ .result = result };
    }

    // ========================================================================
    // Pipeline mode (libpq 14+)
    // ========================================================================

    pub fn enterPipelineMode(self: *Connection) !void {
        if (c.PQenterPipelineMode(self.conn) != 1) {
            return error.PipelineFailed;
        }
    }

    pub fn exitPipelineMode(self: *Connection) !void {
        if (c.PQexitPipelineMode(self.conn) != 1) {
            return error.PipelineFailed;
        }
    }

    pub fn pipelineSync(self: *Connection) !void {
        if (c.PQpipelineSync(self.conn) != 1) {
            return error.PipelineFailed;
        }
    }

    pub fn sendPrepared(self: *Connection, name: [*:0]const u8, params: []const ?[*:0]const u8, n_params: c_int) !void {
        if (c.PQsendQueryPrepared(
            self.conn,
            name,
            n_params,
            @ptrCast(params.ptr),
            null,
            null,
            0,
        ) != 1) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.log.err("sendPrepared '{s}' failed: {s}", .{ std.mem.span(name), std.mem.span(err_msg) });
            return error.QueryFailed;
        }
    }

    /// Get next result from pipeline. Returns null when no more results for current query.
    pub fn getResult(self: *Connection) !?QueryResult {
        const result = c.PQgetResult(self.conn) orelse {
            return null;
        };

        const status = c.PQresultStatus(result);
        if (status == c.PGRES_PIPELINE_SYNC) {
            c.PQclear(result);
            return null;
        }
        if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.log.err("getResult failed: {s}", .{std.mem.span(err_msg)});
            c.PQclear(result);
            return error.QueryFailed;
        }

        return .{ .result = result };
    }

    // ========================================================================
    // Simple queries (fallback)
    // ========================================================================

    pub fn query(self: *Connection, sql: [*:0]const u8) !QueryResult {
        const result = c.PQexec(self.conn, sql) orelse {
            return error.QueryFailed;
        };

        const status = c.PQresultStatus(result);
        if (status != c.PGRES_TUPLES_OK and status != c.PGRES_COMMAND_OK) {
            const err_msg = c.PQerrorMessage(self.conn);
            std.log.err("Query failed: {s}", .{std.mem.span(err_msg)});
            c.PQclear(result);
            return error.QueryFailed;
        }

        return .{ .result = result };
    }
};
