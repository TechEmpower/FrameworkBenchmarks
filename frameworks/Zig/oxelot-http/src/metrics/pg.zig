// PostgreSQL Metrics Instrumentation
//
// Wraps pg.Pool to add metrics collection for:
// - Query count by operation type (select, insert, update, delete)
// - Query duration histogram
// - Connection pool stats (total, in_use)

const std = @import("std");
const reg = @import("registry.zig");

const Registry = reg.Registry;

/// Instrumented PostgreSQL pool wrapper
/// Wraps a pg.Pool to add metrics collection
pub fn InstrumentedPool(comptime Pool: type) type {
    return struct {
        const Self = @This();

        pool: *Pool,
        registry: *Registry,

        /// Create an instrumented pool wrapper
        pub fn init(pool: *Pool, registry: *Registry) Self {
            return .{
                .pool = pool,
                .registry = registry,
            };
        }

        /// Execute a query with metrics
        pub fn query(self: *Self, sql: []const u8) @TypeOf(self.pool.query(sql)) {
            const op = detectOperation(sql);
            const start = std.time.Instant.now() catch null;

            const result = self.pool.query(sql) catch |err| {
                self.recordMetrics(op, start);
                return err;
            };

            self.recordMetrics(op, start);
            return result;
        }

        /// Execute a parameterized query with metrics
        pub fn queryParams(self: *Self, sql: []const u8, params: []const []const u8) @TypeOf(self.pool.queryParams(sql, params)) {
            const op = detectOperation(sql);
            const start = std.time.Instant.now() catch null;

            const result = self.pool.queryParams(sql, params) catch |err| {
                self.recordMetrics(op, start);
                return err;
            };

            self.recordMetrics(op, start);
            return result;
        }

        /// Execute a parameterized query with nullable parameters
        pub fn queryParamsNullable(self: *Self, sql: []const u8, params: []const ?[]const u8) @TypeOf(self.pool.queryParamsNullable(sql, params)) {
            const op = detectOperation(sql);
            const start = std.time.Instant.now() catch null;

            const result = self.pool.queryParamsNullable(sql, params) catch |err| {
                self.recordMetrics(op, start);
                return err;
            };

            self.recordMetrics(op, start);
            return result;
        }

        /// Insert a struct with metrics
        pub fn insert(self: *Self, comptime table: []const u8, value: anytype) !void {
            const start = std.time.Instant.now() catch null;

            try self.pool.insert(table, value);

            self.recordMetrics("insert", start);
        }

        /// Insert with RETURNING clause
        pub fn insertReturning(
            self: *Self,
            comptime table: []const u8,
            value: anytype,
            comptime ReturnT: type,
            comptime returning: []const u8,
        ) !ReturnT {
            const start = std.time.Instant.now() catch null;

            const result = try self.pool.insertReturning(table, value, ReturnT, returning);

            self.recordMetrics("insert", start);
            return result;
        }

        /// Collect and update pool statistics
        pub fn collectPoolStats(self: *Self) void {
            var total: i64 = 0;
            var in_use: i64 = 0;

            for (self.pool.connections) |*conn| {
                total += 1;
                if (conn.in_use.load(.acquire)) {
                    in_use += 1;
                }
            }

            self.registry.pg_pool_connections_total.set(total);
            self.registry.pg_pool_connections_in_use.set(in_use);
        }

        /// Record metrics for a completed operation
        fn recordMetrics(self: *Self, op: []const u8, start: ?std.time.Instant) void {
            self.registry.pg_query_total.inc(.{op});

            if (start) |s| {
                if (std.time.Instant.now()) |end| {
                    const elapsed_ns = end.since(s);
                    self.registry.pg_query_duration.observe(.{op}, elapsed_ns);
                } else |_| {}
            }
        }

        /// Deinitialize the underlying pool
        pub fn deinit(self: *Self) void {
            self.pool.deinit();
        }
    };
}

/// Detect the SQL operation type from the query string
pub fn detectOperation(sql: []const u8) []const u8 {
    // Skip leading whitespace
    var trimmed = sql;
    while (trimmed.len > 0 and (trimmed[0] == ' ' or trimmed[0] == '\t' or trimmed[0] == '\n' or trimmed[0] == '\r')) {
        trimmed = trimmed[1..];
    }

    if (trimmed.len < 5) return "other";

    // Check for 5-character commands first
    if (startsWithIgnoreCase(trimmed, "BEGIN")) return "transaction";

    if (trimmed.len < 6) return "other";

    // Check first 6 characters (case-insensitive)
    if (startsWithIgnoreCase(trimmed, "SELECT")) return "select";
    if (startsWithIgnoreCase(trimmed, "INSERT")) return "insert";
    if (startsWithIgnoreCase(trimmed, "UPDATE")) return "update";
    if (startsWithIgnoreCase(trimmed, "DELETE")) return "delete";
    if (startsWithIgnoreCase(trimmed, "CREATE")) return "ddl";
    if (startsWithIgnoreCase(trimmed, "ALTER ")) return "ddl";
    if (startsWithIgnoreCase(trimmed, "DROP T")) return "ddl";
    if (startsWithIgnoreCase(trimmed, "COMMIT")) return "transaction";
    if (startsWithIgnoreCase(trimmed, "ROLLBA")) return "transaction";

    return "other";
}

fn startsWithIgnoreCase(haystack: []const u8, needle: []const u8) bool {
    if (haystack.len < needle.len) return false;
    for (haystack[0..needle.len], needle) |h, n| {
        const h_lower = if (h >= 'A' and h <= 'Z') h + 32 else h;
        const n_lower = if (n >= 'A' and n <= 'Z') n + 32 else n;
        if (h_lower != n_lower) return false;
    }
    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "detect operation" {
    try std.testing.expectEqualStrings("select", detectOperation("SELECT * FROM users"));
    try std.testing.expectEqualStrings("select", detectOperation("select * from users"));
    try std.testing.expectEqualStrings("select", detectOperation("  SELECT * FROM users"));
    try std.testing.expectEqualStrings("insert", detectOperation("INSERT INTO users VALUES (1)"));
    try std.testing.expectEqualStrings("update", detectOperation("UPDATE users SET name = 'foo'"));
    try std.testing.expectEqualStrings("delete", detectOperation("DELETE FROM users WHERE id = 1"));
    try std.testing.expectEqualStrings("ddl", detectOperation("CREATE TABLE foo (id INT)"));
    try std.testing.expectEqualStrings("transaction", detectOperation("BEGIN"));
    try std.testing.expectEqualStrings("transaction", detectOperation("COMMIT"));
    try std.testing.expectEqualStrings("transaction", detectOperation("ROLLBACK"));
    try std.testing.expectEqualStrings("other", detectOperation("EXPLAIN SELECT 1"));
}

test "starts with ignore case" {
    try std.testing.expect(startsWithIgnoreCase("SELECT", "SELECT"));
    try std.testing.expect(startsWithIgnoreCase("select", "SELECT"));
    try std.testing.expect(startsWithIgnoreCase("SELECT", "select"));
    try std.testing.expect(!startsWithIgnoreCase("INSERT", "SELECT"));
}
