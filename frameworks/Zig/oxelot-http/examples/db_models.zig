// Struct mapping example for zig-http PostgreSQL
//
// Demonstrates zero-copy struct mapping:
// - Parsing query results into Zig structs
// - Comptime SQL generation for INSERT
// - Iterator-based row processing
//
// Run with: zig build && ./zig-out/bin/example-db-models
//
// Before running, ensure PostgreSQL is running and create the database:
//   createdb zig_http_example
//   psql zig_http_example -c "CREATE TABLE users (id SERIAL PRIMARY KEY, email TEXT NOT NULL, name TEXT, active BOOLEAN DEFAULT true)"

const std = @import("std");
const pg = @import("pg");

// Define a struct that matches the database schema
// Field names must match column names exactly
const User = struct {
    id: i64,
    email: []const u8, // zero-copy: points into PGresult memory
    name: ?[]const u8, // optional for nullable columns
    active: bool,
};

// Struct for inserting (without auto-generated fields)
const NewUser = struct {
    email: []const u8,
    name: ?[]const u8,
    active: bool,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Connect to database
    var pool = pg.Pool.init(allocator, .{
        .database = "zig_http_example",
        .username = "postgres",
        .password = "postgres",
        .pool_size = 2,
    }) catch |err| {
        std.log.err("Failed to connect to PostgreSQL: {}", .{err});
        std.log.err("Make sure PostgreSQL is running and the database exists:", .{});
        std.log.err("  createdb zig_http_example", .{});
        return err;
    };
    defer pool.deinit();

    // Demonstrate comptime SQL generation
    std.log.info("=== Comptime SQL Generation ===", .{});
    const insert_sql = pg.insertSql("users", NewUser);
    std.log.info("Generated INSERT: {s}", .{insert_sql});

    const update_sql = pg.updateSql("users", NewUser, "WHERE id = $4");
    std.log.info("Generated UPDATE: {s}", .{update_sql});

    // Insert a new user using struct
    std.log.info("\n=== Inserting User ===", .{});
    const new_user = NewUser{
        .email = "demo@example.com",
        .name = "Demo User",
        .active = true,
    };

    pool.insert("users", new_user) catch |err| {
        std.log.warn("Insert failed (user may already exist): {}", .{err});
    };

    // Query and parse into structs
    std.log.info("\n=== Query with Struct Parsing ===", .{});
    var result = try pool.query("SELECT id, email, name, active FROM users ORDER BY id DESC LIMIT 5");
    defer result.deinit();

    std.log.info("Found {d} users", .{result.rowCount()});

    // Method 1: Parse single row
    if (result.rowCount() > 0) {
        const first_user = try result.parse(User);
        std.log.info("First user: {s} ({s})", .{
            first_user.email,
            first_user.name orelse "no name",
        });
    }

    // Method 2: Parse all rows into slice
    const users = try result.parseAll(User, allocator);
    defer allocator.free(users); // only frees slice, not strings (they're in PGresult)

    std.log.info("\nAll users:", .{});
    for (users) |user| {
        const status = if (user.active) "active" else "inactive";
        std.log.info("  [{d}] {s} - {s} ({s})", .{
            user.id,
            user.email,
            user.name orelse "(no name)",
            status,
        });
    }

    // Method 3: Iterator-based (most memory efficient)
    std.log.info("\n=== Iterator-based Processing ===", .{});
    var result2 = try pool.query("SELECT id, email, name, active FROM users WHERE active = true");
    defer result2.deinit();

    var iter = result2.iter(User);
    var count: usize = 0;
    while (iter.next()) |user| {
        count += 1;
        std.log.info("Active user: {s}", .{user.email});
    }
    std.log.info("Processed {d} active users", .{count});

    // Demonstrate type safety with parsing
    std.log.info("\n=== Type-Safe Value Parsing ===", .{});
    const parsed_int = try pg.parseValue(i32, "42");
    const parsed_float = try pg.parseValue(f64, "3.14159");
    const parsed_bool = try pg.parseValue(bool, "t");
    const parsed_optional: ?i32 = try pg.parseValue(?i32, null);

    std.log.info("Parsed int: {d}", .{parsed_int});
    std.log.info("Parsed float: {d:.5}", .{parsed_float});
    std.log.info("Parsed bool: {}", .{parsed_bool});
    std.log.info("Parsed optional (null): {?}", .{parsed_optional});

    std.log.info("\n=== Done ===", .{});
}
