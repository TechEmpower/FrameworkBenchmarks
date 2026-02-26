// Database migrations example for zig-http
//
// Demonstrates the SQL-based migration system including:
// - Running pending migrations
// - Checking migration status
// - Rolling back migrations
// - Creating new migration files (no DB connection required)
//
// Run with: zig build && ./zig-out/bin/example-db-migrations <command>
//
// Commands:
//   up            - Apply all pending migrations (requires DB)
//   down          - Rollback the last migration (requires DB)
//   status        - Show migration status (requires DB)
//   new <name>    - Create a new migration file (no DB required)
//
// Before running up/down/status, ensure PostgreSQL is running.

const std = @import("std");
const pg = @import("pg");

const MIGRATIONS_DIR = "examples/migrations";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command line arguments
    var args = std.process.args();
    _ = args.next(); // skip program name

    const cmd = args.next() orelse {
        printUsage();
        return;
    };

    // "new" command doesn't require database connection
    if (std.mem.eql(u8, cmd, "new")) {
        const name = args.next() orelse {
            std.log.err("Usage: db_migrations new <name>", .{});
            std.log.err("Example: db_migrations new add_user_wishlist", .{});
            return;
        };

        const filename = pg.createMigrationFile(allocator, MIGRATIONS_DIR, name) catch |err| {
            std.log.err("Failed to create migration: {}", .{err});
            return err;
        };
        defer allocator.free(filename);

        std.log.info("Created migration: {s}", .{filename});
        return;
    }

    // Other commands require database connection
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

    // Initialize migrations
    var migrations = pg.Migrations.init(allocator, &pool, MIGRATIONS_DIR);
    defer migrations.deinit();

    if (std.mem.eql(u8, cmd, "up")) {
        const applied = migrations.run() catch |err| {
            std.log.err("Migration failed: {}", .{err});
            return err;
        };
        if (applied == 0) {
            std.log.info("No pending migrations", .{});
        } else {
            std.log.info("Applied {d} migration(s)", .{applied});
        }
    } else if (std.mem.eql(u8, cmd, "down")) {
        const rolled = migrations.rollback(1) catch |err| {
            std.log.err("Rollback failed: {}", .{err});
            return err;
        };
        if (rolled == 0) {
            std.log.info("No migrations to rollback", .{});
        } else {
            std.log.info("Rolled back {d} migration(s)", .{rolled});
        }
    } else if (std.mem.eql(u8, cmd, "status")) {
        const status = migrations.status() catch |err| {
            std.log.err("Failed to get status: {}", .{err});
            return err;
        };
        defer migrations.freeStatus(status);

        if (status.len == 0) {
            std.log.info("No migrations found", .{});
        } else {
            std.log.info("Migration status:", .{});
            for (status) |m| {
                const state: []const u8 = if (m.applied) "applied" else "pending";
                std.log.info("  [{s}] {s}", .{ state, m.name });
            }
        }
    } else {
        printUsage();
    }
}

fn printUsage() void {
    std.log.info("Usage: db_migrations <command>", .{});
    std.log.info("", .{});
    std.log.info("Commands:", .{});
    std.log.info("  new <name>      Create a new migration file (no DB required)", .{});
    std.log.info("  up              Apply all pending migrations", .{});
    std.log.info("  down            Rollback the last migration", .{});
    std.log.info("  status          Show migration status", .{});
    std.log.info("", .{});
    std.log.info("Example:", .{});
    std.log.info("  db_migrations new add_user_wishlist", .{});
    std.log.info("  # Creates: examples/migrations/20251226160000_add_user_wishlist.sql", .{});
}
