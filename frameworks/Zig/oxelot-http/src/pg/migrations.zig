// PostgreSQL Migrations for oxelot-http
//
// SQL-based migration system similar to sqlx/goose. Migrations are plain SQL
// files with up/down sections marked by comments.
//
// File format:
//   -- +migrate up
//   CREATE TABLE users (...);
//
//   -- +migrate down
//   DROP TABLE users;
//
// Naming convention: yyyymmddhhmmss_name.sql (e.g., 20251226153000_create_users.sql)
//
// Features:
// - Transaction-per-migration execution
// - PostgreSQL advisory locks for concurrency safety
// - Status tracking in _migrations table

const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;
const pg = @import("../pg.zig");

// C library directory functions
const c = @cImport({
    @cInclude("dirent.h");
});

// POSIX directory wrapper for Zig 0.16 compatibility
const PosixDir = struct {
    dir: *c.DIR,

    fn open(path: []const u8) !PosixDir {
        const path_z = try posix.toPosixPath(path);
        const dir = c.opendir(&path_z) orelse return error.FileNotFound;
        return .{ .dir = dir };
    }

    fn close(self: PosixDir) void {
        _ = c.closedir(self.dir);
    }

    fn next(self: *PosixDir) ?[]const u8 {
        while (true) {
            const entry = c.readdir(self.dir) orelse return null;
            const name = std.mem.span(@as([*:0]const u8, @ptrCast(&entry.*.d_name)));
            // Skip . and ..
            if (std.mem.eql(u8, name, ".") or std.mem.eql(u8, name, "..")) continue;
            return name;
        }
    }
};

/// Create a new migration file without requiring a database connection.
/// Returns the created filename (caller owns the memory).
///
/// Example:
///   const filename = try pg.migrations.createMigrationFile(allocator, "migrations", "add_users");
///   // Creates: migrations/20251226155300_add_users.sql
pub fn createMigrationFile(allocator: std.mem.Allocator, dir: []const u8, name: []const u8) ![]const u8 {
    // Get current timestamp for the filename
    const ts = std.posix.clock_gettime(.REALTIME) catch return error.TimestampFailed;
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts.sec) };
    const year_day = epoch.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch.getDaySeconds();

    const year = year_day.year;
    const month = month_day.month.numeric();
    const day = month_day.day_index + 1;
    const hour = day_seconds.getHoursIntoDay();
    const minute = day_seconds.getMinutesIntoHour();
    const second = day_seconds.getSecondsIntoMinute();

    // Create filename: yyyymmddhhmmss_name.sql
    const filename = try std.fmt.allocPrint(
        allocator,
        "{s}/{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}_{s}.sql",
        .{ dir, year, month, day, hour, minute, second, name },
    );
    errdefer allocator.free(filename);

    // Template content
    const template =
        \\-- +migrate up
        \\
        \\-- +migrate down
        \\
    ;

    // Create directory if it doesn't exist (using POSIX mkdir)
    const dir_z = try posix.toPosixPath(dir);
    posix.mkdirat(posix.AT.FDCWD, &dir_z, 0o755) catch |err| {
        if (err != error.PathAlreadyExists) {
            // Ignore if already exists, but log other errors
        }
    };

    // Write the file using POSIX APIs
    const filename_z = try posix.toPosixPath(filename);
    const fd = try posix.openat(posix.AT.FDCWD, &filename_z, .{
        .ACCMODE = .WRONLY,
        .CREAT = true,
        .TRUNC = true,
    }, 0o644);
    defer posix.close(fd);

    // Write template content
    var written: usize = 0;
    while (written < template.len) {
        written += try posix.write(fd, template[written..]);
    }

    return filename;
}

/// Generate a migration filename without creating the file.
/// Returns just the basename (e.g., "20251226155300_add_users.sql").
pub fn generateMigrationName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
    const ts = std.posix.clock_gettime(.REALTIME) catch return error.TimestampFailed;
    const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts.sec) };
    const year_day = epoch.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch.getDaySeconds();

    return std.fmt.allocPrint(
        allocator,
        "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}_{s}.sql",
        .{
            year_day.year,
            month_day.month.numeric(),
            month_day.day_index + 1,
            day_seconds.getHoursIntoDay(),
            day_seconds.getMinutesIntoHour(),
            day_seconds.getSecondsIntoMinute(),
            name,
        },
    );
}

pub const MigrationError = error{
    FileNotFound,
    ParseError,
    NoDownMigration,
    QueryFailed,
    LockFailed,
    InvalidFileName,
    OutOfMemory,
    NoAvailableConnections,
    ConnectionFailed,
};

pub const MigrationStatus = struct {
    name: []const u8,
    applied: bool,
    applied_at: ?i64,
};

const ParsedMigration = struct {
    name: []const u8,
    number: u32,
    up: []const u8,
    down: ?[]const u8,
};

/// Advisory lock ID for migrations (arbitrary constant)
const MIGRATION_LOCK_ID: i64 = 748392056;

pub const Migrations = struct {
    allocator: std.mem.Allocator,
    pool: *pg.Pool,
    dir: []const u8,

    /// Initialize migrations manager
    pub fn init(allocator: std.mem.Allocator, pool: *pg.Pool, dir: []const u8) Migrations {
        return .{
            .allocator = allocator,
            .pool = pool,
            .dir = dir,
        };
    }

    pub fn deinit(self: *Migrations) void {
        _ = self;
        // Nothing to clean up - we don't own the pool
    }

    /// Get the status of all migrations
    pub fn status(self: *Migrations) ![]MigrationStatus {
        // Ensure migrations table exists
        try self.ensureMigrationsTable();

        // Get list of migration files
        const files = try self.listMigrationFiles();
        defer {
            for (files) |f| {
                self.allocator.free(f.name);
            }
            self.allocator.free(files);
        }

        // Get applied migrations from database
        const applied = try self.getAppliedMigrations();
        defer {
            for (applied) |a| {
                self.allocator.free(a.name);
            }
            self.allocator.free(applied);
        }

        // Build status list
        var statuses: std.ArrayListUnmanaged(MigrationStatus) = .empty;
        errdefer {
            for (statuses.items) |s| {
                self.allocator.free(s.name);
            }
            statuses.deinit(self.allocator);
        }

        for (files) |file| {
            const name_copy = try self.allocator.dupe(u8, file.name);
            errdefer self.allocator.free(name_copy);

            var is_applied = false;
            var applied_at: ?i64 = null;

            for (applied) |a| {
                if (std.mem.eql(u8, a.name, file.name)) {
                    is_applied = true;
                    applied_at = a.applied_at;
                    break;
                }
            }

            try statuses.append(self.allocator, .{
                .name = name_copy,
                .applied = is_applied,
                .applied_at = applied_at,
            });
        }

        return statuses.toOwnedSlice(self.allocator);
    }

    /// Free status list returned by status()
    pub fn freeStatus(self: *Migrations, statuses: []MigrationStatus) void {
        for (statuses) |s| {
            self.allocator.free(s.name);
        }
        self.allocator.free(statuses);
    }

    /// Run all pending migrations
    /// Returns the number of migrations applied
    pub fn run(self: *Migrations) !usize {
        // Acquire advisory lock
        try self.acquireLock();
        defer self.releaseLock() catch {};

        // Ensure migrations table exists
        try self.ensureMigrationsTable();

        // Get list of migration files
        const files = try self.listMigrationFiles();
        defer {
            for (files) |f| {
                self.allocator.free(f.name);
            }
            self.allocator.free(files);
        }

        // Get applied migrations
        const applied = try self.getAppliedMigrations();
        defer {
            for (applied) |a| {
                self.allocator.free(a.name);
            }
            self.allocator.free(applied);
        }

        // Find pending migrations
        var applied_count: usize = 0;

        for (files) |file| {
            // Check if already applied
            var is_applied = false;
            for (applied) |a| {
                if (std.mem.eql(u8, a.name, file.name)) {
                    is_applied = true;
                    break;
                }
            }

            if (is_applied) continue;

            // Parse and run the migration
            const content = try self.readMigrationFile(file.name);
            defer self.allocator.free(content);

            const parsed = try parseMigrationContent(content);

            std.log.info("Applying migration: {s}", .{file.name});

            // Execute in transaction
            try self.executeInTransaction(parsed.up);

            // Record as applied
            try self.recordMigration(file.name);

            applied_count += 1;
            std.log.info("Applied migration: {s}", .{file.name});
        }

        return applied_count;
    }

    /// Rollback the last N migrations
    /// Returns the number of migrations rolled back
    pub fn rollback(self: *Migrations, count: usize) !usize {
        if (count == 0) return 0;

        // Acquire advisory lock
        try self.acquireLock();
        defer self.releaseLock() catch {};

        // Get applied migrations in reverse order
        const applied = try self.getAppliedMigrationsReverse();
        defer {
            for (applied) |a| {
                self.allocator.free(a.name);
            }
            self.allocator.free(applied);
        }

        var rolled_back: usize = 0;
        const to_rollback = @min(count, applied.len);

        for (applied[0..to_rollback]) |migration| {
            // Read and parse the migration file
            const content = self.readMigrationFile(migration.name) catch |err| {
                std.log.err("Failed to read migration file {s}: {}", .{ migration.name, err });
                return err;
            };
            defer self.allocator.free(content);

            const parsed = try parseMigrationContent(content);

            const down = parsed.down orelse {
                std.log.err("Migration {s} has no down section", .{migration.name});
                return MigrationError.NoDownMigration;
            };

            std.log.info("Rolling back migration: {s}", .{migration.name});

            // Execute down migration in transaction
            try self.executeInTransaction(down);

            // Remove from applied
            try self.unrecordMigration(migration.name);

            rolled_back += 1;
            std.log.info("Rolled back migration: {s}", .{migration.name});
        }

        return rolled_back;
    }

    /// Create a new migration file
    pub fn create(self: *Migrations, name: []const u8) !void {
        // Get current timestamp for the filename
        const ts = std.posix.clock_gettime(.REALTIME) catch return MigrationError.QueryFailed;
        const epoch = std.time.epoch.EpochSeconds{ .secs = @intCast(ts.sec) };
        const year_day = epoch.getEpochDay().calculateYearDay();
        const month_day = year_day.calculateMonthDay();
        const day_seconds = epoch.getDaySeconds();

        const year = year_day.year;
        const month = month_day.month.numeric();
        const day = month_day.day_index + 1;
        const hour = day_seconds.getHoursIntoDay();
        const minute = day_seconds.getMinutesIntoHour();
        const second = day_seconds.getSecondsIntoMinute();

        // Create filename: yyyymmddhhmmss_name.sql
        const filename = try std.fmt.allocPrint(
            self.allocator,
            "{s}/{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}_{s}.sql",
            .{ self.dir, year, month, day, hour, minute, second, name },
        );
        defer self.allocator.free(filename);

        // Template content
        const template =
            \\-- +migrate up
            \\
            \\-- +migrate down
            \\
        ;

        // Write the file using POSIX APIs
        const filename_z = try posix.toPosixPath(filename);
        const fd = try posix.openat(posix.AT.FDCWD, &filename_z, .{
            .ACCMODE = .WRONLY,
            .CREAT = true,
            .TRUNC = true,
        }, 0o644);
        defer posix.close(fd);

        var written: usize = 0;
        while (written < template.len) {
            written += try posix.write(fd, template[written..]);
        }

        std.log.info("Created migration: {s}", .{filename});
    }

    // ========================================================================
    // Private helpers
    // ========================================================================

    fn ensureMigrationsTable(self: *Migrations) !void {
        const sql =
            \\CREATE TABLE IF NOT EXISTS _migrations (
            \\    id SERIAL PRIMARY KEY,
            \\    name VARCHAR(255) NOT NULL UNIQUE,
            \\    applied_at TIMESTAMP DEFAULT now()
            \\)
        ;

        var result = self.pool.query(sql) catch |err| {
            std.log.err("Failed to create migrations table: {}", .{err});
            return MigrationError.QueryFailed;
        };
        result.deinit();
    }

    fn acquireLock(self: *Migrations) !void {
        const sql = try std.fmt.allocPrint(
            self.allocator,
            "SELECT pg_advisory_lock({d})",
            .{MIGRATION_LOCK_ID},
        );
        defer self.allocator.free(sql);

        var result = self.pool.query(sql) catch {
            return MigrationError.LockFailed;
        };
        result.deinit();
    }

    fn releaseLock(self: *Migrations) !void {
        const sql = try std.fmt.allocPrint(
            self.allocator,
            "SELECT pg_advisory_unlock({d})",
            .{MIGRATION_LOCK_ID},
        );
        defer self.allocator.free(sql);

        var result = self.pool.query(sql) catch {
            return MigrationError.LockFailed;
        };
        result.deinit();
    }

    const MigrationFile = struct {
        name: []const u8,
        timestamp: u64,
    };

    fn listMigrationFiles(self: *Migrations) ![]MigrationFile {
        var dir = PosixDir.open(self.dir) catch {
            return MigrationError.FileNotFound;
        };
        defer dir.close();

        var files: std.ArrayListUnmanaged(MigrationFile) = .empty;
        errdefer {
            for (files.items) |f| {
                self.allocator.free(f.name);
            }
            files.deinit(self.allocator);
        }

        while (dir.next()) |entry_name| {
            if (!std.mem.endsWith(u8, entry_name, ".sql")) continue;

            // Parse timestamp from filename (yyyymmddhhmmss_name.sql)
            const timestamp = parseTimestamp(entry_name) catch continue;

            const name_copy = try self.allocator.dupe(u8, entry_name);
            try files.append(self.allocator, .{
                .name = name_copy,
                .timestamp = timestamp,
            });
        }

        // Sort by timestamp
        std.mem.sort(MigrationFile, files.items, {}, struct {
            fn lessThan(_: void, a: MigrationFile, b: MigrationFile) bool {
                return a.timestamp < b.timestamp;
            }
        }.lessThan);

        return files.toOwnedSlice(self.allocator);
    }

    fn parseTimestamp(filename: []const u8) !u64 {
        // Find underscore - timestamp should be 14 digits (yyyymmddhhmmss)
        const underscore_pos = std.mem.indexOf(u8, filename, "_") orelse {
            return MigrationError.InvalidFileName;
        };

        if (underscore_pos != 14) {
            return MigrationError.InvalidFileName;
        }

        return std.fmt.parseInt(u64, filename[0..underscore_pos], 10) catch {
            return MigrationError.InvalidFileName;
        };
    }

    fn readMigrationFile(self: *Migrations, name: []const u8) ![]u8 {
        const path = try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ self.dir, name });
        defer self.allocator.free(path);

        const path_z = posix.toPosixPath(path) catch {
            return MigrationError.FileNotFound;
        };

        const fd = posix.openat(posix.AT.FDCWD, &path_z, .{ .ACCMODE = .RDONLY }, 0) catch {
            return MigrationError.FileNotFound;
        };
        defer posix.close(fd);

        // Get file size using lseek
        const SEEK_END = 2;
        const SEEK_SET = 0;
        const end_pos = linux.lseek(fd, 0, SEEK_END);
        if (@as(isize, @bitCast(end_pos)) < 0) {
            return MigrationError.FileNotFound;
        }
        _ = linux.lseek(fd, 0, SEEK_SET);
        const size: usize = @intCast(end_pos);

        if (size > 10 * 1024 * 1024) {
            return MigrationError.FileNotFound; // File too large
        }

        const buffer = self.allocator.alloc(u8, size) catch {
            return MigrationError.OutOfMemory;
        };
        errdefer self.allocator.free(buffer);

        // Read the file contents
        var total_read: usize = 0;
        while (total_read < size) {
            const bytes_read = posix.read(fd, buffer[total_read..]) catch {
                return MigrationError.FileNotFound;
            };
            if (bytes_read == 0) break; // EOF
            total_read += bytes_read;
        }

        if (total_read != size) {
            return MigrationError.FileNotFound;
        }

        return buffer;
    }

    const AppliedMigration = struct {
        name: []const u8,
        applied_at: ?i64,
    };

    fn getAppliedMigrations(self: *Migrations) ![]AppliedMigration {
        const sql = "SELECT name, EXTRACT(EPOCH FROM applied_at)::bigint FROM _migrations ORDER BY id ASC";

        var result = self.pool.query(sql) catch |err| {
            std.log.err("Failed to query applied migrations: {}", .{err});
            return MigrationError.QueryFailed;
        };
        defer result.deinit();

        var applied: std.ArrayListUnmanaged(AppliedMigration) = .empty;
        errdefer {
            for (applied.items) |a| {
                self.allocator.free(a.name);
            }
            applied.deinit(self.allocator);
        }

        var rows = result.rows();
        while (rows.next()) |row| {
            const name = row.get(0) orelse continue;
            const name_copy = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(name_copy);

            try applied.append(self.allocator, .{
                .name = name_copy,
                .applied_at = row.getInt(1),
            });
        }

        return applied.toOwnedSlice(self.allocator);
    }

    fn getAppliedMigrationsReverse(self: *Migrations) ![]AppliedMigration {
        const sql = "SELECT name, EXTRACT(EPOCH FROM applied_at)::bigint FROM _migrations ORDER BY id DESC";

        var result = self.pool.query(sql) catch |err| {
            std.log.err("Failed to query applied migrations: {}", .{err});
            return MigrationError.QueryFailed;
        };
        defer result.deinit();

        var applied: std.ArrayListUnmanaged(AppliedMigration) = .empty;
        errdefer {
            for (applied.items) |a| {
                self.allocator.free(a.name);
            }
            applied.deinit(self.allocator);
        }

        var rows = result.rows();
        while (rows.next()) |row| {
            const name = row.get(0) orelse continue;
            const name_copy = try self.allocator.dupe(u8, name);
            errdefer self.allocator.free(name_copy);

            try applied.append(self.allocator, .{
                .name = name_copy,
                .applied_at = row.getInt(1),
            });
        }

        return applied.toOwnedSlice(self.allocator);
    }

    fn executeInTransaction(self: *Migrations, sql: []const u8) !void {
        // Start transaction
        var begin_result = self.pool.query("BEGIN") catch {
            return MigrationError.QueryFailed;
        };
        begin_result.deinit();

        // Execute the migration SQL
        var result = self.pool.query(sql) catch |err| {
            // Rollback on error
            var rollback_result = self.pool.query("ROLLBACK") catch {
                std.log.err("Migration failed and rollback also failed: {}", .{err});
                return MigrationError.QueryFailed;
            };
            rollback_result.deinit();
            std.log.err("Migration failed: {}", .{err});
            return MigrationError.QueryFailed;
        };
        result.deinit();

        // Commit
        var commit_result = self.pool.query("COMMIT") catch {
            return MigrationError.QueryFailed;
        };
        commit_result.deinit();
    }

    fn recordMigration(self: *Migrations, name: []const u8) !void {
        const params = [_][]const u8{name};
        var result = self.pool.queryParams(
            "INSERT INTO _migrations (name) VALUES ($1)",
            &params,
        ) catch {
            return MigrationError.QueryFailed;
        };
        result.deinit();
    }

    fn unrecordMigration(self: *Migrations, name: []const u8) !void {
        const params = [_][]const u8{name};
        var result = self.pool.queryParams(
            "DELETE FROM _migrations WHERE name = $1",
            &params,
        ) catch {
            return MigrationError.QueryFailed;
        };
        result.deinit();
    }
};

/// Parse migration file content into up and down sections
fn parseMigrationContent(content: []const u8) !struct { up: []const u8, down: ?[]const u8 } {
    const up_marker = "-- +migrate up";
    const down_marker = "-- +migrate down";

    // Find up section
    const up_start = std.mem.indexOf(u8, content, up_marker) orelse {
        return MigrationError.ParseError;
    };
    const up_content_start = up_start + up_marker.len;

    // Find down section (optional)
    const down_start = std.mem.indexOf(u8, content, down_marker);

    const up_end = down_start orelse content.len;
    const up_sql = std.mem.trim(u8, content[up_content_start..up_end], " \t\r\n");

    if (up_sql.len == 0) {
        return MigrationError.ParseError;
    }

    var down_sql: ?[]const u8 = null;
    if (down_start) |ds| {
        const down_content_start = ds + down_marker.len;
        const trimmed = std.mem.trim(u8, content[down_content_start..], " \t\r\n");
        if (trimmed.len > 0) {
            down_sql = trimmed;
        }
    }

    return .{
        .up = up_sql,
        .down = down_sql,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "parse migration content - up only" {
    const content =
        \\-- +migrate up
        \\CREATE TABLE users (id SERIAL PRIMARY KEY);
    ;

    const result = try parseMigrationContent(content);
    try std.testing.expectEqualStrings("CREATE TABLE users (id SERIAL PRIMARY KEY);", result.up);
    try std.testing.expect(result.down == null);
}

test "parse migration content - up and down" {
    const content =
        \\-- +migrate up
        \\CREATE TABLE users (id SERIAL PRIMARY KEY);
        \\
        \\-- +migrate down
        \\DROP TABLE users;
    ;

    const result = try parseMigrationContent(content);
    try std.testing.expectEqualStrings("CREATE TABLE users (id SERIAL PRIMARY KEY);", result.up);
    try std.testing.expectEqualStrings("DROP TABLE users;", result.down.?);
}

test "parse migration content - multiline SQL" {
    const content =
        \\-- +migrate up
        \\CREATE TABLE users (
        \\    id SERIAL PRIMARY KEY,
        \\    email VARCHAR(255) NOT NULL
        \\);
        \\CREATE INDEX idx_users_email ON users(email);
        \\
        \\-- +migrate down
        \\DROP TABLE users;
    ;

    const result = try parseMigrationContent(content);
    try std.testing.expect(std.mem.indexOf(u8, result.up, "CREATE TABLE users") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.up, "CREATE INDEX") != null);
    try std.testing.expectEqualStrings("DROP TABLE users;", result.down.?);
}

test "parse migration content - missing up marker" {
    const content = "CREATE TABLE users (id SERIAL PRIMARY KEY);";
    const result = parseMigrationContent(content);
    try std.testing.expectError(MigrationError.ParseError, result);
}

test "parse timestamp" {
    try std.testing.expectEqual(@as(u64, 20251226153000), try Migrations.parseTimestamp("20251226153000_create_users.sql"));
    try std.testing.expectEqual(@as(u64, 20240101120000), try Migrations.parseTimestamp("20240101120000_add_posts.sql"));
    try std.testing.expectEqual(@as(u64, 20231215093045), try Migrations.parseTimestamp("20231215093045_something.sql"));
}

test "parse timestamp - invalid" {
    // No underscore
    try std.testing.expectError(MigrationError.InvalidFileName, Migrations.parseTimestamp("create_users.sql"));
    // Wrong length (not 14 digits)
    try std.testing.expectError(MigrationError.InvalidFileName, Migrations.parseTimestamp("0001_create_users.sql"));
    try std.testing.expectError(MigrationError.InvalidFileName, Migrations.parseTimestamp("202512261530_short.sql"));
    // Non-numeric
    try std.testing.expectError(MigrationError.InvalidFileName, Migrations.parseTimestamp("2025122615300a_bad.sql"));
}
