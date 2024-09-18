const std = @import("std");
const regex = @import("regex");
const dns = @import("dig");
const pg = @import("pg");

const Allocator = std.mem.Allocator;
const Pool = pg.Pool;
const ArrayList = std.ArrayList;
const Regex = regex.Regex;

pub fn initPool(allocator: Allocator) !*pg.Pool {
    const info = try parsePostgresConnStr(allocator);
    std.debug.print("Connection: {s}:{s}@{s}:{d}/{s}\n", .{ info.username, info.password, info.hostname, info.port, info.database });

    const pg_pool = try Pool.init(allocator, .{
        .size = 28,
        .connect = .{
            .port = info.port,
            .host = info.hostname,
        },
        .auth = .{
            .username = info.username,
            .database = info.database,
            .password = info.password,
        },
        .timeout = 10_000,
    });

    return pg_pool;
}

pub const ConnectionInfo = struct {
    username: []const u8,
    password: []const u8,
    hostname: []const u8,
    port: u16,
    database: []const u8,
};

fn addressAsString(address: std.net.Address) ![]const u8 {
    const bytes = @as(*const [4]u8, @ptrCast(&address.in.sa.addr));

    var buffer: [256]u8 = undefined;
    var source = std.io.StreamSource{ .buffer = std.io.fixedBufferStream(&buffer) };
    var writer = source.writer();

    //try writer.writeAll("Hello, World!");

    try writer.print("{}.{}.{}.{}", .{
        bytes[0],
        bytes[1],
        bytes[2],
        bytes[3],
    });

    const output = source.buffer.getWritten();

    return output;
}

fn parsePostgresConnStr(allocator: Allocator) !ConnectionInfo {
    const pg_port = try getEnvVar(allocator, "PG_PORT", "5432");
    std.debug.print("tfb port {s}\n", .{pg_port});
    var port = try std.fmt.parseInt(u16, pg_port, 0);

    if (port == 0) {
        port = 5432;
    }

    return ConnectionInfo{
        .username = try getEnvVar(allocator, "PG_USER", "benchmarkdbuser"),
        .password = try getEnvVar(allocator, "PG_PASS", "benchmarkdbpass"),
        .hostname = try getEnvVar(allocator, "PG_HOST", "localhost"),
        .port = port,
        .database = try getEnvVar(allocator, "PG_DB", "hello_world"),
    };
}

fn getEnvVar(allocator: Allocator, name: []const u8, default: []const u8) ![]const u8 {
    const env_var = std.process.getEnvVarOwned(allocator, name) catch |err| switch (err) {
        error.EnvironmentVariableNotFound => return default,
        error.OutOfMemory => return err,
        error.InvalidWtf8 => return err,
    };

    if (env_var.len == 0) return default;

    return env_var;
}
