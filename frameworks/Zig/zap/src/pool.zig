const std = @import("std");
const pg = @import("pg");
const regex = @import("regex");
const dns = @import("dns");

const Allocator = std.mem.Allocator;
const Pool = pg.Pool;
const ArrayList = std.ArrayList;
const Regex = regex.Regex;

pub fn initPool(allocator: Allocator) !*pg.Pool {
    const info = try parsePostgresConnStr();
    std.debug.print("Cconnection info: {s}:{s}@{s}:{d}/{s}\n", .{ info.username, info.password, info.hostname, info.port, info.database });

    const hostname = std.os.getenv("PG_HOST") orelse "localhost";
    var addresses = try dns.helpers.getAddressList(hostname, allocator);
    defer addresses.deinit();

    var hostAddress = std.net.Address.parseIp("127.0.0.1", 0) catch unreachable;

    for (addresses.addrs) |address| {
        hostAddress = address;
    }

    std.debug.print("tfb hostname {}\n", .{hostAddress.in});

    const host = try addressAsString(hostAddress);

    var pg_pool = try Pool.init(allocator, .{ .size = 28, .connect = .{
        .port = info.port,
        .host = host,
    }, .auth = .{
        .username = info.username,
        .database = info.database,
        .password = info.password,
    }, .timeout = 10_000,});

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

fn parsePostgresConnStr() !ConnectionInfo {
    return ConnectionInfo{
        .username = std.os.getenv("PG_USER") orelse "benchmarkdbuser",
        .password = std.os.getenv("PG_PASS") orelse "benchmarkdbpass",
        .hostname = std.os.getenv("PG_HOST") orelse "localhost", // ,
        .port = try std.fmt.parseInt(u16, std.os.getenv("PG_PORT") orelse "5432", 0),
        .database = std.os.getenv("PG_DB") orelse "hello_world",
    };
}
