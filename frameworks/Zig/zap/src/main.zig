const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");
const middleware = @import("middleware.zig");

const Allocator = std.mem.Allocator;
const Pool = pg.Pool;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};
    const allocator = gpa.allocator();

    var zap_port: []u8 = undefined;
    var arg_string = try std.fmt.allocPrint(allocator, "{s}", .{"0"});
    defer allocator.free(arg_string);

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    while (args.next()) |arg| {
        arg_string = try std.fmt.allocPrint(allocator, "{s}", .{arg});
        zap_port = arg_string;
    }

    var port = try std.fmt.parseInt(u16, zap_port, 0);

    if (port == 0) {
        port = 3000;
    }

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    var prng = std.Random.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp())));

    middleware.SharedAllocator.init(allocator);

    // create the endpoints
    var dbEndpoint = endpoints.DbEndpoint.init();
    var plaintextEndpoint = endpoints.PlaintextEndpoint.init();
    var jsonEndpoint = endpoints.JsonEndpoint.init();
    var fortunesEndpoint = endpoints.FortunesEndpoint.init();

    // we wrap the endpoints with middleware handlers
    var jsonEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, endpoints.JsonEndpoint, middleware.Context).init(
        &jsonEndpoint,
        null,
        .{ .checkPath = true },
    );

    var plaintextEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, endpoints.PlaintextEndpoint, middleware.Context).init(
        &plaintextEndpoint,
        jsonEndpointHandler.getHandler(),
        .{ .checkPath = true },
    );

    var fortunesEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, endpoints.FortunesEndpoint, middleware.Context).init(
        &fortunesEndpoint,
        plaintextEndpointHandler.getHandler(),
        .{ .checkPath = true },
    );

    var dbEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, endpoints.DbEndpoint, middleware.Context).init(
        &dbEndpoint,
        fortunesEndpointHandler.getHandler(),
        .{ .checkPath = true },
    );

    var headerHandler = middleware.HeaderMiddleWare.init(dbEndpointHandler.getHandler());
    var prngHandler = middleware.RandomMiddleWare.init(headerHandler.getHandler(), &prng);
    var pgHandler = middleware.PgMiddleWare.init(prngHandler.getHandler(), pg_pool);

    var listener = try zap.Middleware.Listener(middleware.Context).init(
        .{
            .on_request = null,
            .port = port,
            .log = false,
            .max_clients = 100000,
        },
        pgHandler.getHandler(),
        middleware.SharedAllocator.getAllocator,
    );
    try listener.listen();

    const threads = 128;

    std.debug.print("Listening at 0.0.0.0:{d} on {d} threads\n", .{ port, threads });

    // start worker threads
    zap.start(.{
        .threads = threads,
        .workers = 1,
    });
}
