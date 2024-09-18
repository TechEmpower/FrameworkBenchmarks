const std = @import("std");
const zap = @import("zap");
const pg = @import("pg");
const regex = @import("regex");
const dns = @import("dns");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");
const middleware = @import("middleware.zig");

const RndGen = std.rand.DefaultPrng;
const Allocator = std.mem.Allocator;
const Pool = pg.Pool;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};

    var tsa = std.heap.ThreadSafeAllocator{
        .child_allocator = gpa.allocator(),
    };

    const allocator = tsa.allocator();

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    var prng = std.rand.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp())));

    middleware.SharedAllocator.init(allocator);

    // create the endpoint
    var dbEndpoint = endpoints.DbEndpoint.init();
    var plaintextEndpoint = endpoints.PlaintextEndpoint.init();
    var jsonEndpoint = endpoints.JsonEndpoint.init();
    var fortunesEndpoint = endpoints.FortunesEndpoint.init();

    // we wrap the endpoint with a middleware handler
    var jsonEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, middleware.Context).init(
        jsonEndpoint.endpoint(), // the endpoint
        null, // no other handler (we are the last in the chain)
        false, // break on finish. See EndpointHandler for this. Not applicable here.
    );

    var plaintextEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, middleware.Context).init(
        plaintextEndpoint.endpoint(),
        jsonEndpointHandler.getHandler(),
        false,
    );

    var fortunesEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, middleware.Context).init(
        fortunesEndpoint.endpoint(), // the endpoint
        plaintextEndpointHandler.getHandler(), // no other handler (we are the last in the chain)
        false,
    );

    var dbEndpointHandler = zap.Middleware.EndpointHandler(middleware.Handler, middleware.Context).init(
        dbEndpoint.endpoint(), // the endpoint
        fortunesEndpointHandler.getHandler(), // no other handler (we are the last in the chain)
        false,
    );

    var headerHandler = middleware.HeaderMiddleWare.init(dbEndpointHandler.getHandler());
    var prngHandler = middleware.RandomMiddleWare.init(headerHandler.getHandler(), &prng);
    var pgHandler = middleware.PgMiddleWare.init(prngHandler.getHandler(), pg_pool);

    var listener = try zap.Middleware.Listener(middleware.Context).init(
        .{
            .on_request = null, // must be null
            .port = 3000,
            .log = false,
            .max_clients = 100000,
        },
        pgHandler.getHandler(),

        middleware.SharedAllocator.getAllocator,
    );
    try listener.listen();

    const cpuCount = @as(i16, @intCast(std.Thread.getCpuCount() catch 1));

    std.debug.print("Listening on 0.0.0.0:3000 on {d} threads\n", .{cpuCount});

    // start worker threads
    zap.start(.{
        .threads = 16 * cpuCount,
        .workers = 1,
    });
}
