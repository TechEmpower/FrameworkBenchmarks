const std = @import("std");
const builtin = @import("builtin");
const zap = @import("zap");
const pg = @import("pg");
const regex = @import("regex");
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

    var zap_port: []u8 = undefined;
    var arg_string = try std.fmt.allocPrint(allocator, "{s}", .{"0"});
    defer allocator.free(arg_string);

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    while (args.next()) |arg| {
        arg_string = try std.fmt.allocPrint(allocator, "{s}", .{arg});

        zap_port = arg_string; // use arg
    }

    var port = try std.fmt.parseInt(u16, zap_port, 0);

    if (port == 0) {
        port = 3000;
    }

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
            .port = port,
            .log = false,
            .max_clients = 100000,
        },
        pgHandler.getHandler(),

        middleware.SharedAllocator.getAllocator,
    );
    try listener.listen();

    //const cpuCount = @as(i16, @intCast(std.Thread.getCpuCount() catch 1));
    //const workers = if (builtin.mode == .Debug) 1 else cpuCount;
    const threads = 128;

    std.debug.print("Listening at 0.0.0.0:{d} on {d} threads\n", .{port, threads});

    // start worker threads
    zap.start(.{
        .threads = threads,
        .workers = 1,
    });
}
