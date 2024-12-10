const std = @import("std");
const builtin = @import("builtin");
const httpz = @import("httpz");
const pg = @import("pg");
const datetimez = @import("datetimez");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");

const RndGen = std.rand.DefaultPrng;
const Allocator = std.mem.Allocator;
const Pool = pg.Pool;

var server: httpz.ServerCtx(*endpoints.Global, *endpoints.Global) = undefined;

pub fn main() !void {
    const cpu_count = try std.Thread.getCpuCount();
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};

    const allocator = gpa.allocator();

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    var prng = std.rand.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp())));

    var global = endpoints.Global{
        .pool = pg_pool,
        .prng = &prng,
    };

    var httpz_port: []u8 = undefined;
    var arg_string = try std.fmt.allocPrint(allocator, "{s}", .{"0"});
    defer allocator.free(arg_string);

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    while (args.next()) |arg| {
        arg_string = try std.fmt.allocPrint(allocator, "{s}", .{arg});

       httpz_port = arg_string; // use arg
    }

    var port = try std.fmt.parseInt(u16,httpz_port, 0);

    if (port == 0) {
        port = 3000;
    }

    const workers = @as(u16, @intCast(16 * cpu_count));

    server = try httpz.ServerApp(*endpoints.Global).init(allocator, .{
        .port = port,
        .address = "0.0.0.0",
        .workers = .{
            // Number of worker threads
            // (blocking mode: handled differently)
            .count = workers,

            // Maximum number of concurrent connection each worker can handle
            // (blocking mode: currently ignored)
            .max_conn = 8_192,

            // Minimum number of connection states each worker should maintain
            // (blocking mode: currently ignored)
            .min_conn = 64,

            // A pool of larger buffers that can be used for any data larger than configured
            // static buffers. For example, if response headers don't fit in in
            // $response.header_buffer_size, a buffer will be pulled from here.
            // This is per-worker.
            .large_buffer_count = 16,

            // The size of each large buffer.
            .large_buffer_size = 65536,

            // Size of bytes retained for the connection arena between use. This will
            // result in up to `count * min_conn * retain_allocated_bytes` of memory usage.
            .retain_allocated_bytes = 4096,
        },

        // configures the threadpool which processes requests. The threadpool is
        // where your application code runs.
        .thread_pool = .{
            // Number threads. If you're handlers are doing a lot of i/o, a higher
            // number might provide better throughput
            // (blocking mode: handled differently)
            .count = 256,

            // The maximum number of pending requests that the thread pool will accept
            // This applies back pressure to the above workers and ensures that, under load
            // pending requests get precedence over processing new requests.
            .backlog = 2048,

            // Size of the static buffer to give each thread. Memory usage will be
            // `count * buffer_size`. If you're making heavy use of either `req.arena` or
            // `res.arena`, this is likely the single easiest way to gain performance.
            .buffer_size = 8192,
        },
        .request = .{
            // Maximum request body size that we'll process. We can allocate up 
            // to this much memory per request for the body. Internally, we might
            // keep this memory around for a number of requests as an optimization.
            .max_body_size = 1_048_576,

            // This memory is allocated upfront. The request header _must_ fit into
            // this space, else the request will be rejected.
            .buffer_size = 4_096,

            // Maximum number of headers to accept. 
            // Additional headers will be silently ignored.
            .max_header_count = 32,

            // Maximum number of URL parameters to accept.
            // Additional parameters will be silently ignored.
            .max_param_count = 10,

            // Maximum number of query string parameters to accept.
            // Additional parameters will be silently ignored.
            .max_query_count = 32,

            // Maximum number of x-www-form-urlencoded fields to support.
            // Additional parameters will be silently ignored. This must be
            // set to a value greater than 0 (the default) if you're going
            // to use the req.formData() method.
            .max_form_count = 0,

            // Maximum number of multipart/form-data fields to support.
            // Additional parameters will be silently ignored. This must be
            // set to a value greater than 0 (the default) if you're going
            // to use the req.multiFormData() method.
            .max_multiform_count = 0,
    },
    }, &global);
    defer server.deinit();

    // now that our server is up, we register our intent to handle SIGINT
    try std.posix.sigaction(std.posix.SIG.INT, &.{
        .handler = .{ .handler = shutdown },
        .mask = std.posix.empty_sigset,
        .flags = 0,
    }, null);

    var router = server.router();
    router.get("/json", endpoints.json);
    router.get("/plaintext", endpoints.plaintext);
    router.get("/db", endpoints.db);
    router.get("/fortunes", endpoints.fortune);

    std.debug.print("Httpz using {d} workers listening at 0.0.0.0:{d}\n", .{ workers, port });

    try server.listen();
}

fn shutdown(_: c_int) callconv(.C) void {
    server.stop();
}

fn notFound(_: *httpz.Request, res: *httpz.Response) !void {
    res.status = 404;
    res.body = "Not Found";
}

fn errorHandler(req: *httpz.Request, res: *httpz.Response, err: anyerror) void {
    res.status = 500;
    res.body = "Internal Server Error";
    std.log.warn("httpz: unhandled exception for request: {s}\nErr: {}", .{ req.url.raw, err });
}
