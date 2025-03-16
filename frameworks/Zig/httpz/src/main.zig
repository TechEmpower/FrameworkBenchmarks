const std = @import("std");
const builtin = @import("builtin");
const httpz = @import("httpz");
const pg = @import("pg");
const datetimez = @import("datetimez");
const pool = @import("pool.zig");

const endpoints = @import("endpoints.zig");

var server: httpz.ServerCtx(*endpoints.Global, *endpoints.Global) = undefined;

pub fn main() !void {
    const cpu_count = try std.Thread.getCpuCount();
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = true,
    }){};

    const allocator = gpa.allocator();

    var pg_pool = try pool.initPool(allocator);
    defer pg_pool.deinit();

    const date_thread = try std.Thread.spawn(.{}, struct {
        fn update() void {
            const ally = std.heap.page_allocator;
            while (true) {
                const now = datetimez.datetime.Date.now();
                const time = datetimez.datetime.Time.now();

                // Wed, 17 Apr 2013 12:00:00 GMT
                // Return date in ISO format YYYY-MM-DD
                const TB_DATE_FMT = "{s:0>3}, {d:0>2} {s:0>3} {d:0>4} {d:0>2}:{d:0>2}:{d:0>2} GMT";
                endpoints.date_str = try std.fmt.allocPrint(ally, TB_DATE_FMT, .{ now.weekdayName()[0..3], now.day, now.monthName()[0..3], now.year, time.hour, time.minute, time.second });
                std.time.sleep(std.time.ns_per_ms * 980);
            }
        }
    }.update, .{});

    date_thread.detach();

    var prng = std.rand.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp())));

    var rand = prng.random();

    var global = endpoints.Global{
        .pool = pg_pool,
        .rand = &rand,
    };

    const args = try std.process.argsAlloc(allocator);

    const port: u16 = if (args.len > 1) try std.fmt.parseInt(u16, args[1], 0) else 3000;

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
        },
        .request = .{
            // Maximum number of headers to accept.
            // Additional headers will be silently ignored.
            .max_header_count = 32,

            // Maximum number of URL parameters to accept.
            // Additional parameters will be silently ignored.
            .max_param_count = 0,

            // Maximum number of query string parameters to accept.
            // Additional parameters will be silently ignored.
            .max_query_count = 0,

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
