const std = @import("std");

const zzz = @import("zzz");
const http = zzz.HTTP;

const tardy = zzz.tardy;
const Tardy = tardy.Tardy(.auto);
const Runtime = tardy.Runtime;
const Socket = tardy.Socket;

const Server = http.Server;
const Router = http.Router;
const Context = http.Context;
const Route = http.Route;
const Respond = http.Respond;

const Message = struct { message: []const u8 };
var date: [29]u8 = undefined;

pub fn main() !void {
    const host: []const u8 = "0.0.0.0";
    const port: u16 = 8080;

    const date_thread = try std.Thread.spawn(.{}, struct {
        fn a() !void {
            while (true) {
                var d = http.Date.init(std.time.timestamp());
                const http_date = d.to_http_date();
                _ = try http_date.into_buf(date[0..]);
                std.time.sleep(std.time.ns_per_ms * 985);
            }
        }
    }.a, .{});

    date_thread.detach();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit() == .leak) {
        @panic("Memory leak has occurred!");
    };

    const allocator = gpa.allocator();

    var t = try Tardy.init(allocator, .{
        .threading = .all,
    });
    defer t.deinit();

    var router = try Router.init(allocator, &.{
        Route.init("/plaintext").get({}, home_handler).layer(),
        Route.init("/json").get({}, json_handler).layer(),
    }, .{});
    defer router.deinit(allocator);

    var socket = try Socket.init(.{ .tcp = .{ .host = host, .port = port } });
    defer socket.close_blocking();
    try socket.bind();
    try socket.listen(4096);

    const EntryParams = struct {
        router: *const Router,
        socket: Socket,
    };

    try t.entry(
        EntryParams{ .router = &router, .socket = socket },
        struct {
            fn entry(rt: *Runtime, p: EntryParams) !void {
                var server = Server.init(.{
                    .capture_count_max = 0,
                });
                try server.serve(rt, p.router, .{ .normal = p.socket });
            }
        }.entry,
    );
}

pub fn home_handler(ctx: *const Context, _: void) !Respond {
    try ctx.response.headers.put("Date", try ctx.allocator.dupe(u8, date[0..]));
    return ctx.response.apply(.{
        .mime = http.Mime.TEXT,
        .body = "Hello, World!",
        .status = .OK,
    });
}

pub fn json_handler(ctx: *const Context, _: void) !Respond {
    try ctx.response.headers.put("Date", try ctx.allocator.dupe(u8, date[0..]));
    return ctx.response.apply(.{
        .mime = http.Mime.JSON,
        .body = try std.json.stringifyAlloc(ctx.allocator, Message{ .message = "Hello, World!" }, .{}),
        .status = .OK,
    });
}
