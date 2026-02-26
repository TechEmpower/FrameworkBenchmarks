// JSON API server example using zig-http
//
// Demonstrates:
// - JSON request/response handling
// - Path parameters
// - Query parameters
// - ObjectBuilder for dynamic JSON
//
// Run with: zig build && ./zig-out/bin/example-json-api
// Test with:
//   curl http://localhost:8080/api/users
//   curl http://localhost:8080/api/users/123
//   curl -X POST http://localhost:8080/api/users -d '{"name":"John","email":"john@example.com"}'

const std = @import("std");
const http = @import("http");

// In-memory user store (for demo purposes)
var users: std.StringHashMap(User) = undefined;
var next_id: u32 = 1;
var users_mutex: std.Thread.Mutex = .{};

const User = struct {
    id: u32,
    name: []const u8,
    email: []const u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize user store
    users = std.StringHashMap(User).init(allocator);
    defer users.deinit();

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // API routes
    _ = router
        .get("/", handleRoot)
        .get("/api/users", handleListUsers)
        .get("/api/users/:id", handleGetUser)
        .post("/api/users", handleCreateUser)
        .delete("/api/users/:id", handleDeleteUser);

    // Start server
    var server = http.Server.init(allocator, &router, .{
        .threads = 4,
    });
    defer server.deinit();

    std.log.info("Starting JSON API server on http://localhost:8080", .{});
    try server.run("0.0.0.0", 8080);
}

fn handleRoot(ctx: *http.Context) !void {
    try ctx.sendJson(.{
        .name = "zig-http JSON API",
        .version = "0.1.0",
        .endpoints = .{
            "/api/users",
            "/api/users/:id",
        },
    });
}

fn handleListUsers(ctx: *http.Context) !void {
    // Query parameter for filtering
    const filter = ctx.query("filter");
    _ = filter; // Would use for filtering

    // Build JSON array response
    var builder = http.ArrayBuilder.init(ctx.allocator);
    defer builder.deinit();

    users_mutex.lock();
    defer users_mutex.unlock();

    var iter = users.valueIterator();
    while (iter.next()) |user| {
        var obj = http.ObjectBuilder.init(ctx.allocator);
        defer obj.deinit();
        _ = try obj.int("id", user.id);
        _ = try obj.string("name", user.name);
        _ = try obj.string("email", user.email);
        const user_json = try obj.build();
        defer ctx.allocator.free(user_json);
        _ = try builder.pushRaw(user_json);
    }

    const result = try builder.build();
    defer ctx.allocator.free(result);

    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.send(result);
}

fn handleGetUser(ctx: *http.Context) !void {
    const id_str = ctx.param("id") orelse {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing user ID" });
        return;
    };

    users_mutex.lock();
    defer users_mutex.unlock();

    if (users.get(id_str)) |user| {
        try ctx.sendJson(.{
            .id = user.id,
            .name = user.name,
            .email = user.email,
        });
    } else {
        _ = ctx.notFound();
        try ctx.sendJson(.{ .@"error" = "User not found" });
    }
}

fn handleCreateUser(ctx: *http.Context) !void {
    const CreateUserRequest = struct {
        name: []const u8,
        email: []const u8,
    };

    const body = ctx.json(CreateUserRequest) catch {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Invalid JSON body" });
        return;
    };

    users_mutex.lock();
    defer users_mutex.unlock();

    const id = next_id;
    next_id += 1;

    const id_str = std.fmt.allocPrint(ctx.allocator, "{}", .{id}) catch {
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to create user" });
        return;
    };

    const user = User{
        .id = id,
        .name = body.name,
        .email = body.email,
    };

    users.put(id_str, user) catch {
        ctx.allocator.free(id_str);
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to create user" });
        return;
    };

    _ = ctx.response.created();
    try ctx.sendJson(.{
        .id = id,
        .name = body.name,
        .email = body.email,
    });
}

fn handleDeleteUser(ctx: *http.Context) !void {
    const id_str = ctx.param("id") orelse {
        _ = ctx.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing user ID" });
        return;
    };

    users_mutex.lock();
    defer users_mutex.unlock();

    if (users.remove(id_str)) {
        _ = ctx.response.noContent();
    } else {
        _ = ctx.notFound();
        try ctx.sendJson(.{ .@"error" = "User not found" });
    }
}
