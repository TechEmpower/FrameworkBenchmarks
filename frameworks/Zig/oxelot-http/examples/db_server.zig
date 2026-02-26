// Example: HTTP server with PostgreSQL database
//
// Demonstrates:
// - Connection pooling
// - Query execution with JSON responses
// - Parameterized queries for safety
//
// Prerequisites:
// - PostgreSQL server running (docker-compose from actor-runtime)
// - Database 'actor_runtime' with todo_lists and todo_items tables
//
// Run with: zig build db-server

const std = @import("std");
const http = @import("http");
const pg = @import("pg");

var db_pool: pg.Pool = undefined;

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // Initialize PostgreSQL connection pool
    // Using credentials from actor-runtime docker-compose
    db_pool = pg.Pool.init(allocator, .{
        .host = "localhost",
        .port = 5432,
        .database = "actor_runtime",
        .username = "actor_user",
        .password = "dev_password_change_in_production",
        .pool_size = 10,
    }) catch |err| {
        std.log.err("Failed to initialize database pool: {}", .{err});
        std.log.info("Make sure PostgreSQL is running: docker-compose up -d (in actor-runtime)", .{});
        return;
    };
    defer db_pool.deinit();

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    // Routes
    _ = router
        .get("/", handleRoot)
        .get("/lists", handleGetLists)
        .get("/lists/:id", handleGetList)
        .post("/lists", handleCreateList)
        .get("/lists/:id/items", handleGetItems)
        .post("/lists/:id/items", handleCreateItem)
        .put("/items/:id/toggle", handleToggleItem)
        .delete("/items/:id", handleDeleteItem);

    // Start server
    var server = http.Server.init(allocator, &router, .{
        .threads = 4,
    });
    defer server.deinit();

    std.log.info("Starting todo API server on http://localhost:8080", .{});
    std.log.info("Endpoints:", .{});
    std.log.info("  GET    /lists              - List all todo lists", .{});
    std.log.info("  POST   /lists              - Create list (JSON: {{\"title\": \"...\"}})", .{});
    std.log.info("  GET    /lists/:id          - Get list by ID", .{});
    std.log.info("  GET    /lists/:id/items    - Get items in a list", .{});
    std.log.info("  POST   /lists/:id/items    - Add item (JSON: {{\"description\": \"...\"}})", .{});
    std.log.info("  PUT    /items/:id/toggle   - Toggle item done status", .{});
    std.log.info("  DELETE /items/:id          - Delete an item", .{});
    try server.run("0.0.0.0", 8080);
}

fn handleRoot(ctx: *http.Context) !void {
    try ctx.sendJson(.{
        .message = "Todo API Server (zig-http + PostgreSQL)",
        .endpoints = .{
            "GET /lists",
            "POST /lists",
            "GET /lists/:id",
            "GET /lists/:id/items",
            "POST /lists/:id/items",
            "PUT /items/:id/toggle",
            "DELETE /items/:id",
        },
    });
}

fn handleGetLists(ctx: *http.Context) !void {
    const json = db_pool.queryJson(
        "SELECT id, title, created_at, updated_at FROM todo_lists ORDER BY created_at DESC",
    ) catch |err| {
        std.log.err("Query failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Database query failed" });
        return;
    };
    defer ctx.allocator.free(json);

    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleGetList(ctx: *http.Context) !void {
    const id = ctx.param("id") orelse {
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing list ID" });
        return;
    };

    const json = db_pool.queryParamsJson(
        "SELECT id, title, created_at, updated_at FROM todo_lists WHERE id = $1",
        &.{id},
    ) catch |err| {
        std.log.err("Query failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Database query failed" });
        return;
    };
    defer ctx.allocator.free(json);

    if (std.mem.eql(u8, json, "[]")) {
        _ = ctx.response.notFound();
        try ctx.sendJson(.{ .@"error" = "List not found" });
        return;
    }

    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleCreateList(ctx: *http.Context) !void {
    const ListInput = struct {
        title: []const u8 = "Untitled List",
        id: ?[]const u8 = null,
    };

    const input = ctx.request.json(ListInput) catch |err| {
        std.log.err("JSON parse failed: {}", .{err});
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Invalid JSON body" });
        return;
    };

    // Generate UUID if not provided
    var id_buf: [32]u8 = undefined;
    const list_id: []const u8 = if (input.id) |id| id else blk: {
        const uuid = std.crypto.random.int(u128);
        _ = std.fmt.bufPrint(&id_buf, "{x:0>32}", .{uuid}) catch unreachable;
        break :blk id_buf[0..32];
    };

    const json = db_pool.queryParamsJson(
        "INSERT INTO todo_lists (id, title) VALUES ($1, $2) RETURNING id, title, created_at, updated_at",
        &.{ list_id, input.title },
    ) catch |err| {
        std.log.err("Insert failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to create list" });
        return;
    };
    defer ctx.allocator.free(json);

    _ = ctx.response.created();
    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleGetItems(ctx: *http.Context) !void {
    const list_id = ctx.param("id") orelse {
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing list ID" });
        return;
    };

    const json = db_pool.queryParamsJson(
        "SELECT id, list_id, description, done, priority, created_at, updated_at FROM todo_items WHERE list_id = $1 ORDER BY priority DESC, created_at",
        &.{list_id},
    ) catch |err| {
        std.log.err("Query failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Database query failed" });
        return;
    };
    defer ctx.allocator.free(json);

    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleCreateItem(ctx: *http.Context) !void {
    const list_id = ctx.param("id") orelse {
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing list ID" });
        return;
    };

    const ItemInput = struct {
        description: []const u8,
        priority: i32 = 0,
    };

    const input = ctx.request.json(ItemInput) catch |err| {
        std.log.err("JSON parse failed: {}", .{err});
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Invalid JSON body. Required: {\"description\": \"...\"}" });
        return;
    };

    var priority_buf: [16]u8 = undefined;
    const priority_str = std.fmt.bufPrint(&priority_buf, "{d}", .{input.priority}) catch "0";

    const json = db_pool.queryParamsJson(
        "INSERT INTO todo_items (list_id, description, priority) VALUES ($1, $2, $3) RETURNING id, list_id, description, done, priority, created_at, updated_at",
        &.{ list_id, input.description, priority_str },
    ) catch |err| {
        std.log.err("Insert failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to create item (does the list exist?)" });
        return;
    };
    defer ctx.allocator.free(json);

    _ = ctx.response.created();
    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleToggleItem(ctx: *http.Context) !void {
    const item_id = ctx.param("id") orelse {
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing item ID" });
        return;
    };

    const json = db_pool.queryParamsJson(
        "UPDATE todo_items SET done = NOT done, updated_at = NOW() WHERE id = $1 RETURNING id, list_id, description, done, priority, created_at, updated_at",
        &.{item_id},
    ) catch |err| {
        std.log.err("Update failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to toggle item" });
        return;
    };
    defer ctx.allocator.free(json);

    if (std.mem.eql(u8, json, "[]")) {
        _ = ctx.response.notFound();
        try ctx.sendJson(.{ .@"error" = "Item not found" });
        return;
    }

    _ = ctx.response.contentType(http.ContentType.json);
    try ctx.response.body.appendSlice(ctx.allocator, json);
}

fn handleDeleteItem(ctx: *http.Context) !void {
    const item_id = ctx.param("id") orelse {
        _ = ctx.response.badRequest();
        try ctx.sendJson(.{ .@"error" = "Missing item ID" });
        return;
    };

    const json = db_pool.queryParamsJson(
        "DELETE FROM todo_items WHERE id = $1 RETURNING id",
        &.{item_id},
    ) catch |err| {
        std.log.err("Delete failed: {}", .{err});
        _ = ctx.response.internalServerError();
        try ctx.sendJson(.{ .@"error" = "Failed to delete item" });
        return;
    };
    defer ctx.allocator.free(json);

    if (std.mem.eql(u8, json, "[]")) {
        _ = ctx.response.notFound();
        try ctx.sendJson(.{ .@"error" = "Item not found" });
        return;
    }

    _ = ctx.response.noContent();
}
