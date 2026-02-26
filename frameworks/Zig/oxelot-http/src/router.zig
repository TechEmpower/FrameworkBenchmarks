const std = @import("std");
const Request = @import("request.zig").Request;
const Response = @import("response.zig").Response;
const Method = @import("method.zig").Method;
const simd_json = @import("simd_json.zig");
const ContentType = @import("headers.zig").ContentType;
const websocket = @import("websocket.zig");
const mw = @import("middleware.zig");

// Re-export middleware types
// Note: mw.Context is opaque, but our Context is compatible via pointer casting
pub const Middleware = mw.Middleware;
pub const Next = mw.Next;
pub const MiddlewareChain = mw.MiddlewareChain;

/// Route handler context
pub const Context = struct {
    request: *Request,
    response: *Response,
    allocator: std.mem.Allocator,

    /// Get path parameter
    pub fn param(self: *const Context, name: []const u8) ?[]const u8 {
        return self.request.param(name);
    }

    /// Get path parameter as typed value
    pub fn paramAs(self: *const Context, comptime T: type, name: []const u8) ?T {
        return self.request.paramAs(T, name);
    }

    /// Get query parameter
    pub fn query(self: *const Context, name: []const u8) ?[]const u8 {
        return self.request.query(name);
    }

    /// Get header
    pub fn header(self: *const Context, name: []const u8) ?[]const u8 {
        return self.request.header(name);
    }

    /// Parse JSON body
    pub fn json(self: *const Context, comptime T: type) !T {
        return self.request.json(T);
    }

    // Response shortcuts
    pub fn ok(self: *Context) *Response {
        return self.response.ok();
    }

    pub fn notFound(self: *Context) *Response {
        return self.response.notFound();
    }

    pub fn badRequest(self: *Context) *Response {
        return self.response.badRequest();
    }

    pub fn sendJson(self: *Context, value: anytype) !void {
        try self.response.json(value);
    }

    /// Send JSON response using SIMD-accelerated serializer (faster)
    /// Uses a stack buffer for small responses, falls back to allocation for larger ones
    pub fn sendJsonFast(self: *Context, value: anytype) !void {
        _ = self.response.contentType(ContentType.json);

        // Try stack buffer first (covers most API responses)
        var stack_buf: [4096]u8 = undefined;
        var writer = simd_json.JsonWriter.init(&stack_buf);

        if (writer.writeValue(value)) {
            try self.response.body.appendSlice(self.allocator, writer.getWritten());
        } else |_| {
            // Fall back to allocation for larger responses
            const json_str = try simd_json.stringifyAlloc(self.allocator, value);
            defer self.allocator.free(json_str);
            try self.response.body.appendSlice(self.allocator, json_str);
        }
    }

    pub fn sendText(self: *Context, content: []const u8) !void {
        try self.response.text(content);
    }

    pub fn sendHtml(self: *Context, content: []const u8) !void {
        try self.response.html(content);
    }
};

/// Handler function signature
pub const Handler = *const fn (*Context) anyerror!void;

/// WebSocket handler callbacks
pub const WebSocketHandler = struct {
    /// Called when connection is opened (after upgrade handshake)
    onOpen: ?*const fn (*websocket.Connection) void = null,
    /// Called when a message (text or binary) is received
    onMessage: ?*const fn (*websocket.Connection, []const u8, websocket.Opcode) void = null,
    /// Called when connection is closed
    onClose: ?*const fn (*websocket.Connection, ?websocket.CloseCode, ?[]const u8) void = null,
    /// Called on error
    onError: ?*const fn (*websocket.Connection, anyerror) void = null,
};

/// Route segment type
const Segment = union(enum) {
    literal: []const u8,
    param: []const u8, // :name
    wildcard: []const u8, // *name
};

/// Route definition
const Route = struct {
    method: Method,
    pattern: []const u8,
    segments: []const Segment,
    handler: Handler,
    middleware: ?[]const Middleware = null, // Per-route middleware chain
};

/// WebSocket route definition
const WebSocketRoute = struct {
    pattern: []const u8,
    segments: []const Segment,
    handler: WebSocketHandler,
};

/// Builder for per-route middleware chains
pub const RouteBuilder = struct {
    router: *Router,
    pending_middleware: std.ArrayListUnmanaged(Middleware),

    /// Add middleware to the pending chain
    pub fn with(self: *RouteBuilder, middleware: Middleware) *RouteBuilder {
        self.pending_middleware.append(self.router.allocator, middleware) catch {};
        return self;
    }

    /// Register a GET route with pending middleware
    pub fn get(self: *RouteBuilder, pattern: []const u8, handler: Handler) *Router {
        return self.addRouteWithMiddleware(.GET, pattern, handler);
    }

    /// Register a POST route with pending middleware
    pub fn post(self: *RouteBuilder, pattern: []const u8, handler: Handler) *Router {
        return self.addRouteWithMiddleware(.POST, pattern, handler);
    }

    /// Register a PUT route with pending middleware
    pub fn put(self: *RouteBuilder, pattern: []const u8, handler: Handler) *Router {
        return self.addRouteWithMiddleware(.PUT, pattern, handler);
    }

    /// Register a DELETE route with pending middleware
    pub fn delete(self: *RouteBuilder, pattern: []const u8, handler: Handler) *Router {
        return self.addRouteWithMiddleware(.DELETE, pattern, handler);
    }

    /// Register a PATCH route with pending middleware
    pub fn patch(self: *RouteBuilder, pattern: []const u8, handler: Handler) *Router {
        return self.addRouteWithMiddleware(.PATCH, pattern, handler);
    }

    fn addRouteWithMiddleware(self: *RouteBuilder, method: Method, pattern: []const u8, handler: Handler) *Router {
        const middleware_slice = if (self.pending_middleware.items.len > 0)
            self.pending_middleware.toOwnedSlice(self.router.allocator) catch null
        else
            null;

        self.router.addRouteInternal(method, pattern, handler, middleware_slice) catch {};

        // Reset for reuse
        self.pending_middleware = .empty;

        return self.router;
    }
};

/// HTTP Router with path pattern matching
pub const Router = struct {
    allocator: std.mem.Allocator,
    routes: std.ArrayListUnmanaged(Route),
    ws_routes: std.ArrayListUnmanaged(WebSocketRoute),
    not_found_handler: ?Handler,
    global_middleware: std.ArrayListUnmanaged(Middleware),
    route_builder: RouteBuilder,

    pub fn init(allocator: std.mem.Allocator) Router {
        return Router{
            .allocator = allocator,
            .routes = .empty,
            .ws_routes = .empty,
            .not_found_handler = null,
            .global_middleware = .empty,
            .route_builder = RouteBuilder{
                .router = undefined, // Set in with() when we have stable address
                .pending_middleware = .empty,
            },
        };
    }

    pub fn deinit(self: *Router) void {
        for (self.routes.items) |r| {
            self.allocator.free(r.segments);
            if (r.middleware) |m| {
                self.allocator.free(m);
            }
        }
        self.routes.deinit(self.allocator);
        for (self.ws_routes.items) |r| {
            self.allocator.free(r.segments);
        }
        self.ws_routes.deinit(self.allocator);
        self.global_middleware.deinit(self.allocator);
        self.route_builder.pending_middleware.deinit(self.allocator);
    }

    /// Add global middleware (runs on ALL routes)
    pub fn useGlobal(self: *Router, middleware: Middleware) *Router {
        self.global_middleware.append(self.allocator, middleware) catch {};
        return self;
    }

    /// Start a per-route middleware chain
    /// Usage: router.with(auth).with(logging).get("/api", handler)
    pub fn with(self: *Router, middleware: Middleware) *RouteBuilder {
        self.route_builder.router = self; // Set stable pointer
        self.route_builder.pending_middleware = .empty;
        self.route_builder.pending_middleware.append(self.allocator, middleware) catch {};
        return &self.route_builder;
    }

    /// Register a GET route
    pub fn get(self: *Router, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(.GET, pattern, handler) catch {};
        return self;
    }

    /// Register a POST route
    pub fn post(self: *Router, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(.POST, pattern, handler) catch {};
        return self;
    }

    /// Register a PUT route
    pub fn put(self: *Router, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(.PUT, pattern, handler) catch {};
        return self;
    }

    /// Register a DELETE route
    pub fn delete(self: *Router, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(.DELETE, pattern, handler) catch {};
        return self;
    }

    /// Register a PATCH route
    pub fn patch(self: *Router, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(.PATCH, pattern, handler) catch {};
        return self;
    }

    /// Register a route for any method
    pub fn route(self: *Router, method: Method, pattern: []const u8, handler: Handler) *Router {
        self.addRoute(method, pattern, handler) catch {};
        return self;
    }

    /// Set custom 404 handler
    pub fn notFound(self: *Router, handler: Handler) *Router {
        self.not_found_handler = handler;
        return self;
    }

    /// Register a WebSocket route
    pub fn ws(self: *Router, pattern: []const u8, handler: WebSocketHandler) *Router {
        self.addWebSocketRoute(pattern, handler) catch {};
        return self;
    }

    fn addWebSocketRoute(self: *Router, pattern: []const u8, handler: WebSocketHandler) !void {
        const segments = try self.parsePattern(pattern);
        try self.ws_routes.append(self.allocator, .{
            .pattern = pattern,
            .segments = segments,
            .handler = handler,
        });
    }

    fn addRoute(self: *Router, method: Method, pattern: []const u8, handler: Handler) !void {
        try self.addRouteInternal(method, pattern, handler, null);
    }

    fn addRouteInternal(self: *Router, method: Method, pattern: []const u8, handler: Handler, middleware: ?[]const Middleware) !void {
        const segments = try self.parsePattern(pattern);
        try self.routes.append(self.allocator, .{
            .method = method,
            .pattern = pattern,
            .segments = segments,
            .handler = handler,
            .middleware = middleware,
        });
    }

    fn parsePattern(self: *Router, pattern: []const u8) ![]const Segment {
        var segments: std.ArrayListUnmanaged(Segment) = .empty;
        errdefer segments.deinit(self.allocator);

        var iter = std.mem.splitScalar(u8, pattern, '/');
        while (iter.next()) |part| {
            if (part.len == 0) continue;

            if (part[0] == ':') {
                try segments.append(self.allocator, .{ .param = part[1..] });
            } else if (part[0] == '*') {
                try segments.append(self.allocator, .{ .wildcard = part[1..] });
            } else {
                try segments.append(self.allocator, .{ .literal = part });
            }
        }

        return segments.toOwnedSlice(self.allocator);
    }

    /// Match a request to a route and execute the handler with middleware
    pub fn handle(self: *Router, request: *Request, response: *Response) !void {
        var ctx = Context{
            .request = request,
            .response = response,
            .allocator = self.allocator,
        };

        // Find matching route
        for (self.routes.items) |r| {
            if (r.method != request.method) continue;

            if (try self.matchRoute(r, request)) {
                try self.executeWithMiddleware(&ctx, r.handler, r.middleware);
                return;
            }
        }

        // No match found - still run global middleware for not-found
        const not_found = self.not_found_handler orelse defaultNotFound;
        try self.executeWithMiddleware(&ctx, not_found, null);
    }

    fn defaultNotFound(ctx: *Context) anyerror!void {
        _ = ctx.response.notFound();
        try ctx.response.json(.{ .@"error" = "Not Found" });
    }

    /// Execute handler with combined global + route middleware
    fn executeWithMiddleware(self: *Router, ctx: *Context, handler: Handler, route_middleware: ?[]const Middleware) !void {
        const global_count = self.global_middleware.items.len;
        const route_count = if (route_middleware) |rm| rm.len else 0;
        const total = global_count + route_count;

        if (total == 0) {
            // No middleware - call handler directly
            return handler(ctx);
        }

        // Build combined chain (stack allocation for common case <=32 middleware)
        var stack_buf: [32]Middleware = undefined;
        var combined_slice: []const Middleware = undefined;
        var heap_buf: ?[]Middleware = null;
        defer if (heap_buf) |hb| self.allocator.free(hb);

        if (total <= 32) {
            @memcpy(stack_buf[0..global_count], self.global_middleware.items);
            if (route_middleware) |rm| {
                @memcpy(stack_buf[global_count..][0..rm.len], rm);
            }
            combined_slice = stack_buf[0..total];
        } else {
            // Heap allocation for large chains (rare)
            heap_buf = try self.allocator.alloc(Middleware, total);
            @memcpy(heap_buf.?[0..global_count], self.global_middleware.items);
            if (route_middleware) |rm| {
                @memcpy(heap_buf.?[global_count..][0..rm.len], rm);
            }
            combined_slice = heap_buf.?;
        }

        // Cast to opaque context pointer for middleware chain
        const opaque_ctx: *mw.Context = @ptrCast(ctx);
        const opaque_handler: mw.Handler = @ptrCast(handler);

        const chain = MiddlewareChain{
            .middlewares = combined_slice,
            .handler = opaque_handler,
        };

        return chain.run(opaque_ctx);
    }

    fn matchRoute(self: *Router, r: Route, request: *Request) !bool {
        var path_iter = std.mem.splitScalar(u8, request.path, '/');
        var segment_index: usize = 0;

        while (path_iter.next()) |part| {
            if (part.len == 0) continue;

            if (segment_index >= r.segments.len) {
                return false;
            }

            const segment = r.segments[segment_index];
            switch (segment) {
                .literal => |lit| {
                    if (!std.mem.eql(u8, part, lit)) {
                        return false;
                    }
                },
                .param => |name| {
                    // Store parameter value
                    try request.path_params.put(self.allocator, name, part);
                },
                .wildcard => |name| {
                    // Capture rest of path
                    const rest_start = @intFromPtr(part.ptr) - @intFromPtr(request.path.ptr);
                    try request.path_params.put(self.allocator, name, request.path[rest_start..]);
                    return true; // Wildcard consumes rest
                },
            }
            segment_index += 1;
        }

        // All segments must be matched
        return segment_index == r.segments.len;
    }

    /// Find a matching WebSocket route for a path
    pub fn findWebSocketRoute(self: *Router, path: []const u8) ?WebSocketHandler {
        for (self.ws_routes.items) |r| {
            if (self.matchWebSocketRoute(r.segments, path)) {
                return r.handler;
            }
        }
        return null;
    }

    fn matchWebSocketRoute(self: *Router, segments: []const Segment, path: []const u8) bool {
        _ = self;
        var path_iter = std.mem.splitScalar(u8, path, '/');
        var segment_index: usize = 0;

        while (path_iter.next()) |part| {
            if (part.len == 0) continue;

            if (segment_index >= segments.len) {
                return false;
            }

            const segment = segments[segment_index];
            switch (segment) {
                .literal => |lit| {
                    if (!std.mem.eql(u8, part, lit)) {
                        return false;
                    }
                },
                .param, .wildcard => {
                    // Params and wildcards match anything
                    // For wildcards, we don't need to consume the rest for simple matching
                },
            }
            segment_index += 1;
        }

        // All segments must be matched
        return segment_index == segments.len;
    }
};

fn testHandler(ctx: *Context) !void {
    // Verify the parameter was captured
    const id = ctx.param("id") orelse return error.NoParam;
    if (!std.mem.eql(u8, id, "123")) return error.WrongParam;
}

test "router path matching" {
    const allocator = std.testing.allocator;

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    _ = router_instance.get("/users/:id", testHandler);

    // Create a mock request
    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/users/123";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    try std.testing.expectEqualStrings("123", request.param("id").?);
}

// Middleware tests
var middleware_order: [8]u8 = undefined;
var middleware_order_idx: usize = 0;

fn recordChar(c: u8) void {
    if (middleware_order_idx < middleware_order.len) {
        middleware_order[middleware_order_idx] = c;
        middleware_order_idx += 1;
    }
}

fn middleware1(ctx: *mw.Context, next: Next) anyerror!void {
    recordChar('1');
    try next.call(ctx);
    recordChar('a');
}

fn middleware2(ctx: *mw.Context, next: Next) anyerror!void {
    recordChar('2');
    try next.call(ctx);
    recordChar('b');
}

fn shortCircuitMiddleware(_: *mw.Context, _: Next) anyerror!void {
    recordChar('X');
    // Don't call next - short circuit
}

fn middlewareTestHandler(_: *Context) anyerror!void {
    recordChar('H');
}

test "middleware - single middleware execution" {
    const allocator = std.testing.allocator;

    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    _ = router_instance
        .useGlobal(middleware1)
        .get("/test", middlewareTestHandler);

    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/test";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    try std.testing.expectEqualStrings("1Ha", middleware_order[0..middleware_order_idx]);
}

test "middleware - multiple middleware in order" {
    const allocator = std.testing.allocator;

    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    _ = router_instance
        .useGlobal(middleware1)
        .useGlobal(middleware2)
        .get("/test", middlewareTestHandler);

    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/test";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    // Order should be: 1 -> 2 -> H -> b -> a (onion model)
    try std.testing.expectEqualStrings("12Hba", middleware_order[0..middleware_order_idx]);
}

test "middleware - short circuit" {
    const allocator = std.testing.allocator;

    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    _ = router_instance
        .useGlobal(middleware1)
        .useGlobal(shortCircuitMiddleware)
        .useGlobal(middleware2)
        .get("/test", middlewareTestHandler);

    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/test";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    // Should stop at X, middleware2 and handler should not run
    try std.testing.expectEqualStrings("1Xa", middleware_order[0..middleware_order_idx]);
}

test "middleware - per-route with chain builder" {
    const allocator = std.testing.allocator;

    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    // Route with per-route middleware
    _ = router_instance
        .with(middleware1)
        .with(middleware2)
        .get("/protected", middlewareTestHandler);

    // Route without middleware
    _ = router_instance.get("/public", middlewareTestHandler);

    // Test protected route
    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/protected";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    try std.testing.expectEqualStrings("12Hba", middleware_order[0..middleware_order_idx]);

    // Test public route - should only call handler
    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    request.deinit();
    request = Request.init(allocator);
    request.method = .GET;
    request.path = "/public";

    response.deinit();
    response = Response.init(allocator);

    try router_instance.handle(&request, &response);

    try std.testing.expectEqualStrings("H", middleware_order[0..middleware_order_idx]);
}

test "middleware - global plus per-route combined" {
    const allocator = std.testing.allocator;

    middleware_order_idx = 0;
    @memset(&middleware_order, 0);

    var router_instance = Router.init(allocator);
    defer router_instance.deinit();

    // Global middleware first
    _ = router_instance.useGlobal(middleware1);

    // Route with additional per-route middleware
    _ = router_instance
        .with(middleware2)
        .get("/test", middlewareTestHandler);

    var request = Request.init(allocator);
    defer request.deinit();
    request.method = .GET;
    request.path = "/test";

    var response = Response.init(allocator);
    defer response.deinit();

    try router_instance.handle(&request, &response);

    // Global (1) runs first, then per-route (2), then handler, then unwind
    try std.testing.expectEqualStrings("12Hba", middleware_order[0..middleware_order_idx]);
}
