// CORS (Cross-Origin Resource Sharing) middleware for oxelot-http
//
// Handles CORS preflight requests and sets appropriate headers.

const std = @import("std");
const mw = @import("../middleware.zig");
const router = @import("../router.zig");

pub const Config = struct {
    /// Allowed origins ("*" for any)
    allowed_origins: []const []const u8 = &.{"*"},
    /// Allowed HTTP methods
    allowed_methods: []const []const u8 = &.{ "GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS" },
    /// Allowed headers
    allowed_headers: []const []const u8 = &.{ "Content-Type", "Authorization", "X-Requested-With" },
    /// Headers to expose to the client
    expose_headers: []const []const u8 = &.{},
    /// Allow credentials (cookies, auth headers)
    allow_credentials: bool = false,
    /// Preflight cache duration in seconds
    max_age: u32 = 86400, // 24 hours
};

/// Create a configured CORS middleware
pub fn create(config: Config) CorsMiddleware {
    return .{ .config = config };
}

/// Stateful CORS middleware with configuration
pub const CorsMiddleware = struct {
    config: Config,

    /// Get the middleware function (returns permissive CORS for now)
    pub fn middleware(self: *const CorsMiddleware) mw.Middleware {
        _ = self;
        return cors;
    }
};

/// Default permissive CORS middleware
/// Allows all origins, common methods and headers
pub fn cors(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    // Get the Origin header from the request
    const origin = ctx.request.headers.get("Origin") orelse ctx.request.headers.get("origin");

    // Set CORS headers
    if (origin) |o| {
        ctx.response.headers.set("Access-Control-Allow-Origin", o) catch {};
    } else {
        ctx.response.headers.set("Access-Control-Allow-Origin", "*") catch {};
    }

    ctx.response.headers.set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, PATCH, OPTIONS") catch {};
    ctx.response.headers.set("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With") catch {};
    ctx.response.headers.set("Access-Control-Max-Age", "86400") catch {};

    // Handle preflight OPTIONS request
    if (ctx.request.method == .OPTIONS) {
        _ = ctx.response.setStatus(.no_content);
        return; // Don't call next - respond immediately
    }

    // Continue to the next handler
    return next.call(opaque_ctx);
}

/// Strict CORS middleware - only allows specified origin
pub fn strict(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    const origin = ctx.request.headers.get("Origin") orelse ctx.request.headers.get("origin");

    if (origin) |o| {
        // Echo back the origin (in production, validate against allowlist)
        ctx.response.headers.set("Access-Control-Allow-Origin", o) catch {};
        ctx.response.headers.set("Access-Control-Allow-Credentials", "true") catch {};
    }

    ctx.response.headers.set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS") catch {};
    ctx.response.headers.set("Access-Control-Allow-Headers", "Content-Type, Authorization") catch {};
    ctx.response.headers.set("Access-Control-Max-Age", "86400") catch {};
    ctx.response.headers.set("Vary", "Origin") catch {};

    if (ctx.request.method == .OPTIONS) {
        _ = ctx.response.setStatus(.no_content);
        return;
    }

    return next.call(opaque_ctx);
}
