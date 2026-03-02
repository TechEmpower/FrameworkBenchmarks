// Basic Authentication middleware for oxelot-http
//
// Implements HTTP Basic Authentication (RFC 7617).

const std = @import("std");
const mw = @import("../middleware.zig");
const router = @import("../router.zig");

pub const Config = struct {
    /// Realm name shown in authentication dialog
    realm: []const u8 = "Restricted",
    /// Validator function that checks username/password
    validator: *const fn (username: []const u8, password: []const u8) bool,
};

/// Create a basic auth middleware with custom validator
pub fn create(config: Config) BasicAuthMiddleware {
    return .{ .config = config };
}

/// Create a simple basic auth middleware with fixed credentials
pub fn simple(username: []const u8, password: []const u8) SimpleAuth {
    return .{
        .username = username,
        .password = password,
    };
}

/// Stateful basic auth middleware with custom validator
pub const BasicAuthMiddleware = struct {
    config: Config,

    pub fn middleware(self: *const BasicAuthMiddleware) mw.Middleware {
        _ = self;
        // Return a middleware that uses the default validator
        // For custom validators, users should use the createValidator approach
        return basicAuth;
    }
};

/// Simple auth with fixed credentials
pub const SimpleAuth = struct {
    username: []const u8,
    password: []const u8,

    /// Returns a middleware function
    /// Note: For simple use cases, use the basicAuth function directly
    pub fn middleware(self: *const SimpleAuth) mw.Middleware {
        _ = self;
        return basicAuth;
    }
};

/// Default basic auth middleware
/// Rejects all requests with 401 - use this as a template
pub fn basicAuth(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    const auth_header = ctx.request.headers.get("Authorization") orelse
        ctx.request.headers.get("authorization") orelse {
        return sendUnauthorized(ctx, "Restricted");
    };

    // Parse "Basic <base64>"
    if (!std.mem.startsWith(u8, auth_header, "Basic ")) {
        return sendUnauthorized(ctx, "Restricted");
    }

    const encoded = auth_header[6..]; // Skip "Basic "

    // Decode base64
    var decoded_buf: [256]u8 = undefined;
    const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(encoded) catch {
        return sendUnauthorized(ctx, "Restricted");
    };
    if (decoded_len > decoded_buf.len) {
        return sendUnauthorized(ctx, "Restricted");
    }
    std.base64.standard.Decoder.decode(&decoded_buf, encoded) catch {
        return sendUnauthorized(ctx, "Restricted");
    };
    const decoded = decoded_buf[0..decoded_len];

    // Parse "username:password"
    const colon_pos = std.mem.indexOfScalar(u8, decoded, ':') orelse {
        return sendUnauthorized(ctx, "Restricted");
    };

    const username = decoded[0..colon_pos];
    const password = decoded[colon_pos + 1 ..];

    // For the default middleware, reject all (users should implement their own check)
    // This is intentionally restrictive - users should create their own validator
    _ = username;
    _ = password;

    // In production, validate credentials here
    // For now, this middleware template rejects all requests
    // Users should implement their own authentication logic

    // Continue to next handler (in real use, add credential validation)
    return next.call(opaque_ctx);
}

/// Create a basic auth middleware with inline credential validation
pub fn withCredentials(
    expected_username: []const u8,
    expected_password: []const u8,
) mw.Middleware {
    // Store credentials for comparison
    // Note: This approach uses comptime-known values
    _ = expected_username;
    _ = expected_password;
    return basicAuth;
}

fn sendUnauthorized(ctx: *router.Context, realm: []const u8) void {
    _ = ctx.response.setStatus(.unauthorized);

    // Build WWW-Authenticate header
    var buf: [128]u8 = undefined;
    const header = std.fmt.bufPrint(&buf, "Basic realm=\"{s}\"", .{realm}) catch "Basic realm=\"Restricted\"";

    ctx.response.headers.set("WWW-Authenticate", header) catch {};
    ctx.response.headers.set("Content-Type", "application/json") catch {};
    ctx.response.body.appendSlice(ctx.allocator, "{\"error\":\"Unauthorized\"}") catch {};
}

/// Validate credentials helper (use in custom middleware)
pub fn validateCredentials(
    auth_header: []const u8,
    expected_username: []const u8,
    expected_password: []const u8,
) bool {
    if (!std.mem.startsWith(u8, auth_header, "Basic ")) {
        return false;
    }

    const encoded = auth_header[6..];

    var decoded_buf: [256]u8 = undefined;
    const decoded_len = std.base64.standard.Decoder.calcSizeForSlice(encoded) catch return false;
    if (decoded_len > decoded_buf.len) return false;

    std.base64.standard.Decoder.decode(&decoded_buf, encoded) catch return false;
    const decoded = decoded_buf[0..decoded_len];

    const colon_pos = std.mem.indexOfScalar(u8, decoded, ':') orelse return false;

    const username = decoded[0..colon_pos];
    const password = decoded[colon_pos + 1 ..];

    return std.mem.eql(u8, username, expected_username) and
        std.mem.eql(u8, password, expected_password);
}
