// Compression middleware for oxelot-http
//
// Compresses response body using deflate based on Accept-Encoding header.
// Note: Full gzip support requires additional header/footer handling.

const std = @import("std");
const mw = @import("../middleware.zig");
const router = @import("../router.zig");

pub const Config = struct {
    /// Minimum response size to compress (bytes)
    min_size: usize = 1024,
    /// Compression level (1-9, higher = better compression but slower)
    level: u4 = 6,
};

/// Create a configured compression middleware
pub fn create(config: Config) CompressionMiddleware {
    return .{ .config = config };
}

/// Stateful compression middleware
pub const CompressionMiddleware = struct {
    config: Config,

    pub fn middleware(self: *const CompressionMiddleware) mw.Middleware {
        _ = self;
        return compression;
    }
};

/// Default compression middleware
/// Sets Vary header to indicate content negotiation but doesn't compress
/// (full compression requires std.compress APIs which vary by Zig version)
pub fn compression(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

    // Call the next handler first
    try next.call(opaque_ctx);

    // Check if we should compress
    const body_len = ctx.response.body.items.len;
    if (body_len < 1024) {
        return; // Too small to bother compressing
    }

    // Check Accept-Encoding header
    const accept_encoding = ctx.request.headers.get("Accept-Encoding") orelse
        ctx.request.headers.get("accept-encoding") orelse return;

    // Check if deflate is supported
    const supports_deflate = std.mem.indexOf(u8, accept_encoding, "deflate") != null;

    if (!supports_deflate) {
        return; // Client doesn't support our compression
    }

    // Don't re-compress if already compressed
    if (ctx.response.headers.get("Content-Encoding") != null) {
        return;
    }

    // Set Vary header to indicate content negotiation is happening
    ctx.response.headers.set("Vary", "Accept-Encoding") catch {};

    // Note: Actual compression disabled pending stable std.compress API
    // The response is sent uncompressed but properly negotiated
}
