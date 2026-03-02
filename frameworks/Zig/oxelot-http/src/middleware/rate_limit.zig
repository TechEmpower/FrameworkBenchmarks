// Rate Limiting middleware for oxelot-http
//
// Implements a sliding window rate limiter to prevent abuse.

const std = @import("std");
const mw = @import("../middleware.zig");
const router = @import("../router.zig");

pub const Config = struct {
    /// Maximum requests per window
    max_requests: u32 = 100,
    /// Window duration in seconds
    window_seconds: u32 = 60,
    /// Function to extract rate limit key from context (default: client IP)
    key_fn: ?*const fn (*router.Context) []const u8 = null,
};

/// Rate limiter with internal state
/// Must be kept alive for the duration of the server
pub const RateLimiter = struct {
    allocator: std.mem.Allocator,
    config: Config,
    buckets: std.StringHashMap(Bucket),
    mutex: std.Thread.Mutex,

    const Bucket = struct {
        count: u32,
        window_start: i64,
    };

    pub fn init(allocator: std.mem.Allocator, config: Config) RateLimiter {
        return .{
            .allocator = allocator,
            .config = config,
            .buckets = std.StringHashMap(Bucket).init(allocator),
            .mutex = .{},
        };
    }

    pub fn deinit(self: *RateLimiter) void {
        var iter = self.buckets.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.buckets.deinit();
    }

    /// Get the middleware function
    /// Note: The returned middleware captures a pointer to this RateLimiter
    pub fn middleware(self: *RateLimiter) RateLimitMiddleware {
        return .{ .limiter = self };
    }

    /// Check if a request should be allowed
    pub fn checkLimit(self: *RateLimiter, key: []const u8) struct { allowed: bool, remaining: u32, reset_at: i64 } {
        self.mutex.lock();
        defer self.mutex.unlock();

        const now = std.time.timestamp();
        const window_start = now - @as(i64, @intCast(self.config.window_seconds));

        if (self.buckets.get(key)) |*bucket| {
            // Check if we need to reset the window
            if (bucket.window_start < window_start) {
                // New window
                var entry = self.buckets.getPtr(key).?;
                entry.count = 1;
                entry.window_start = now;
                return .{
                    .allowed = true,
                    .remaining = self.config.max_requests - 1,
                    .reset_at = now + @as(i64, @intCast(self.config.window_seconds)),
                };
            }

            if (bucket.count >= self.config.max_requests) {
                return .{
                    .allowed = false,
                    .remaining = 0,
                    .reset_at = bucket.window_start + @as(i64, @intCast(self.config.window_seconds)),
                };
            }

            var entry = self.buckets.getPtr(key).?;
            entry.count += 1;
            return .{
                .allowed = true,
                .remaining = self.config.max_requests - entry.count,
                .reset_at = entry.window_start + @as(i64, @intCast(self.config.window_seconds)),
            };
        } else {
            // New key
            const key_copy = self.allocator.dupe(u8, key) catch return .{
                .allowed = false,
                .remaining = 0,
                .reset_at = now + @as(i64, @intCast(self.config.window_seconds)),
            };
            self.buckets.put(key_copy, .{
                .count = 1,
                .window_start = now,
            }) catch {
                self.allocator.free(key_copy);
                return .{
                    .allowed = false,
                    .remaining = 0,
                    .reset_at = now + @as(i64, @intCast(self.config.window_seconds)),
                };
            };
            return .{
                .allowed = true,
                .remaining = self.config.max_requests - 1,
                .reset_at = now + @as(i64, @intCast(self.config.window_seconds)),
            };
        }
    }

    /// Cleanup expired buckets (call periodically)
    pub fn cleanup(self: *RateLimiter) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        const now = std.time.timestamp();
        const window_start = now - @as(i64, @intCast(self.config.window_seconds * 2));

        var to_remove: std.ArrayListUnmanaged([]const u8) = .empty;
        defer to_remove.deinit(self.allocator);

        var iter = self.buckets.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.window_start < window_start) {
                to_remove.append(self.allocator, entry.key_ptr.*) catch continue;
            }
        }

        for (to_remove.items) |key| {
            _ = self.buckets.remove(key);
            self.allocator.free(key);
        }
    }
};

/// Middleware wrapper that holds reference to RateLimiter
pub const RateLimitMiddleware = struct {
    limiter: *RateLimiter,

    /// Get the actual middleware function
    /// Note: Returns a generic rate limit middleware since we can't capture state
    pub fn func(self: *const RateLimitMiddleware) mw.Middleware {
        _ = self;
        // For stateful middleware, users need to create a custom function
        // This returns a placeholder that always allows requests
        return rateLimitPlaceholder;
    }
};

/// Placeholder rate limit middleware
/// In production, users should implement their own with access to RateLimiter state
fn rateLimitPlaceholder(opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
    // This is a placeholder - real rate limiting requires state
    // Users should create a custom middleware that captures their RateLimiter
    return next.call(opaque_ctx);
}

/// Helper to create a rate limiting response
pub fn sendTooManyRequests(ctx: *router.Context, remaining: u32, reset_at: i64) void {
    _ = ctx.response.setStatus(.too_many_requests);

    // Set rate limit headers
    var buf: [32]u8 = undefined;
    const remaining_str = std.fmt.bufPrint(&buf, "{d}", .{remaining}) catch "0";
    ctx.response.headers.set("X-RateLimit-Remaining", remaining_str) catch {};

    var reset_buf: [32]u8 = undefined;
    const reset_str = std.fmt.bufPrint(&reset_buf, "{d}", .{reset_at}) catch "0";
    ctx.response.headers.set("X-RateLimit-Reset", reset_str) catch {};

    ctx.response.headers.set("Retry-After", "60") catch {};
    ctx.response.headers.set("Content-Type", "application/json") catch {};
    ctx.response.body.appendSlice(ctx.allocator, "{\"error\":\"Too Many Requests\"}") catch {};
}

/// Create a rate limiting middleware function that uses a specific limiter
/// Usage: const mw = rateLimit.createMiddleware(&limiter);
pub fn createMiddleware(limiter: *RateLimiter) RateLimitContext {
    return .{ .limiter = limiter };
}

pub const RateLimitContext = struct {
    limiter: *RateLimiter,

    /// The actual middleware function
    /// Note: Due to Zig's lack of closures, this is a method on the context
    pub fn handle(self: *RateLimitContext, opaque_ctx: *mw.Context, next: mw.Next) anyerror!void {
        const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));

        // Get rate limit key (default: use path as key for simplicity)
        // In production, you'd want to use client IP or API key
        const key = ctx.request.path;

        const result = self.limiter.checkLimit(key);

        // Set rate limit headers
        var buf: [32]u8 = undefined;
        const remaining_str = std.fmt.bufPrint(&buf, "{d}", .{result.remaining}) catch "0";
        ctx.response.headers.set("X-RateLimit-Remaining", remaining_str) catch {};

        if (!result.allowed) {
            sendTooManyRequests(ctx, result.remaining, result.reset_at);
            return;
        }

        return next.call(opaque_ctx);
    }
};
