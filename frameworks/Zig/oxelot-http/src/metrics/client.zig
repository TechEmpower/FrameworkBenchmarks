// HTTP Client Metrics Instrumentation
//
// Wraps an HTTP client to add metrics collection for:
// - Outbound request count by method, host, status
// - Request duration histogram
// - Connection pool stats

const std = @import("std");
const reg = @import("registry.zig");

const Registry = reg.Registry;

/// Instrumented HTTP client wrapper
/// Wraps an HTTP client to add metrics collection
pub fn InstrumentedClient(comptime Client: type) type {
    return struct {
        const Self = @This();

        client: *Client,
        registry: *Registry,

        /// Create an instrumented client wrapper
        pub fn init(client: *Client, registry: *Registry) Self {
            return .{
                .client = client,
                .registry = registry,
            };
        }

        /// Simple GET request with metrics
        pub fn get(self: *Self, url: []const u8) !Client.Response {
            return self.request(.GET, url, .{});
        }

        /// Simple HEAD request with metrics
        pub fn head(self: *Self, url: []const u8) !Client.Response {
            return self.request(.HEAD, url, .{});
        }

        /// Simple DELETE request with metrics
        pub fn delete(self: *Self, url: []const u8) !Client.Response {
            return self.request(.DELETE, url, .{});
        }

        /// POST request with metrics
        pub fn post(self: *Self, url: []const u8, options: Client.RequestOptions) !Client.Response {
            return self.request(.POST, url, options);
        }

        /// PUT request with metrics
        pub fn put(self: *Self, url: []const u8, options: Client.RequestOptions) !Client.Response {
            return self.request(.PUT, url, options);
        }

        /// PATCH request with metrics
        pub fn patch(self: *Self, url: []const u8, options: Client.RequestOptions) !Client.Response {
            return self.request(.PATCH, url, options);
        }

        /// Make an HTTP request with metrics
        pub fn request(self: *Self, method: anytype, url: []const u8, options: Client.RequestOptions) !Client.Response {
            const host = parseHost(url);
            const method_str = methodToString(method);
            const start = std.time.Instant.now() catch null;

            const response = try self.client.request(method, url, options);

            const status_str = statusToString(response.status.code());
            self.registry.http_client_requests_total.inc(.{ method_str, host, status_str });

            if (start) |s| {
                if (std.time.Instant.now()) |end| {
                    const elapsed_ns = end.since(s);
                    self.registry.http_client_request_duration.observe(.{ method_str, host }, elapsed_ns);
                } else |_| {}
            }

            return response;
        }

        /// Collect and update connection pool statistics
        pub fn collectPoolStats(self: *Self) void {
            if (self.client.poolStats()) |stats| {
                self.registry.http_client_pool_connections.set(@intCast(stats.total_slots));
                self.registry.http_client_pool_in_use.set(@intCast(stats.in_use));
            }
        }

        /// Deinitialize the underlying client
        pub fn deinit(self: *Self) void {
            self.client.deinit();
        }
    };
}

/// Parse host from URL
pub fn parseHost(url: []const u8) []const u8 {
    // Find start of host (after "://")
    var start: usize = 0;
    if (std.mem.indexOf(u8, url, "://")) |pos| {
        start = pos + 3;
    }

    // Find end of host (before "/" or ":" or end)
    var end = start;
    while (end < url.len) {
        if (url[end] == '/' or url[end] == ':' or url[end] == '?') break;
        end += 1;
    }

    if (end > start) {
        return url[start..end];
    }
    return "unknown";
}

/// Convert method to string
fn methodToString(method: anytype) []const u8 {
    const T = @TypeOf(method);
    if (@typeInfo(T) == .@"enum") {
        return @tagName(method);
    }
    return "UNKNOWN";
}

/// Convert status code to string
fn statusToString(code: u16) []const u8 {
    return switch (code) {
        200 => "200",
        201 => "201",
        204 => "204",
        301 => "301",
        302 => "302",
        304 => "304",
        400 => "400",
        401 => "401",
        403 => "403",
        404 => "404",
        405 => "405",
        500 => "500",
        502 => "502",
        503 => "503",
        else => "other",
    };
}

// ============================================================================
// Tests
// ============================================================================

test "parse host" {
    try std.testing.expectEqualStrings("example.com", parseHost("http://example.com/path"));
    try std.testing.expectEqualStrings("example.com", parseHost("https://example.com:8080/path"));
    try std.testing.expectEqualStrings("example.com", parseHost("https://example.com"));
    try std.testing.expectEqualStrings("api.example.com", parseHost("https://api.example.com/v1/users"));
    try std.testing.expectEqualStrings("localhost", parseHost("http://localhost:8080/"));
}
