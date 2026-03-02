// Metrics Registry
//
// Central store for all application metrics. Provides labeled metric support
// where metrics can be indexed by multiple label values (e.g., method, route, status).
//
// Thread-safe with atomic operations for metric values and mutex for registry operations.

const std = @import("std");
const types = @import("types.zig");
pub const Counter = types.Counter;
pub const Gauge = types.Gauge;
pub const Histogram = types.Histogram;

/// Label key used for metric lookups
pub const LabelKey = struct {
    values: [MAX_LABELS][]const u8,
    len: usize,

    pub const MAX_LABELS = 4;

    pub fn init(labels: anytype) LabelKey {
        var key = LabelKey{
            .values = undefined,
            .len = 0,
        };

        const info = @typeInfo(@TypeOf(labels));
        if (info == .@"struct" and info.@"struct".is_tuple) {
            inline for (labels, 0..) |label, i| {
                if (i < MAX_LABELS) {
                    key.values[i] = asString(label);
                    key.len = i + 1;
                }
            }
        }
        return key;
    }

    fn asString(value: anytype) []const u8 {
        const T = @TypeOf(value);
        const info = @typeInfo(T);

        // Handle enums first
        if (info == .@"enum") {
            return @tagName(value);
        }

        // Handle string slices directly
        if (T == []const u8) {
            return value;
        }

        // Handle string literals (*const [N:0]u8) and other pointer types
        if (info == .pointer) {
            const ptr_info = info.pointer;
            if (@typeInfo(ptr_info.child) == .array) {
                const arr_info = @typeInfo(ptr_info.child).array;
                if (arr_info.child == u8) {
                    // String literal - coerce to slice
                    return value;
                }
            }
        }

        return "unknown";
    }

    pub fn hash(self: LabelKey) u32 {
        var h: u32 = 0;
        for (0..self.len) |i| {
            for (self.values[i]) |c| {
                h = h *% 31 +% c;
            }
            h = h *% 31 +% ',';
        }
        return h;
    }

    pub fn eql(a: LabelKey, b: LabelKey) bool {
        if (a.len != b.len) return false;
        for (0..a.len) |i| {
            if (!std.mem.eql(u8, a.values[i], b.values[i])) return false;
        }
        return true;
    }

    /// Create a copy of this LabelKey with owned string values
    /// The caller is responsible for calling deinitValues to free the memory
    pub fn dupe(self: LabelKey, allocator: std.mem.Allocator) !LabelKey {
        var result = LabelKey{
            .values = undefined,
            .len = self.len,
        };
        for (0..self.len) |i| {
            result.values[i] = try allocator.dupe(u8, self.values[i]);
        }
        return result;
    }

    /// Free the owned string values in this LabelKey
    pub fn deinitValues(self: *LabelKey, allocator: std.mem.Allocator) void {
        for (0..self.len) |i| {
            allocator.free(self.values[i]);
        }
        self.len = 0;
    }
};

/// A metric family with multiple labeled instances
pub fn LabeledMetric(comptime T: type) type {
    return struct {
        const Self = @This();

        metrics: std.ArrayHashMapUnmanaged(LabelKey, *T, LabelKeyContext, true),
        allocator: std.mem.Allocator,
        mutex: std.Thread.Mutex,
        name: []const u8,
        help: []const u8,
        label_names: []const []const u8,
        buckets: ?[]const f64, // Only for Histogram

        const LabelKeyContext = struct {
            pub fn hash(_: LabelKeyContext, key: LabelKey) u32 {
                return key.hash();
            }
            pub fn eql(_: LabelKeyContext, a: LabelKey, b: LabelKey, _: usize) bool {
                return a.eql(b);
            }
        };

        pub fn init(
            allocator: std.mem.Allocator,
            name: []const u8,
            help: []const u8,
            label_names: []const []const u8,
            buckets: ?[]const f64,
        ) Self {
            return .{
                .metrics = .{},
                .allocator = allocator,
                .mutex = .{},
                .name = name,
                .help = help,
                .label_names = label_names,
                .buckets = buckets,
            };
        }

        pub fn deinit(self: *Self) void {
            var iter = self.metrics.iterator();
            while (iter.next()) |entry| {
                // Free owned label strings
                var key = entry.key_ptr.*;
                key.deinitValues(self.allocator);

                if (T == Histogram) {
                    entry.value_ptr.*.deinit();
                }
                self.allocator.destroy(entry.value_ptr.*);
            }
            self.metrics.deinit(self.allocator);
        }

        /// Get or create a metric instance with the given labels
        pub fn get(self: *Self, labels: anytype) *T {
            const key = LabelKey.init(labels);

            // Fast path: check without lock
            if (self.metrics.get(key)) |metric| {
                return metric;
            }

            // Slow path: acquire lock and create
            self.mutex.lock();
            defer self.mutex.unlock();

            // Double-check after acquiring lock
            if (self.metrics.get(key)) |metric| {
                return metric;
            }

            // Create new metric
            const metric = self.allocator.create(T) catch unreachable;
            if (T == Histogram) {
                metric.* = Histogram.init(self.allocator, self.buckets orelse Histogram.DEFAULT_BUCKETS) catch unreachable;
            } else {
                metric.* = T.init();
            }

            // Dupe the key to own the label strings (prevents use-after-free when caller's memory is freed)
            const owned_key = key.dupe(self.allocator) catch unreachable;
            self.metrics.put(self.allocator, owned_key, metric) catch unreachable;
            return metric;
        }

        /// Increment a counter with the given labels
        pub fn inc(self: *Self, labels: anytype) void {
            self.get(labels).inc();
        }

        /// Add to a counter with the given labels
        pub fn add(self: *Self, labels: anytype, n: u64) void {
            self.get(labels).add(n);
        }

        /// Observe a value in a histogram with the given labels
        pub fn observe(self: *Self, labels: anytype, value: u64) void {
            if (T == Histogram) {
                self.get(labels).observe(value);
            }
        }

        /// Iterate over all metric instances
        pub fn iterator(self: *Self) std.ArrayHashMapUnmanaged(LabelKey, *T, LabelKeyContext, true).Iterator {
            return self.metrics.iterator();
        }
    };
}

pub const LabeledCounter = LabeledMetric(Counter);
pub const LabeledHistogram = LabeledMetric(Histogram);

/// Central metrics registry
pub const Registry = struct {
    allocator: std.mem.Allocator,

    // HTTP server metrics
    http_requests_total: LabeledCounter,
    http_request_duration: LabeledHistogram,
    http_requests_in_flight: Gauge,

    // HTTP client metrics
    http_client_requests_total: LabeledCounter,
    http_client_request_duration: LabeledHistogram,
    http_client_pool_connections: Gauge,
    http_client_pool_in_use: Gauge,

    // PostgreSQL metrics
    pg_pool_connections_total: Gauge,
    pg_pool_connections_in_use: Gauge,
    pg_query_total: LabeledCounter,
    pg_query_duration: LabeledHistogram,

    // Process metrics
    process_memory_bytes: Gauge,

    pub fn init(allocator: std.mem.Allocator) Registry {
        return .{
            .allocator = allocator,

            // HTTP server
            .http_requests_total = LabeledCounter.init(
                allocator,
                "http_requests_total",
                "Total number of HTTP requests",
                &.{ "method", "route", "status" },
                null,
            ),
            .http_request_duration = LabeledHistogram.init(
                allocator,
                "http_request_duration_seconds",
                "HTTP request latency in seconds",
                &.{ "method", "route" },
                Histogram.DEFAULT_BUCKETS,
            ),
            .http_requests_in_flight = Gauge.init(),

            // HTTP client
            .http_client_requests_total = LabeledCounter.init(
                allocator,
                "http_client_requests_total",
                "Total number of outbound HTTP requests",
                &.{ "method", "host", "status" },
                null,
            ),
            .http_client_request_duration = LabeledHistogram.init(
                allocator,
                "http_client_request_duration_seconds",
                "Outbound HTTP request latency in seconds",
                &.{ "method", "host" },
                Histogram.DEFAULT_BUCKETS,
            ),
            .http_client_pool_connections = Gauge.init(),
            .http_client_pool_in_use = Gauge.init(),

            // PostgreSQL
            .pg_pool_connections_total = Gauge.init(),
            .pg_pool_connections_in_use = Gauge.init(),
            .pg_query_total = LabeledCounter.init(
                allocator,
                "pg_query_total",
                "Total number of PostgreSQL queries",
                &.{"operation"},
                null,
            ),
            .pg_query_duration = LabeledHistogram.init(
                allocator,
                "pg_query_duration_seconds",
                "PostgreSQL query latency in seconds",
                &.{"operation"},
                Histogram.DB_BUCKETS,
            ),

            // Process
            .process_memory_bytes = Gauge.init(),
        };
    }

    pub fn deinit(self: *Registry) void {
        self.http_requests_total.deinit();
        self.http_request_duration.deinit();
        self.http_client_requests_total.deinit();
        self.http_client_request_duration.deinit();
        self.pg_query_total.deinit();
        self.pg_query_duration.deinit();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "labeled counter" {
    const allocator = std.testing.allocator;

    var counter = LabeledCounter.init(
        allocator,
        "test_counter",
        "A test counter",
        &.{ "method", "path" },
        null,
    );
    defer counter.deinit();

    counter.inc(.{ "GET", "/api/users" });
    counter.inc(.{ "GET", "/api/users" });
    counter.inc(.{ "POST", "/api/users" });

    const get_users = counter.get(.{ "GET", "/api/users" });
    try std.testing.expectEqual(@as(u64, 2), get_users.get());

    const post_users = counter.get(.{ "POST", "/api/users" });
    try std.testing.expectEqual(@as(u64, 1), post_users.get());
}

test "labeled histogram" {
    const allocator = std.testing.allocator;

    var histogram = LabeledHistogram.init(
        allocator,
        "test_histogram",
        "A test histogram",
        &.{"method"},
        &.{ 0.001, 0.01, 0.1 },
    );
    defer histogram.deinit();

    histogram.observe(.{"GET"}, 500_000); // 0.5ms
    histogram.observe(.{"GET"}, 5_000_000); // 5ms
    histogram.observe(.{"POST"}, 50_000_000); // 50ms

    const get_hist = histogram.get(.{"GET"});
    try std.testing.expectEqual(@as(u64, 2), get_hist.getCount());

    const post_hist = histogram.get(.{"POST"});
    try std.testing.expectEqual(@as(u64, 1), post_hist.getCount());
}

test "registry init" {
    const allocator = std.testing.allocator;

    var registry = Registry.init(allocator);
    defer registry.deinit();

    registry.http_requests_total.inc(.{ "GET", "/", "200" });
    try std.testing.expectEqual(@as(u64, 1), registry.http_requests_total.get(.{ "GET", "/", "200" }).get());

    registry.http_requests_in_flight.inc();
    try std.testing.expectEqual(@as(i64, 1), registry.http_requests_in_flight.get());
}
