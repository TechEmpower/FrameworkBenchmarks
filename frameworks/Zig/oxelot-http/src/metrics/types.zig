// Metrics Types
//
// Thread-safe metric primitives using atomic operations.
// These types are designed to be used concurrently from multiple threads
// without mutex contention on the hot path.

const std = @import("std");

/// Thread-safe counter that can only increase.
/// Uses atomic operations for lock-free increments.
pub const Counter = struct {
    value: std.atomic.Value(u64),

    pub fn init() Counter {
        return .{ .value = std.atomic.Value(u64).init(0) };
    }

    /// Increment the counter by 1.
    pub fn inc(self: *Counter) void {
        _ = self.value.fetchAdd(1, .monotonic);
    }

    /// Add n to the counter.
    pub fn add(self: *Counter, n: u64) void {
        _ = self.value.fetchAdd(n, .monotonic);
    }

    /// Get the current counter value.
    pub fn get(self: *const Counter) u64 {
        return self.value.load(.monotonic);
    }

    /// Reset the counter to 0.
    pub fn reset(self: *Counter) void {
        self.value.store(0, .monotonic);
    }
};

/// Thread-safe gauge that can increase or decrease.
/// Uses atomic operations for lock-free updates.
pub const Gauge = struct {
    value: std.atomic.Value(i64),

    pub fn init() Gauge {
        return .{ .value = std.atomic.Value(i64).init(0) };
    }

    /// Set the gauge to a specific value.
    pub fn set(self: *Gauge, v: i64) void {
        self.value.store(v, .monotonic);
    }

    /// Increment the gauge by 1.
    pub fn inc(self: *Gauge) void {
        _ = self.value.fetchAdd(1, .monotonic);
    }

    /// Decrement the gauge by 1.
    pub fn dec(self: *Gauge) void {
        _ = self.value.fetchSub(1, .monotonic);
    }

    /// Add n to the gauge.
    pub fn add(self: *Gauge, n: i64) void {
        _ = self.value.fetchAdd(n, .monotonic);
    }

    /// Subtract n from the gauge.
    pub fn sub(self: *Gauge, n: i64) void {
        _ = self.value.fetchSub(n, .monotonic);
    }

    /// Get the current gauge value.
    pub fn get(self: *const Gauge) i64 {
        return self.value.load(.monotonic);
    }
};

/// Thread-safe histogram for measuring distributions.
/// Uses atomic operations for lock-free observation recording.
///
/// Bucket boundaries are specified in seconds (as f64).
/// Observations are recorded in nanoseconds (u64) and converted to seconds
/// for bucket comparison.
pub const Histogram = struct {
    /// Bucket boundaries in seconds (upper bounds, exclusive)
    buckets: []const f64,
    /// Count of observations that fell into each bucket (cumulative)
    counts: []std.atomic.Value(u64),
    /// Sum of all observed values in nanoseconds
    sum: std.atomic.Value(u64),
    /// Total number of observations
    count: std.atomic.Value(u64),
    allocator: std.mem.Allocator,

    /// Default bucket boundaries for HTTP request latency (in seconds)
    pub const DEFAULT_BUCKETS: []const f64 = &.{
        0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0,
    };

    /// Default bucket boundaries for database query latency (in seconds)
    pub const DB_BUCKETS: []const f64 = &.{
        0.0001, 0.0005, 0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0,
    };

    pub fn init(allocator: std.mem.Allocator, buckets: []const f64) !Histogram {
        // Allocate counts array (+1 for +Inf bucket)
        const counts = try allocator.alloc(std.atomic.Value(u64), buckets.len + 1);
        for (counts) |*c| {
            c.* = std.atomic.Value(u64).init(0);
        }

        return .{
            .buckets = buckets,
            .counts = counts,
            .sum = std.atomic.Value(u64).init(0),
            .count = std.atomic.Value(u64).init(0),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Histogram) void {
        self.allocator.free(self.counts);
    }

    /// Observe a value in nanoseconds.
    pub fn observe(self: *Histogram, value_ns: u64) void {
        const value_sec = @as(f64, @floatFromInt(value_ns)) / 1_000_000_000.0;

        // Increment bucket counts (cumulative)
        for (self.buckets, 0..) |bucket, i| {
            if (value_sec <= bucket) {
                _ = self.counts[i].fetchAdd(1, .monotonic);
            }
        }
        // Always increment +Inf bucket
        _ = self.counts[self.buckets.len].fetchAdd(1, .monotonic);

        // Update sum and count
        _ = self.sum.fetchAdd(value_ns, .monotonic);
        _ = self.count.fetchAdd(1, .monotonic);
    }

    /// Get the count for a specific bucket index.
    pub fn getBucketCount(self: *const Histogram, index: usize) u64 {
        if (index >= self.counts.len) return 0;
        return self.counts[index].load(.monotonic);
    }

    /// Get the total count of observations.
    pub fn getCount(self: *const Histogram) u64 {
        return self.count.load(.monotonic);
    }

    /// Get the sum of all observations in nanoseconds.
    pub fn getSum(self: *const Histogram) u64 {
        return self.sum.load(.monotonic);
    }

    /// Get the sum of all observations in seconds (as f64).
    pub fn getSumSeconds(self: *const Histogram) f64 {
        return @as(f64, @floatFromInt(self.sum.load(.monotonic))) / 1_000_000_000.0;
    }

    /// Reset all values to zero.
    pub fn reset(self: *Histogram) void {
        for (self.counts) |*c| {
            c.store(0, .monotonic);
        }
        self.sum.store(0, .monotonic);
        self.count.store(0, .monotonic);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "counter basic operations" {
    var counter = Counter.init();

    try std.testing.expectEqual(@as(u64, 0), counter.get());

    counter.inc();
    try std.testing.expectEqual(@as(u64, 1), counter.get());

    counter.add(5);
    try std.testing.expectEqual(@as(u64, 6), counter.get());

    counter.reset();
    try std.testing.expectEqual(@as(u64, 0), counter.get());
}

test "gauge basic operations" {
    var gauge = Gauge.init();

    try std.testing.expectEqual(@as(i64, 0), gauge.get());

    gauge.set(100);
    try std.testing.expectEqual(@as(i64, 100), gauge.get());

    gauge.inc();
    try std.testing.expectEqual(@as(i64, 101), gauge.get());

    gauge.dec();
    try std.testing.expectEqual(@as(i64, 100), gauge.get());

    gauge.add(50);
    try std.testing.expectEqual(@as(i64, 150), gauge.get());

    gauge.sub(30);
    try std.testing.expectEqual(@as(i64, 120), gauge.get());
}

test "histogram basic operations" {
    const allocator = std.testing.allocator;

    var histogram = try Histogram.init(allocator, &.{ 0.001, 0.01, 0.1 });
    defer histogram.deinit();

    try std.testing.expectEqual(@as(u64, 0), histogram.getCount());

    // Observe 500 microseconds (500_000 ns)
    histogram.observe(500_000);
    try std.testing.expectEqual(@as(u64, 1), histogram.getCount());
    try std.testing.expectEqual(@as(u64, 500_000), histogram.getSum());

    // Check buckets (cumulative)
    // 0.0005s < 0.001s, so bucket[0] = 1, bucket[1] = 1, bucket[2] = 1, +Inf = 1
    try std.testing.expectEqual(@as(u64, 1), histogram.getBucketCount(0));
    try std.testing.expectEqual(@as(u64, 1), histogram.getBucketCount(1));
    try std.testing.expectEqual(@as(u64, 1), histogram.getBucketCount(2));
    try std.testing.expectEqual(@as(u64, 1), histogram.getBucketCount(3)); // +Inf

    // Observe 5 milliseconds (5_000_000 ns)
    histogram.observe(5_000_000);
    try std.testing.expectEqual(@as(u64, 2), histogram.getCount());

    // Check buckets again
    // 0.005s > 0.001s, <= 0.01s
    // bucket[0] = 1, bucket[1] = 2, bucket[2] = 2, +Inf = 2
    try std.testing.expectEqual(@as(u64, 1), histogram.getBucketCount(0));
    try std.testing.expectEqual(@as(u64, 2), histogram.getBucketCount(1));
    try std.testing.expectEqual(@as(u64, 2), histogram.getBucketCount(2));
    try std.testing.expectEqual(@as(u64, 2), histogram.getBucketCount(3)); // +Inf

    histogram.reset();
    try std.testing.expectEqual(@as(u64, 0), histogram.getCount());
    try std.testing.expectEqual(@as(u64, 0), histogram.getSum());
}

test "histogram sum seconds" {
    const allocator = std.testing.allocator;

    var histogram = try Histogram.init(allocator, Histogram.DEFAULT_BUCKETS);
    defer histogram.deinit();

    // Observe 1 second (1_000_000_000 ns)
    histogram.observe(1_000_000_000);
    const sum_sec = histogram.getSumSeconds();
    try std.testing.expectApproxEqAbs(@as(f64, 1.0), sum_sec, 0.0001);
}
