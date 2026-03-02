// Prometheus Exporter
//
// Formats metrics in Prometheus text exposition format.
// See: https://prometheus.io/docs/instrumenting/exposition_formats/

const std = @import("std");
const registry = @import("registry.zig");
const types = @import("types.zig");

const Registry = registry.Registry;
const LabeledCounter = registry.LabeledCounter;
const LabeledHistogram = registry.LabeledHistogram;
const Gauge = types.Gauge;
const Counter = types.Counter;
const Histogram = types.Histogram;
const LabelKey = registry.LabelKey;

/// Format all metrics in the registry to Prometheus text format
pub fn format(reg: *Registry, writer: anytype) !void {
    // HTTP server metrics
    try formatLabeledCounter(writer, &reg.http_requests_total);
    try formatLabeledHistogram(writer, &reg.http_request_duration);
    try formatGauge(writer, "http_requests_in_flight", "Number of HTTP requests currently being processed", &reg.http_requests_in_flight);

    // HTTP client metrics
    try formatLabeledCounter(writer, &reg.http_client_requests_total);
    try formatLabeledHistogram(writer, &reg.http_client_request_duration);
    try formatGauge(writer, "http_client_pool_connections", "Total HTTP client pool connections", &reg.http_client_pool_connections);
    try formatGauge(writer, "http_client_pool_in_use", "HTTP client pool connections in use", &reg.http_client_pool_in_use);

    // PostgreSQL metrics
    try formatGauge(writer, "pg_pool_connections_total", "Total PostgreSQL pool connections", &reg.pg_pool_connections_total);
    try formatGauge(writer, "pg_pool_connections_in_use", "PostgreSQL pool connections in use", &reg.pg_pool_connections_in_use);
    try formatLabeledCounter(writer, &reg.pg_query_total);
    try formatLabeledHistogram(writer, &reg.pg_query_duration);

    // Process metrics
    try formatGauge(writer, "process_memory_bytes", "Process memory usage in bytes", &reg.process_memory_bytes);
}

fn formatGauge(writer: anytype, name: []const u8, help: []const u8, gauge: *const Gauge) !void {
    try writer.print("# HELP {s} {s}\n", .{ name, help });
    try writer.print("# TYPE {s} gauge\n", .{name});
    try writer.print("{s} {d}\n", .{ name, gauge.get() });
}

fn formatLabeledCounter(writer: anytype, counter: *LabeledCounter) !void {
    try writer.print("# HELP {s} {s}\n", .{ counter.name, counter.help });
    try writer.print("# TYPE {s} counter\n", .{counter.name});

    var iter = counter.metrics.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*;
        const value = entry.value_ptr.*.get();

        try writer.print("{s}{{", .{counter.name});
        for (0..key.len) |i| {
            if (i > 0) try writer.writeAll(",");
            try writer.print("{s}=\"{s}\"", .{ counter.label_names[i], key.values[i] });
        }
        try writer.print("}} {d}\n", .{value});
    }
}

fn formatLabeledHistogram(writer: anytype, histogram: *LabeledHistogram) !void {
    try writer.print("# HELP {s} {s}\n", .{ histogram.name, histogram.help });
    try writer.print("# TYPE {s} histogram\n", .{histogram.name});

    const buckets = histogram.buckets orelse Histogram.DEFAULT_BUCKETS;

    var iter = histogram.metrics.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*;
        const hist = entry.value_ptr.*;

        // Format bucket counts
        for (buckets, 0..) |bucket, i| {
            try writer.print("{s}_bucket{{", .{histogram.name});
            for (0..key.len) |j| {
                if (j > 0) try writer.writeAll(",");
                try writer.print("{s}=\"{s}\"", .{ histogram.label_names[j], key.values[j] });
            }
            if (key.len > 0) try writer.writeAll(",");
            try writer.print("le=\"{d}\"}} {d}\n", .{ bucket, hist.getBucketCount(i) });
        }

        // +Inf bucket
        try writer.print("{s}_bucket{{", .{histogram.name});
        for (0..key.len) |j| {
            if (j > 0) try writer.writeAll(",");
            try writer.print("{s}=\"{s}\"", .{ histogram.label_names[j], key.values[j] });
        }
        if (key.len > 0) try writer.writeAll(",");
        try writer.print("le=\"+Inf\"}} {d}\n", .{hist.getBucketCount(buckets.len)});

        // Sum
        try writer.print("{s}_sum{{", .{histogram.name});
        for (0..key.len) |j| {
            if (j > 0) try writer.writeAll(",");
            try writer.print("{s}=\"{s}\"", .{ histogram.label_names[j], key.values[j] });
        }
        try writer.print("}} {d}\n", .{hist.getSumSeconds()});

        // Count
        try writer.print("{s}_count{{", .{histogram.name});
        for (0..key.len) |j| {
            if (j > 0) try writer.writeAll(",");
            try writer.print("{s}=\"{s}\"", .{ histogram.label_names[j], key.values[j] });
        }
        try writer.print("}} {d}\n", .{hist.getCount()});
    }
}

// ============================================================================
// Tests
// ============================================================================

test "format gauge" {
    var gauge = Gauge.init();
    gauge.set(42);

    var buffer: [256]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buffer);
    try formatGauge(&writer, "test_gauge", "A test gauge", &gauge);

    const output = buffer[0..writer.end];
    try std.testing.expect(std.mem.indexOf(u8, output, "# HELP test_gauge A test gauge") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "# TYPE test_gauge gauge") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "test_gauge 42") != null);
}

test "format labeled counter" {
    const allocator = std.testing.allocator;

    var counter = LabeledCounter.init(
        allocator,
        "http_requests_total",
        "Total HTTP requests",
        &.{ "method", "path" },
        null,
    );
    defer counter.deinit();

    counter.inc(.{ "GET", "/api" });
    counter.inc(.{ "GET", "/api" });
    counter.inc(.{ "POST", "/api" });

    var buffer: [4096]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buffer);
    try formatLabeledCounter(&writer, &counter);

    const output = buffer[0..writer.end];
    try std.testing.expect(std.mem.indexOf(u8, output, "# HELP http_requests_total Total HTTP requests") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "# TYPE http_requests_total counter") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "method=\"GET\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "path=\"/api\"") != null);
}

test "format labeled histogram" {
    const allocator = std.testing.allocator;

    var histogram = LabeledHistogram.init(
        allocator,
        "http_request_duration_seconds",
        "HTTP request latency",
        &.{"method"},
        &.{ 0.001, 0.01, 0.1 },
    );
    defer histogram.deinit();

    histogram.observe(.{"GET"}, 500_000); // 0.5ms

    var buffer: [4096]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buffer);
    try formatLabeledHistogram(&writer, &histogram);

    const output = buffer[0..writer.end];
    try std.testing.expect(std.mem.indexOf(u8, output, "# HELP http_request_duration_seconds") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "# TYPE http_request_duration_seconds histogram") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "_bucket{") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "le=\"+Inf\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "_sum{") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "_count{") != null);
}

test "format registry" {
    const allocator = std.testing.allocator;

    var reg = Registry.init(allocator);
    defer reg.deinit();

    reg.http_requests_total.inc(.{ "GET", "/", "200" });
    reg.http_requests_in_flight.set(5);

    var buffer: [8192]u8 = undefined;
    var writer: std.Io.Writer = .fixed(&buffer);
    try format(&reg, &writer);

    const output = buffer[0..writer.end];
    try std.testing.expect(std.mem.indexOf(u8, output, "http_requests_total") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "http_requests_in_flight 5") != null);
}
