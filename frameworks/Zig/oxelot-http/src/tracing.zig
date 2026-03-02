// Tracing Module for oxelot-http
//
// Provides distributed tracing with:
// - OpenTelemetry-compatible spans
// - W3C Trace Context propagation
// - OTLP HTTP/JSON export
//
// Usage:
//
//   // Create an exporter
//   var exporter = tracing.Exporter.init(allocator, .{
//       .endpoint = "http://localhost:4318",
//       .service_name = "my-service",
//   });
//   defer exporter.deinit();
//
//   // Create a span
//   var span = tracing.Span.init(allocator, "GET /api/users", .server);
//   defer span.deinit();
//
//   // Add attributes
//   span.setString("http.method", "GET");
//   span.setInt("http.status_code", 200);
//
//   // End the span
//   span.end();
//
//   // Export to collector
//   exporter.addSpan(&span);
//   try exporter.flush();

const std = @import("std");

// Re-export span types
pub const span = @import("tracing/span.zig");
pub const Span = span.Span;
pub const Kind = span.Kind;
pub const Status = span.Status;
pub const Value = span.Value;
pub const Attribute = span.Attribute;
pub const Event = span.Event;

// Re-export context propagation
pub const context = @import("tracing/context.zig");
pub const TraceContext = context.TraceContext;
pub const parseTraceparent = context.parseTraceparent;
pub const formatTraceparent = context.formatTraceparent;
pub const extractFromHeaders = context.extractFromHeaders;
pub const injectToHeaders = context.injectToHeaders;

// Re-export OTLP exporter
pub const otlp = @import("tracing/otlp.zig");
pub const Exporter = otlp.Exporter;
pub const ExporterConfig = otlp.Config;

/// Create a new span
pub fn startSpan(allocator: std.mem.Allocator, name: []const u8, kind: Kind) Span {
    return Span.init(allocator, name, kind);
}

/// Create a child span
pub fn startChildSpan(allocator: std.mem.Allocator, parent: *const Span, name: []const u8, kind: Kind) Span {
    return Span.initChild(allocator, parent, name, kind);
}

/// Create a span from an incoming trace context
pub fn startSpanFromContext(
    allocator: std.mem.Allocator,
    trace_ctx: TraceContext,
    name: []const u8,
    kind: Kind,
) Span {
    var span_id: [8]u8 = undefined;
    std.crypto.random.bytes(&span_id);

    return Span.initWithIds(
        allocator,
        trace_ctx.trace_id,
        span_id,
        trace_ctx.parent_id,
        name,
        kind,
    );
}

// ============================================================================
// Tests
// ============================================================================

test "tracing module imports" {
    // Verify all submodules can be imported
    _ = span;
    _ = context;
    _ = otlp;
}

test "start span" {
    const allocator = std.testing.allocator;

    var s = startSpan(allocator, "test", .server);
    defer s.deinit();

    try std.testing.expectEqualStrings("test", s.name);
}

test "start child span" {
    const allocator = std.testing.allocator;

    var parent = startSpan(allocator, "parent", .server);
    defer parent.deinit();

    var child = startChildSpan(allocator, &parent, "child", .internal);
    defer child.deinit();

    try std.testing.expectEqualSlices(u8, &parent.trace_id, &child.trace_id);
}

test "start span from context" {
    const allocator = std.testing.allocator;

    const ctx = TraceContext.init();
    var s = startSpanFromContext(allocator, ctx, "test", .server);
    defer s.deinit();

    try std.testing.expectEqualSlices(u8, &ctx.trace_id, &s.trace_id);
    try std.testing.expectEqualSlices(u8, &ctx.parent_id, &s.parent_span_id.?);
}

test {
    _ = @import("tracing/span.zig");
    _ = @import("tracing/context.zig");
    _ = @import("tracing/otlp.zig");
}
