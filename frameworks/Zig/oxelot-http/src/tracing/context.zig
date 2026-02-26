// W3C Trace Context Propagation
//
// Implements the W3C Trace Context specification for distributed tracing.
// See: https://www.w3.org/TR/trace-context/
//
// Header format:
//   traceparent: 00-{trace-id}-{parent-id}-{flags}
//   tracestate: vendor1=value1,vendor2=value2

const std = @import("std");
const span_mod = @import("span.zig");
const Span = span_mod.Span;

/// W3C Trace Context
pub const TraceContext = struct {
    /// Version (always 00 for current spec)
    version: u8,
    /// 16-byte trace ID
    trace_id: [16]u8,
    /// 8-byte parent span ID
    parent_id: [8]u8,
    /// Trace flags (bit 0 = sampled)
    flags: u8,

    /// Create a new trace context with random IDs
    pub fn init() TraceContext {
        var trace_id: [16]u8 = undefined;
        var parent_id: [8]u8 = undefined;
        std.crypto.random.bytes(&trace_id);
        std.crypto.random.bytes(&parent_id);

        return .{
            .version = 0,
            .trace_id = trace_id,
            .parent_id = parent_id,
            .flags = 0x01, // sampled by default
        };
    }

    /// Create a trace context from a span
    pub fn fromSpan(s: *const Span) TraceContext {
        return .{
            .version = 0,
            .trace_id = s.trace_id,
            .parent_id = s.span_id,
            .flags = 0x01, // sampled
        };
    }

    /// Check if trace is sampled
    pub fn isSampled(self: TraceContext) bool {
        return (self.flags & 0x01) != 0;
    }

    /// Set sampled flag
    pub fn setSampled(self: *TraceContext, sampled: bool) void {
        if (sampled) {
            self.flags |= 0x01;
        } else {
            self.flags &= ~@as(u8, 0x01);
        }
    }
};

/// Parse traceparent header
/// Format: 00-{32 hex trace-id}-{16 hex parent-id}-{2 hex flags}
pub fn parseTraceparent(header: []const u8) ?TraceContext {
    // Minimum length: 00-{32}-{16}-{2} = 55 characters
    if (header.len < 55) return null;

    // Check format: version-traceid-parentid-flags
    if (header[2] != '-' or header[35] != '-' or header[52] != '-') return null;

    // Parse version
    const version = parseHexByte(header[0..2]) orelse return null;
    if (version != 0) return null; // Only support version 00

    // Parse trace ID (32 hex chars = 16 bytes)
    var trace_id: [16]u8 = undefined;
    if (!parseHexBytes(header[3..35], &trace_id)) return null;

    // Check trace ID is valid (not all zeros)
    if (std.mem.eql(u8, &trace_id, &[_]u8{0} ** 16)) return null;

    // Parse parent ID (16 hex chars = 8 bytes)
    var parent_id: [8]u8 = undefined;
    if (!parseHexBytes(header[36..52], &parent_id)) return null;

    // Check parent ID is valid (not all zeros)
    if (std.mem.eql(u8, &parent_id, &[_]u8{0} ** 8)) return null;

    // Parse flags
    const flags = parseHexByte(header[53..55]) orelse return null;

    return .{
        .version = version,
        .trace_id = trace_id,
        .parent_id = parent_id,
        .flags = flags,
    };
}

/// Format traceparent header
/// Format: 00-{32 hex trace-id}-{16 hex parent-id}-{2 hex flags}
pub fn formatTraceparent(ctx: TraceContext, buf: *[55]u8) []const u8 {
    const hex = "0123456789abcdef";

    // Version
    buf[0] = '0';
    buf[1] = '0';
    buf[2] = '-';

    // Trace ID
    for (ctx.trace_id, 0..) |b, i| {
        buf[3 + i * 2] = hex[b >> 4];
        buf[3 + i * 2 + 1] = hex[b & 0x0f];
    }
    buf[35] = '-';

    // Parent ID
    for (ctx.parent_id, 0..) |b, i| {
        buf[36 + i * 2] = hex[b >> 4];
        buf[36 + i * 2 + 1] = hex[b & 0x0f];
    }
    buf[52] = '-';

    // Flags
    buf[53] = hex[ctx.flags >> 4];
    buf[54] = hex[ctx.flags & 0x0f];

    return buf;
}

/// Extract trace context from HTTP headers
/// Looks for 'traceparent' header (case-insensitive)
pub fn extractFromHeaders(comptime Headers: type, headers: *const Headers) ?TraceContext {
    // Try to get traceparent header
    const traceparent = headers.get("traceparent") orelse
        headers.get("Traceparent") orelse
        return null;

    return parseTraceparent(traceparent);
}

/// Inject trace context into HTTP headers
pub fn injectToHeaders(comptime Headers: type, ctx: TraceContext, headers: *Headers) void {
    var buf: [55]u8 = undefined;
    const traceparent = formatTraceparent(ctx, &buf);
    headers.set("traceparent", traceparent) catch {};
}

/// Parse a single hex byte (2 characters)
fn parseHexByte(chars: *const [2]u8) ?u8 {
    const high = hexDigit(chars[0]) orelse return null;
    const low = hexDigit(chars[1]) orelse return null;
    return (high << 4) | low;
}

/// Parse hex string into bytes
fn parseHexBytes(hex: []const u8, out: []u8) bool {
    if (hex.len != out.len * 2) return false;

    for (out, 0..) |*byte, i| {
        byte.* = parseHexByte(hex[i * 2 ..][0..2]) orelse return false;
    }
    return true;
}

/// Convert hex character to value
fn hexDigit(c: u8) ?u8 {
    if (c >= '0' and c <= '9') return c - '0';
    if (c >= 'a' and c <= 'f') return c - 'a' + 10;
    if (c >= 'A' and c <= 'F') return c - 'A' + 10;
    return null;
}

// ============================================================================
// Tests
// ============================================================================

test "parse valid traceparent" {
    const header = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01";
    const ctx = parseTraceparent(header) orelse return error.ParseFailed;

    try std.testing.expectEqual(@as(u8, 0), ctx.version);
    try std.testing.expectEqual(@as(u8, 0x01), ctx.flags);
    try std.testing.expect(ctx.isSampled());
}

test "parse invalid traceparent - wrong length" {
    const header = "00-abc-def-01";
    try std.testing.expect(parseTraceparent(header) == null);
}

test "parse invalid traceparent - all zeros trace id" {
    const header = "00-00000000000000000000000000000000-b7ad6b7169203331-01";
    try std.testing.expect(parseTraceparent(header) == null);
}

test "format traceparent" {
    const ctx = TraceContext{
        .version = 0,
        .trace_id = .{ 0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c },
        .parent_id = .{ 0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31 },
        .flags = 0x01,
    };

    var buf: [55]u8 = undefined;
    const header = formatTraceparent(ctx, &buf);

    try std.testing.expectEqualStrings("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01", header);
}

test "roundtrip traceparent" {
    const original = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01";
    const ctx = parseTraceparent(original) orelse return error.ParseFailed;

    var buf: [55]u8 = undefined;
    const formatted = formatTraceparent(ctx, &buf);

    try std.testing.expectEqualStrings(original, formatted);
}

test "trace context init" {
    const ctx = TraceContext.init();
    try std.testing.expectEqual(@as(u8, 0), ctx.version);
    try std.testing.expect(ctx.isSampled());
}
