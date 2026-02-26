// Span - Distributed Tracing Span
//
// Represents a unit of work in a distributed trace.
// Compatible with OpenTelemetry span semantics.

const std = @import("std");
const posix = std.posix;

/// Span status
pub const Status = enum {
    unset,
    ok,
    @"error",
};

/// Span kind
pub const Kind = enum {
    internal,
    server,
    client,
    producer,
    consumer,
};

/// Attribute value types
pub const Value = union(enum) {
    string: []const u8,
    int: i64,
    float: f64,
    bool: bool,
};

/// Span event (annotation)
pub const Event = struct {
    name: []const u8,
    timestamp_ns: i128,
    attributes: ?[]const Attribute = null,
};

/// Span attribute
pub const Attribute = struct {
    key: []const u8,
    value: Value,
};

/// A distributed tracing span
pub const Span = struct {
    /// 16-byte trace ID
    trace_id: [16]u8,
    /// 8-byte span ID
    span_id: [8]u8,
    /// Parent span ID (null if root span)
    parent_span_id: ?[8]u8,
    /// Span name (e.g., "GET /api/users")
    name: []const u8,
    /// Span kind
    kind: Kind,
    /// Start time in nanoseconds since Unix epoch
    start_time_ns: i128,
    /// End time in nanoseconds since Unix epoch (null if not ended)
    end_time_ns: ?i128,
    /// Span status
    status: Status,
    /// Status message (for error status)
    status_message: ?[]const u8,
    /// Span attributes
    attributes: std.ArrayListUnmanaged(Attribute),
    /// Span events
    events: std.ArrayListUnmanaged(Event),
    /// Allocator for dynamic data
    allocator: std.mem.Allocator,

    /// Create a new root span (no parent)
    pub fn init(allocator: std.mem.Allocator, name: []const u8, kind: Kind) Span {
        var trace_id: [16]u8 = undefined;
        var span_id: [8]u8 = undefined;
        std.crypto.random.bytes(&trace_id);
        std.crypto.random.bytes(&span_id);

        return initWithIds(allocator, trace_id, span_id, null, name, kind);
    }

    /// Create a child span
    pub fn initChild(allocator: std.mem.Allocator, parent: *const Span, name: []const u8, kind: Kind) Span {
        var span_id: [8]u8 = undefined;
        std.crypto.random.bytes(&span_id);

        return initWithIds(allocator, parent.trace_id, span_id, parent.span_id, name, kind);
    }

    /// Create a span with explicit IDs
    pub fn initWithIds(
        allocator: std.mem.Allocator,
        trace_id: [16]u8,
        span_id: [8]u8,
        parent_span_id: ?[8]u8,
        name: []const u8,
        kind: Kind,
    ) Span {
        return .{
            .trace_id = trace_id,
            .span_id = span_id,
            .parent_span_id = parent_span_id,
            .name = name,
            .kind = kind,
            .start_time_ns = getTimestampNs(),
            .end_time_ns = null,
            .status = .unset,
            .status_message = null,
            .attributes = .empty,
            .events = .empty,
            .allocator = allocator,
        };
    }

    /// End the span
    pub fn end(self: *Span) void {
        if (self.end_time_ns == null) {
            self.end_time_ns = getTimestampNs();
        }
    }

    /// Set span status
    pub fn setStatus(self: *Span, status: Status, message: ?[]const u8) void {
        self.status = status;
        self.status_message = message;
    }

    /// Set a string attribute
    pub fn setString(self: *Span, key: []const u8, value: []const u8) void {
        self.attributes.append(self.allocator, .{ .key = key, .value = .{ .string = value } }) catch {};
    }

    /// Set an integer attribute
    pub fn setInt(self: *Span, key: []const u8, value: i64) void {
        self.attributes.append(self.allocator, .{ .key = key, .value = .{ .int = value } }) catch {};
    }

    /// Set a float attribute
    pub fn setFloat(self: *Span, key: []const u8, value: f64) void {
        self.attributes.append(self.allocator, .{ .key = key, .value = .{ .float = value } }) catch {};
    }

    /// Set a boolean attribute
    pub fn setBool(self: *Span, key: []const u8, value: bool) void {
        self.attributes.append(self.allocator, .{ .key = key, .value = .{ .bool = value } }) catch {};
    }

    /// Add an event to the span
    pub fn addEvent(self: *Span, name: []const u8) void {
        self.events.append(self.allocator, .{
            .name = name,
            .timestamp_ns = getTimestampNs(),
            .attributes = null,
        }) catch {};
    }

    /// Add an event with attributes
    pub fn addEventWithAttributes(self: *Span, name: []const u8, attrs: []const Attribute) void {
        self.events.append(self.allocator, .{
            .name = name,
            .timestamp_ns = getTimestampNs(),
            .attributes = attrs,
        }) catch {};
    }

    /// Get trace ID as hex string
    pub fn traceIdHex(self: *const Span, buf: *[32]u8) []const u8 {
        return std.fmt.bufPrint(buf, "{}", .{std.fmt.fmtSliceHexLower(&self.trace_id)}) catch "";
    }

    /// Get span ID as hex string
    pub fn spanIdHex(self: *const Span, buf: *[16]u8) []const u8 {
        return std.fmt.bufPrint(buf, "{}", .{std.fmt.fmtSliceHexLower(&self.span_id)}) catch "";
    }

    /// Get duration in nanoseconds (0 if not ended)
    pub fn durationNs(self: *const Span) u64 {
        if (self.end_time_ns) |end_ns| {
            if (end_ns > self.start_time_ns) {
                return @intCast(end_ns - self.start_time_ns);
            }
        }
        return 0;
    }

    /// Free allocated resources
    pub fn deinit(self: *Span) void {
        self.attributes.deinit(self.allocator);
        self.events.deinit(self.allocator);
    }
};

/// Get current timestamp in nanoseconds since Unix epoch
fn getTimestampNs() i128 {
    const ts = posix.clock_gettime(.REALTIME) catch return 0;
    return @as(i128, ts.sec) * 1_000_000_000 + ts.nsec;
}

// ============================================================================
// Tests
// ============================================================================

test "span creation" {
    const allocator = std.testing.allocator;

    var span = Span.init(allocator, "test-span", .server);
    defer span.deinit();

    try std.testing.expectEqualStrings("test-span", span.name);
    try std.testing.expectEqual(Kind.server, span.kind);
    try std.testing.expectEqual(Status.unset, span.status);
    try std.testing.expect(span.end_time_ns == null);
}

test "span child" {
    const allocator = std.testing.allocator;

    var parent = Span.init(allocator, "parent", .server);
    defer parent.deinit();

    var child = Span.initChild(allocator, &parent, "child", .internal);
    defer child.deinit();

    // Child should have same trace ID
    try std.testing.expectEqualSlices(u8, &parent.trace_id, &child.trace_id);
    // Child should have parent's span ID as parent
    try std.testing.expectEqualSlices(u8, &parent.span_id, &child.parent_span_id.?);
    // Child should have different span ID
    try std.testing.expect(!std.mem.eql(u8, &parent.span_id, &child.span_id));
}

test "span attributes" {
    const allocator = std.testing.allocator;

    var span = Span.init(allocator, "test", .server);
    defer span.deinit();

    span.setString("http.method", "GET");
    span.setInt("http.status_code", 200);
    span.setBool("http.tls", true);

    try std.testing.expectEqual(@as(usize, 3), span.attributes.items.len);
}

test "span events" {
    const allocator = std.testing.allocator;

    var span = Span.init(allocator, "test", .server);
    defer span.deinit();

    span.addEvent("started");
    span.addEvent("completed");

    try std.testing.expectEqual(@as(usize, 2), span.events.items.len);
}

test "span end" {
    const allocator = std.testing.allocator;

    var span = Span.init(allocator, "test", .server);
    defer span.deinit();

    try std.testing.expect(span.end_time_ns == null);
    span.end();
    try std.testing.expect(span.end_time_ns != null);
}
