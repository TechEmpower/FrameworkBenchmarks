// OTLP HTTP/JSON Exporter
//
// Exports spans to an OpenTelemetry collector using OTLP over HTTP/JSON.
// See: https://opentelemetry.io/docs/specs/otlp/
//
// Endpoint: POST /v1/traces
// Content-Type: application/json

const std = @import("std");
const span_mod = @import("span.zig");
const Span = span_mod.Span;
const Value = span_mod.Value;
const Attribute = span_mod.Attribute;

/// OTLP Exporter configuration
pub const Config = struct {
    /// OTLP endpoint URL (e.g., "http://localhost:4318")
    endpoint: []const u8 = "http://localhost:4318",
    /// Service name for resource attributes
    service_name: []const u8 = "oxelot-http",
    /// Service version
    service_version: []const u8 = "1.0.0",
    /// Additional resource attributes
    resource_attributes: ?[]const Attribute = null,
    /// Batch size before export
    batch_size: usize = 100,
    /// Export timeout in milliseconds
    timeout_ms: u32 = 30_000,
};

/// HTTP Client interface for sending traces
/// This allows the OTLP exporter to work with any HTTP client implementation
pub const HttpClient = struct {
    context: *anyopaque,
    postFn: *const fn (ctx: *anyopaque, url: []const u8, body: []const u8, content_type: []const u8) HttpError!HttpResponse,

    pub const HttpError = error{
        ConnectionFailed,
        RequestTimeout,
        WriteError,
        ReadError,
        InvalidResponse,
        OutOfMemory,
    };

    pub const HttpResponse = struct {
        status_code: u16,
        body: ?[]const u8,

        pub fn isSuccess(self: HttpResponse) bool {
            return self.status_code >= 200 and self.status_code < 300;
        }
    };

    pub fn post(self: HttpClient, url: []const u8, body: []const u8, content_type: []const u8) HttpError!HttpResponse {
        return self.postFn(self.context, url, body, content_type);
    }
};

/// OTLP HTTP/JSON Exporter
pub const Exporter = struct {
    allocator: std.mem.Allocator,
    config: Config,
    /// Pending spans to export
    pending: std.ArrayListUnmanaged(*Span),
    mutex: std.Thread.Mutex,
    /// Optional HTTP client for sending traces
    http_client: ?HttpClient,
    /// Full URL for the traces endpoint
    traces_url: []const u8,

    pub fn init(allocator: std.mem.Allocator, config: Config) Exporter {
        return initWithClient(allocator, config, null);
    }

    pub fn initWithClient(allocator: std.mem.Allocator, config: Config, http_client: ?HttpClient) Exporter {
        // Build the traces URL: endpoint + "/v1/traces"
        const traces_url = std.fmt.allocPrint(allocator, "{s}/v1/traces", .{config.endpoint}) catch config.endpoint;

        return .{
            .allocator = allocator,
            .config = config,
            .pending = .empty,
            .mutex = .{},
            .http_client = http_client,
            .traces_url = traces_url,
        };
    }

    /// Set the HTTP client after initialization
    pub fn setHttpClient(self: *Exporter, client: HttpClient) void {
        self.http_client = client;
    }

    pub fn deinit(self: *Exporter) void {
        self.pending.deinit(self.allocator);
        // Free traces_url if it was allocated (not the default endpoint)
        if (!std.mem.eql(u8, self.traces_url, self.config.endpoint)) {
            self.allocator.free(self.traces_url);
        }
    }

    /// Add a span to the pending batch
    pub fn addSpan(self: *Exporter, s: *Span) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        self.pending.append(self.allocator, s) catch {};

        // Auto-export when batch is full
        if (self.pending.items.len >= self.config.batch_size) {
            self.exportBatch() catch {};
        }
    }

    /// Export all pending spans
    pub fn flush(self: *Exporter) !void {
        self.mutex.lock();
        defer self.mutex.unlock();

        try self.exportBatch();
    }

    /// Export the current batch of spans
    fn exportBatch(self: *Exporter) !void {
        if (self.pending.items.len == 0) return;

        // Format spans as OTLP JSON
        const json = try self.formatOtlpJson(self.pending.items);
        defer self.allocator.free(json);

        // Clear pending spans
        self.pending.clearRetainingCapacity();

        // Send to collector if HTTP client is available
        if (self.http_client) |client| {
            const response = client.post(
                self.traces_url,
                json,
                "application/json",
            ) catch |err| {
                // Log error but don't fail - tracing should not break the application
                std.log.err("OTLP export failed: {}", .{err});
                return;
            };

            if (!response.isSuccess()) {
                std.log.warn("OTLP export returned non-success status: {d}", .{response.status_code});
            }
        }
    }

    /// Format spans as OTLP JSON
    pub fn formatOtlpJson(self: *Exporter, spans: []const *Span) ![]u8 {
        // Pre-allocate a reasonable buffer size
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        // Use fmt to build the JSON string directly into the buffer
        try self.writeJsonToBuffer(&buf, spans);

        return buf.toOwnedSlice(self.allocator);
    }

    fn writeJsonToBuffer(self: *Exporter, buf: *std.ArrayListUnmanaged(u8), spans: []const *Span) !void {
        try buf.appendSlice(self.allocator, "{\"resourceSpans\":[{");

        // Resource
        try buf.appendSlice(self.allocator, "\"resource\":{\"attributes\":[");
        try self.writeAttributeToBuf(buf, "service.name", .{ .string = self.config.service_name });
        try buf.appendSlice(self.allocator, ",");
        try self.writeAttributeToBuf(buf, "service.version", .{ .string = self.config.service_version });
        if (self.config.resource_attributes) |attrs| {
            for (attrs) |attr| {
                try buf.appendSlice(self.allocator, ",");
                try self.writeAttributeToBuf(buf, attr.key, attr.value);
            }
        }
        try buf.appendSlice(self.allocator, "]},");

        // Scope spans
        try buf.appendSlice(self.allocator, "\"scopeSpans\":[{");
        try buf.appendSlice(self.allocator, "\"scope\":{\"name\":\"oxelot-http\",\"version\":\"1.0.0\"},");
        try buf.appendSlice(self.allocator, "\"spans\":[");

        for (spans, 0..) |s, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ",");
            try self.writeSpanToBuf(buf, s);
        }

        try buf.appendSlice(self.allocator, "]}]}]}");
    }

    fn writeSpanToBuf(self: *Exporter, buf: *std.ArrayListUnmanaged(u8), s: *const Span) !void {
        try buf.appendSlice(self.allocator, "{");

        // Trace ID (hex)
        try buf.appendSlice(self.allocator, "\"traceId\":\"");
        try appendHex(buf, self.allocator, &s.trace_id);
        try buf.appendSlice(self.allocator, "\",");

        // Span ID (hex)
        try buf.appendSlice(self.allocator, "\"spanId\":\"");
        try appendHex(buf, self.allocator, &s.span_id);
        try buf.appendSlice(self.allocator, "\",");

        // Parent span ID
        if (s.parent_span_id) |pid| {
            try buf.appendSlice(self.allocator, "\"parentSpanId\":\"");
            try appendHex(buf, self.allocator, &pid);
            try buf.appendSlice(self.allocator, "\",");
        }

        // Name
        try buf.appendSlice(self.allocator, "\"name\":\"");
        try appendJsonString(buf, self.allocator, s.name);
        try buf.appendSlice(self.allocator, "\",");

        // Kind
        var kind_buf: [32]u8 = undefined;
        const kind_str = std.fmt.bufPrint(&kind_buf, "\"kind\":{d},", .{@intFromEnum(s.kind) + 1}) catch "";
        try buf.appendSlice(self.allocator, kind_str);

        // Timestamps (nanoseconds)
        var ts_buf: [64]u8 = undefined;
        const start_str = std.fmt.bufPrint(&ts_buf, "\"startTimeUnixNano\":\"{d}\",", .{s.start_time_ns}) catch "";
        try buf.appendSlice(self.allocator, start_str);
        if (s.end_time_ns) |end| {
            const end_str = std.fmt.bufPrint(&ts_buf, "\"endTimeUnixNano\":\"{d}\",", .{end}) catch "";
            try buf.appendSlice(self.allocator, end_str);
        }

        // Status
        try buf.appendSlice(self.allocator, "\"status\":{");
        var status_buf: [32]u8 = undefined;
        const status_str = std.fmt.bufPrint(&status_buf, "\"code\":{d}", .{@intFromEnum(s.status)}) catch "";
        try buf.appendSlice(self.allocator, status_str);
        if (s.status_message) |msg| {
            try buf.appendSlice(self.allocator, ",\"message\":\"");
            try appendJsonString(buf, self.allocator, msg);
            try buf.appendSlice(self.allocator, "\"");
        }
        try buf.appendSlice(self.allocator, "},");

        // Attributes
        try buf.appendSlice(self.allocator, "\"attributes\":[");
        for (s.attributes.items, 0..) |attr, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ",");
            try self.writeAttributeObjToBuf(buf, attr.key, attr.value);
        }
        try buf.appendSlice(self.allocator, "],");

        // Events
        try buf.appendSlice(self.allocator, "\"events\":[");
        for (s.events.items, 0..) |event, i| {
            if (i > 0) try buf.appendSlice(self.allocator, ",");
            try buf.appendSlice(self.allocator, "{\"name\":\"");
            try appendJsonString(buf, self.allocator, event.name);
            var event_buf: [64]u8 = undefined;
            const event_str = std.fmt.bufPrint(&event_buf, "\",\"timeUnixNano\":\"{d}\"", .{event.timestamp_ns}) catch "";
            try buf.appendSlice(self.allocator, event_str);
            try buf.appendSlice(self.allocator, "}");
        }
        try buf.appendSlice(self.allocator, "]");

        try buf.appendSlice(self.allocator, "}");
    }

    fn writeAttributeToBuf(self: *Exporter, buf: *std.ArrayListUnmanaged(u8), key: []const u8, value: Value) !void {
        try self.writeAttributeObjToBuf(buf, key, value);
    }

    fn writeAttributeObjToBuf(self: *Exporter, buf: *std.ArrayListUnmanaged(u8), key: []const u8, value: Value) !void {
        try buf.appendSlice(self.allocator, "{\"key\":\"");
        try appendJsonString(buf, self.allocator, key);
        try buf.appendSlice(self.allocator, "\",\"value\":{");

        switch (value) {
            .string => |s| {
                try buf.appendSlice(self.allocator, "\"stringValue\":\"");
                try appendJsonString(buf, self.allocator, s);
                try buf.appendSlice(self.allocator, "\"");
            },
            .int => |i| {
                var int_buf: [32]u8 = undefined;
                const int_str = std.fmt.bufPrint(&int_buf, "\"intValue\":\"{d}\"", .{i}) catch "";
                try buf.appendSlice(self.allocator, int_str);
            },
            .float => |f| {
                var float_buf: [64]u8 = undefined;
                const float_str = std.fmt.bufPrint(&float_buf, "\"doubleValue\":{d}", .{f}) catch "";
                try buf.appendSlice(self.allocator, float_str);
            },
            .bool => |b| {
                if (b) {
                    try buf.appendSlice(self.allocator, "\"boolValue\":true");
                } else {
                    try buf.appendSlice(self.allocator, "\"boolValue\":false");
                }
            },
        }

        try buf.appendSlice(self.allocator, "}}");
    }
};

fn appendHex(buf: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, bytes: []const u8) !void {
    const hex_chars = "0123456789abcdef";
    for (bytes) |b| {
        try buf.append(allocator, hex_chars[b >> 4]);
        try buf.append(allocator, hex_chars[b & 0x0f]);
    }
}

fn appendJsonString(buf: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try buf.appendSlice(allocator, "\\\""),
            '\\' => try buf.appendSlice(allocator, "\\\\"),
            '\n' => try buf.appendSlice(allocator, "\\n"),
            '\r' => try buf.appendSlice(allocator, "\\r"),
            '\t' => try buf.appendSlice(allocator, "\\t"),
            0x00...0x08, 0x0b, 0x0c, 0x0e...0x1f => {
                var escape_buf: [6]u8 = undefined;
                const escape_str = std.fmt.bufPrint(&escape_buf, "\\u{x:0>4}", .{c}) catch "";
                try buf.appendSlice(allocator, escape_str);
            },
            else => try buf.append(allocator, c),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

test "format otlp json" {
    const allocator = std.testing.allocator;

    var exporter = Exporter.init(allocator, .{});
    defer exporter.deinit();

    var span1 = Span.init(allocator, "test-span", .server);
    defer span1.deinit();
    span1.setString("http.method", "GET");
    span1.end();

    var spans = [_]*Span{&span1};
    const json = try exporter.formatOtlpJson(&spans);
    defer allocator.free(json);

    // Verify basic structure
    try std.testing.expect(std.mem.indexOf(u8, json, "\"resourceSpans\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"scopeSpans\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"name\":\"test-span\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"service.name\"") != null);
}

test "append json string escaping" {
    const allocator = std.testing.allocator;

    var buf: std.ArrayListUnmanaged(u8) = .empty;
    defer buf.deinit(allocator);

    try appendJsonString(&buf, allocator, "hello\"world\ntest");

    try std.testing.expectEqualStrings("hello\\\"world\\ntest", buf.items);
}
