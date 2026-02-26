const std = @import("std");
const Method = @import("method.zig").Method;
const Headers = @import("headers.zig").Headers;
pub const multipart = @import("multipart.zig");

pub const pico = @cImport({
    @cInclude("picohttpparser.h");
});

/// HTTP Request
pub const Request = struct {
    allocator: std.mem.Allocator,
    method: Method,
    path: []const u8,
    query_string: ?[]const u8,
    headers: Headers,
    body: ?[]const u8,
    http_version: HttpVersion,

    // Parsed path parameters (populated by router)
    path_params: std.StringHashMapUnmanaged([]const u8),

    // Raw data for zero-copy access
    raw_path: []const u8, // Full path including query string

    // Streaming multipart parser (set by server for large uploads)
    streaming_parser: ?*multipart.StreamingMultipartParser = null,

    // Raw picohttpparser headers for zero-copy fast path
    pico_headers: ?[]const pico.phr_header = null,
    owns_headers: bool = true, // false when using pico_headers (no-copy mode)

    pub const HttpVersion = enum {
        http_1_0,
        http_1_1,
    };

    pub fn init(allocator: std.mem.Allocator) Request {
        return .{
            .allocator = allocator,
            .method = .GET,
            .path = "",
            .query_string = null,
            .headers = Headers.init(allocator),
            .body = null,
            .http_version = .http_1_1,
            .path_params = .{},
            .raw_path = "",
        };
    }

    /// Initialize from pre-parsed picohttpparser results (zero-copy, no allocations).
    /// The pico_hdrs slice must remain valid for the lifetime of this Request.
    pub fn initFromParsed(
        allocator: std.mem.Allocator,
        method_str: []const u8,
        full_path: []const u8,
        minor_version: c_int,
        pico_hdrs: []const pico.phr_header,
        body_data: ?[]const u8,
    ) Request {
        var req = Request{
            .allocator = allocator,
            .method = Method.fromString(method_str) orelse .GET,
            .path = full_path,
            .query_string = null,
            .headers = Headers.init(allocator),
            .body = body_data,
            .http_version = if (minor_version == 0) .http_1_0 else .http_1_1,
            .path_params = .{},
            .raw_path = full_path,
            .pico_headers = pico_hdrs,
            .owns_headers = false,
        };

        // Split path and query string
        if (std.mem.indexOfScalar(u8, full_path, '?')) |qpos| {
            req.path = full_path[0..qpos];
            req.query_string = full_path[qpos + 1 ..];
        }

        return req;
    }

    pub fn deinit(self: *Request) void {
        if (self.owns_headers) {
            self.headers.deinit();
        }
        self.path_params.deinit(self.allocator);
    }

    /// Get a path parameter by name (e.g., :id from /users/:id)
    pub fn param(self: *const Request, name: []const u8) ?[]const u8 {
        return self.path_params.get(name);
    }

    /// Get a path parameter and parse it as a specific type
    pub fn paramAs(self: *const Request, comptime T: type, name: []const u8) ?T {
        const value = self.param(name) orelse return null;
        return switch (@typeInfo(T)) {
            .int => std.fmt.parseInt(T, value, 10) catch null,
            .float => std.fmt.parseFloat(T, value) catch null,
            else => @compileError("paramAs only supports int and float types"),
        };
    }

    /// Get a query parameter by name
    pub fn query(self: *const Request, name: []const u8) ?[]const u8 {
        const qs = self.query_string orelse return null;
        var iter = std.mem.splitScalar(u8, qs, '&');
        while (iter.next()) |pair| {
            if (std.mem.indexOfScalar(u8, pair, '=')) |eq_pos| {
                const key = pair[0..eq_pos];
                if (std.mem.eql(u8, key, name)) {
                    return pair[eq_pos + 1 ..];
                }
            } else {
                if (std.mem.eql(u8, pair, name)) {
                    return ""; // Key exists but no value
                }
            }
        }
        return null;
    }

    /// Get a header value
    pub fn header(self: *const Request, name: []const u8) ?[]const u8 {
        // Fast path: use raw pico headers (zero-copy, no hash map)
        if (self.pico_headers) |hdrs| {
            for (hdrs) |h| {
                if (std.ascii.eqlIgnoreCase(h.name[0..h.name_len], name)) {
                    return h.value[0..h.value_len];
                }
            }
            return null;
        }
        return self.headers.get(name);
    }

    /// Check if request wants to keep connection alive
    pub fn keepAlive(self: *const Request) bool {
        const conn = self.header("Connection") orelse {
            // Default based on HTTP version
            return self.http_version == .http_1_1;
        };
        return std.ascii.eqlIgnoreCase(conn, "keep-alive");
    }

    /// Get content type
    pub fn contentType(self: *const Request) ?[]const u8 {
        return self.header("Content-Type");
    }

    /// Get content length
    pub fn contentLength(self: *const Request) ?usize {
        const value = self.header("Content-Length") orelse return null;
        return std.fmt.parseInt(usize, value, 10) catch null;
    }

    /// Check if this is a JSON request
    pub fn isJson(self: *const Request) bool {
        const ct = self.contentType() orelse return false;
        return std.mem.indexOf(u8, ct, "application/json") != null;
    }

    /// Parse body as JSON into a struct (leaky - allocates strings but doesn't track them)
    pub fn json(self: *const Request, comptime T: type) !T {
        const body_data = self.body orelse return error.NoBody;
        const parsed = try std.json.parseFromSlice(T, self.allocator, body_data, .{});
        return parsed.value;
    }

    /// Parse body as JSON, returning parsed value with arena for cleanup
    pub fn jsonAlloc(self: *const Request, comptime T: type) !std.json.Parsed(T) {
        const body_data = self.body orelse return error.NoBody;
        return try std.json.parseFromSlice(T, self.allocator, body_data, .{});
    }

    /// Check if this is a form-urlencoded request
    pub fn isFormUrlEncoded(self: *const Request) bool {
        const ct = self.contentType() orelse return false;
        return std.mem.indexOf(u8, ct, "application/x-www-form-urlencoded") != null;
    }

    /// Check if this is a multipart/form-data request
    pub fn isMultipart(self: *const Request) bool {
        const ct = self.contentType() orelse return false;
        return std.mem.indexOf(u8, ct, "multipart/form-data") != null;
    }

    /// Get multipart boundary from Content-Type header
    pub fn multipartBoundary(self: *const Request) ?[]const u8 {
        const ct = self.contentType() orelse return null;
        return multipart.extractBoundary(ct);
    }

    /// Create iterator for multipart/form-data parts
    /// Returns null if not a valid multipart request or body exceeds max size
    pub fn multipartParts(self: *const Request, config: multipart.MultipartConfig) ?multipart.MultipartIterator {
        const boundary = self.multipartBoundary() orelse return null;
        const body_data = self.body orelse return null;

        // Pre-validate body size
        if (body_data.len > config.max_body_size) {
            return null;
        }

        return multipart.MultipartIterator.init(body_data, boundary, config);
    }

    /// Check if this is a streaming multipart request (large file upload)
    pub fn isStreamingMultipart(self: *const Request) bool {
        return self.streaming_parser != null;
    }

    /// Get streaming multipart parser for large file uploads
    /// Returns null if this is not a streaming multipart request
    pub fn streamingParts(self: *const Request) ?*multipart.StreamingMultipartParser {
        return self.streaming_parser;
    }

    /// Get a form field value from application/x-www-form-urlencoded body
    /// Returns the raw (still URL-encoded) value
    pub fn formField(self: *const Request, name: []const u8) ?[]const u8 {
        const body_data = self.body orelse return null;
        return getFormValue(body_data, name);
    }

    /// Get a form field value and URL-decode it
    /// Caller owns the returned memory
    pub fn formFieldDecoded(self: *const Request, name: []const u8) !?[]u8 {
        const raw_value = self.formField(name) orelse return null;
        return try urlDecode(self.allocator, raw_value);
    }

    /// Iterator for form fields
    pub fn formFields(self: *const Request) FormFieldIterator {
        return FormFieldIterator.init(self.body orelse "");
    }
};

/// Get a value from URL-encoded form data
fn getFormValue(data: []const u8, name: []const u8) ?[]const u8 {
    var iter = std.mem.splitScalar(u8, data, '&');
    while (iter.next()) |pair| {
        if (std.mem.indexOfScalar(u8, pair, '=')) |eq_pos| {
            const key = pair[0..eq_pos];
            if (std.mem.eql(u8, key, name)) {
                return pair[eq_pos + 1 ..];
            }
        } else {
            if (std.mem.eql(u8, pair, name)) {
                return ""; // Key exists but no value
            }
        }
    }
    return null;
}

/// URL decode a string (converts %XX and + to their original characters)
pub fn urlDecode(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var result = try allocator.alloc(u8, input.len);
    var write_pos: usize = 0;
    var read_pos: usize = 0;

    while (read_pos < input.len) {
        const c = input[read_pos];
        if (c == '%' and read_pos + 2 < input.len) {
            // Hex-encoded byte
            const hex = input[read_pos + 1 .. read_pos + 3];
            if (std.fmt.parseInt(u8, hex, 16)) |byte| {
                result[write_pos] = byte;
                write_pos += 1;
                read_pos += 3;
            } else |_| {
                // Invalid hex, keep literal %
                result[write_pos] = c;
                write_pos += 1;
                read_pos += 1;
            }
        } else if (c == '+') {
            // + means space in form data
            result[write_pos] = ' ';
            write_pos += 1;
            read_pos += 1;
        } else {
            result[write_pos] = c;
            write_pos += 1;
            read_pos += 1;
        }
    }

    // Shrink to actual size
    return allocator.realloc(result, write_pos);
}

/// Iterator for form fields
pub const FormFieldIterator = struct {
    data: []const u8,
    pos: usize,

    pub const Field = struct {
        name: []const u8,
        value: []const u8,
    };

    pub fn init(data: []const u8) FormFieldIterator {
        return .{ .data = data, .pos = 0 };
    }

    pub fn next(self: *FormFieldIterator) ?Field {
        if (self.pos >= self.data.len) return null;

        // Find end of this pair
        const remaining = self.data[self.pos..];
        const pair_end = std.mem.indexOfScalar(u8, remaining, '&') orelse remaining.len;
        const pair = remaining[0..pair_end];

        // Move position past this pair
        self.pos += pair_end;
        if (self.pos < self.data.len) self.pos += 1; // Skip '&'

        // Skip empty pairs
        if (pair.len == 0) return self.next();

        // Split into name=value
        if (std.mem.indexOfScalar(u8, pair, '=')) |eq_pos| {
            return .{
                .name = pair[0..eq_pos],
                .value = pair[eq_pos + 1 ..],
            };
        } else {
            return .{
                .name = pair,
                .value = "",
            };
        }
    }
};

/// Parse an HTTP request from raw bytes using picohttpparser
pub fn parseRequest(allocator: std.mem.Allocator, buffer: []const u8) !?Request {
    var method_ptr: [*c]const u8 = undefined;
    var method_len: usize = undefined;
    var path_ptr: [*c]const u8 = undefined;
    var path_len: usize = undefined;
    var minor_version: c_int = undefined;
    var headers: [64]pico.phr_header = undefined;
    var num_headers: usize = 64;

    const ret = pico.phr_parse_request(
        buffer.ptr,
        buffer.len,
        &method_ptr,
        &method_len,
        &path_ptr,
        &path_len,
        &minor_version,
        &headers,
        &num_headers,
        0,
    );

    if (ret == -2) return null; // Incomplete
    if (ret == -1) return error.ParseError;

    var request = Request.init(allocator);
    errdefer request.deinit();

    // Parse method
    const method_str = method_ptr[0..method_len];
    request.method = Method.fromString(method_str) orelse return error.InvalidMethod;

    // Parse path and query string
    const full_path = path_ptr[0..path_len];
    request.raw_path = full_path;

    if (std.mem.indexOfScalar(u8, full_path, '?')) |qpos| {
        request.path = full_path[0..qpos];
        request.query_string = full_path[qpos + 1 ..];
    } else {
        request.path = full_path;
        request.query_string = null;
    }

    // HTTP version
    request.http_version = if (minor_version == 0) .http_1_0 else .http_1_1;

    // Parse headers
    for (headers[0..num_headers]) |h| {
        const name = h.name[0..h.name_len];
        const value = h.value[0..h.value_len];
        try request.headers.set(name, value);
    }

    // Body starts after headers (ret is the offset)
    const header_end: usize = @intCast(ret);
    const remaining = buffer.len - header_end;

    if (remaining > 0) {
        // Check Content-Length to determine actual body size
        if (request.contentLength()) |content_len| {
            if (remaining >= content_len) {
                // We have the complete body
                request.body = buffer[header_end..][0..content_len];
            } else {
                // Incomplete body - clean up and return null to signal need more data
                request.deinit();
                return null;
            }
        } else {
            // No Content-Length header - take all remaining data
            // (This handles chunked encoding case or requests without body length)
            request.body = buffer[header_end..];
        }
    } else {
        // No body data yet - check if we're expecting one
        if (request.contentLength()) |content_len| {
            if (content_len > 0) {
                // Expecting body but have none - clean up and return null
                request.deinit();
                return null;
            }
        }
    }

    return request;
}

test "request parsing" {
    const allocator = std.testing.allocator;
    const raw_request =
        "GET /users/123?active=true HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Type: application/json\r\n" ++
        "\r\n";

    var req = (try parseRequest(allocator, raw_request)) orelse return error.ParseFailed;
    defer req.deinit();

    try std.testing.expectEqual(Method.GET, req.method);
    try std.testing.expectEqualStrings("/users/123", req.path);
    try std.testing.expectEqualStrings("active=true", req.query_string.?);
    try std.testing.expectEqualStrings("true", req.query("active").?);
    try std.testing.expectEqualStrings("example.com", req.header("Host").?);
}

test "request with body and Content-Length" {
    const allocator = std.testing.allocator;
    const body = "{\"name\":\"test\"}";
    const raw_request =
        "POST /api/users HTTP/1.1\r\n" ++
        "Host: example.com\r\n" ++
        "Content-Type: application/json\r\n" ++
        "Content-Length: 15\r\n" ++
        "\r\n" ++
        body;

    var req = (try parseRequest(allocator, raw_request)) orelse return error.ParseFailed;
    defer req.deinit();

    try std.testing.expectEqual(Method.POST, req.method);
    try std.testing.expectEqualStrings(body, req.body.?);
    try std.testing.expect(req.isJson());
}

test "incomplete body returns null" {
    const allocator = std.testing.allocator;
    const raw_request =
        "POST /api/users HTTP/1.1\r\n" ++
        "Content-Length: 100\r\n" ++
        "\r\n" ++
        "partial";

    const result = try parseRequest(allocator, raw_request);
    try std.testing.expect(result == null);
}

test "form field parsing" {
    const allocator = std.testing.allocator;
    const body = "name=John+Doe&email=john%40example.com&age=30";
    const raw_request =
        "POST /submit HTTP/1.1\r\n" ++
        "Content-Type: application/x-www-form-urlencoded\r\n" ++
        "Content-Length: 45\r\n" ++
        "\r\n" ++
        body;

    var req = (try parseRequest(allocator, raw_request)) orelse return error.ParseFailed;
    defer req.deinit();

    try std.testing.expect(req.isFormUrlEncoded());
    try std.testing.expectEqualStrings("John+Doe", req.formField("name").?);
    try std.testing.expectEqualStrings("30", req.formField("age").?);

    // Test URL decoding
    const decoded_name = (try req.formFieldDecoded("name")).?;
    defer allocator.free(decoded_name);
    try std.testing.expectEqualStrings("John Doe", decoded_name);

    const decoded_email = (try req.formFieldDecoded("email")).?;
    defer allocator.free(decoded_email);
    try std.testing.expectEqualStrings("john@example.com", decoded_email);
}

test "form field iterator" {
    var iter = FormFieldIterator.init("a=1&b=2&c=3");

    const f1 = iter.next().?;
    try std.testing.expectEqualStrings("a", f1.name);
    try std.testing.expectEqualStrings("1", f1.value);

    const f2 = iter.next().?;
    try std.testing.expectEqualStrings("b", f2.name);
    try std.testing.expectEqualStrings("2", f2.value);

    const f3 = iter.next().?;
    try std.testing.expectEqualStrings("c", f3.name);
    try std.testing.expectEqualStrings("3", f3.value);

    try std.testing.expect(iter.next() == null);
}

test "URL decode" {
    const allocator = std.testing.allocator;

    const decoded1 = try urlDecode(allocator, "hello%20world");
    defer allocator.free(decoded1);
    try std.testing.expectEqualStrings("hello world", decoded1);

    const decoded2 = try urlDecode(allocator, "a+b+c");
    defer allocator.free(decoded2);
    try std.testing.expectEqualStrings("a b c", decoded2);

    const decoded3 = try urlDecode(allocator, "%2F%3F%26");
    defer allocator.free(decoded3);
    try std.testing.expectEqualStrings("/?&", decoded3);
}
