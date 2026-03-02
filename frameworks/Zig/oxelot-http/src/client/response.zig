// HTTP Client Response
const std = @import("std");
const Allocator = std.mem.Allocator;
const Status = @import("../status.zig").Status;
const Headers = @import("../headers.zig").Headers;

/// HTTP client response
pub const Response = struct {
    status: Status,
    headers: Headers,
    body: []const u8,
    allocator: Allocator,

    // Original request info for redirect handling
    url: []const u8,

    pub fn deinit(self: *Response) void {
        if (self.body.len > 0) {
            self.allocator.free(self.body);
        }
        self.allocator.free(self.url);
        self.headers.deinit();
    }

    /// Check if response indicates success (2xx)
    pub fn isSuccess(self: *const Response) bool {
        return self.status.isSuccess();
    }

    /// Check if response is a redirect (3xx)
    pub fn isRedirect(self: *const Response) bool {
        return self.status.isRedirect();
    }

    /// Get the response body as a string
    pub fn text(self: *const Response) []const u8 {
        return self.body;
    }

    /// Parse JSON response body into a struct
    pub fn json(self: *const Response, comptime T: type) !T {
        return std.json.parseFromSlice(T, self.allocator, self.body, .{});
    }

    /// Get a specific header value
    pub fn header(self: *const Response, name: []const u8) ?[]const u8 {
        return self.headers.get(name);
    }
};

/// Parse HTTP response from raw data
/// Returns the Response and the number of bytes consumed
pub fn parseResponse(allocator: Allocator, data: []const u8, url: []const u8) !Response {
    // Find end of headers
    const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse
        return error.IncompleteResponse;

    // Parse status line: "HTTP/1.1 200 OK\r\n"
    const first_line_end = std.mem.indexOf(u8, data, "\r\n") orelse
        return error.InvalidResponse;
    const status_line = data[0..first_line_end];

    // Find version end (first space)
    const version_end = std.mem.indexOf(u8, status_line, " ") orelse
        return error.InvalidResponse;

    // Parse status code
    const after_version = status_line[version_end + 1 ..];
    const status_end = std.mem.indexOf(u8, after_version, " ") orelse after_version.len;
    const status_str = after_version[0..status_end];
    const status_code = std.fmt.parseInt(u16, status_str, 10) catch
        return error.InvalidResponse;
    const status: Status = @enumFromInt(status_code);

    // Parse headers
    var headers = Headers.init(allocator);
    errdefer headers.deinit();

    var header_data = data[first_line_end + 2 .. header_end];
    while (header_data.len > 0) {
        const line_end = std.mem.indexOf(u8, header_data, "\r\n") orelse header_data.len;
        const line = header_data[0..line_end];

        if (line.len == 0) break;

        const colon_pos = std.mem.indexOf(u8, line, ":") orelse {
            if (line_end + 2 <= header_data.len) {
                header_data = header_data[line_end + 2 ..];
            } else {
                break;
            }
            continue;
        };

        const name = std.mem.trim(u8, line[0..colon_pos], " ");
        const value = std.mem.trim(u8, line[colon_pos + 1 ..], " ");

        try headers.set(name, value);

        if (line_end + 2 <= header_data.len) {
            header_data = header_data[line_end + 2 ..];
        } else {
            break;
        }
    }

    // Extract body
    const body_start = header_end + 4;
    const body_data = data[body_start..];

    // Check for Content-Length
    var expected_body_len: ?usize = null;
    if (headers.get("Content-Length")) |cl| {
        expected_body_len = std.fmt.parseInt(usize, cl, 10) catch null;
    }

    // Check for chunked encoding
    const is_chunked = if (headers.get("Transfer-Encoding")) |te|
        std.mem.indexOf(u8, te, "chunked") != null
    else
        false;

    var body: []const u8 = "";

    if (is_chunked) {
        // Parse chunked encoding
        body = try parseChunkedBody(allocator, body_data);
    } else if (expected_body_len) |len| {
        if (body_data.len < len) {
            return error.IncompleteResponse;
        }
        body = try allocator.dupe(u8, body_data[0..len]);
    } else {
        // No Content-Length, assume body is rest of data (Connection: close)
        body = try allocator.dupe(u8, body_data);
    }

    return Response{
        .status = status,
        .headers = headers,
        .body = body,
        .allocator = allocator,
        .url = try allocator.dupe(u8, url),
    };
}

/// Parse chunked transfer encoding
fn parseChunkedBody(allocator: Allocator, data: []const u8) ![]const u8 {
    var body: std.ArrayListUnmanaged(u8) = .empty;
    errdefer body.deinit(allocator);

    var pos: usize = 0;
    while (pos < data.len) {
        // Find chunk size line
        const line_end = std.mem.indexOf(u8, data[pos..], "\r\n") orelse break;
        const size_str = std.mem.trim(u8, data[pos..][0..line_end], " \r\n");

        // Parse chunk size (hex)
        const chunk_size = std.fmt.parseInt(usize, size_str, 16) catch break;

        if (chunk_size == 0) break; // Final chunk

        pos += line_end + 2; // Skip size line + CRLF

        if (pos + chunk_size > data.len) break;

        // Append chunk data
        try body.appendSlice(allocator, data[pos..][0..chunk_size]);

        pos += chunk_size + 2; // Skip data + CRLF
    }

    return body.toOwnedSlice(allocator);
}

test "parse simple response" {
    const allocator = std.testing.allocator;
    const raw =
        "HTTP/1.1 200 OK\r\n" ++
        "Content-Type: application/json\r\n" ++
        "Content-Length: 13\r\n" ++
        "\r\n" ++
        "{\"ok\": true}";

    var response = try parseResponse(allocator, raw, "http://example.com");
    defer response.deinit();

    try std.testing.expectEqual(Status.ok, response.status);
    try std.testing.expectEqualStrings("application/json", response.headers.get("Content-Type").?);
    try std.testing.expectEqualStrings("{\"ok\": true}", response.body);
}

test "parse chunked response" {
    const allocator = std.testing.allocator;
    const raw =
        "HTTP/1.1 200 OK\r\n" ++
        "Transfer-Encoding: chunked\r\n" ++
        "\r\n" ++
        "5\r\n" ++
        "Hello\r\n" ++
        "6\r\n" ++
        " World\r\n" ++
        "0\r\n" ++
        "\r\n";

    var response = try parseResponse(allocator, raw, "http://example.com");
    defer response.deinit();

    try std.testing.expectEqual(Status.ok, response.status);
    try std.testing.expectEqualStrings("Hello World", response.body);
}

test "parse redirect response" {
    const allocator = std.testing.allocator;
    const raw =
        "HTTP/1.1 301 Moved Permanently\r\n" ++
        "Location: https://example.com/new\r\n" ++
        "Content-Length: 0\r\n" ++
        "\r\n";

    var response = try parseResponse(allocator, raw, "http://example.com");
    defer response.deinit();

    try std.testing.expectEqual(Status.moved_permanently, response.status);
    try std.testing.expect(response.isRedirect());
    try std.testing.expectEqualStrings("https://example.com/new", response.headers.get("Location").?);
}
