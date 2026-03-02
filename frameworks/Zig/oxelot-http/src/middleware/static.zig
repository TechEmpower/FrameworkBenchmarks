// Static file serving middleware for oxelot-http
//
// Features:
// - MIME type detection based on file extension
// - ETag and Last-Modified cache headers
// - Range requests (partial content / resumable downloads)
// - Directory index (index.html)
// - Security: prevents directory traversal attacks

const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;
const mw = @import("../middleware.zig");
const router = @import("../router.zig");
const Status = @import("../status.zig").Status;

// POSIX file wrapper for Zig 0.16 compatibility
const PosixFile = struct {
    fd: posix.fd_t,

    fn open(path: []const u8) !PosixFile {
        const path_z = try posix.toPosixPath(path);
        const fd = posix.openat(posix.AT.FDCWD, &path_z, .{ .ACCMODE = .RDONLY }, 0) catch |err| {
            return switch (err) {
                error.IsDir => error.IsDir,
                else => error.FileNotFound,
            };
        };
        return .{ .fd = fd };
    }

    fn close(self: PosixFile) void {
        posix.close(self.fd);
    }

    fn stat(self: PosixFile) !struct { size: u64, mtime: i64 } {
        // Get file size using lseek
        const SEEK_END = 2;
        const SEEK_CUR = 1;
        const cur_pos = linux.lseek(self.fd, 0, SEEK_CUR);
        const end_pos = linux.lseek(self.fd, 0, SEEK_END);
        if (@as(isize, @bitCast(end_pos)) < 0) {
            return error.StatFailed;
        }
        // Restore position
        _ = linux.lseek(self.fd, @bitCast(cur_pos), 0);
        // Note: mtime not available without fstat, return 0 for now
        return .{
            .size = end_pos,
            .mtime = 0,
        };
    }

    fn seekTo(self: PosixFile, pos: u64) !void {
        const SEEK_SET = 0;
        const result = linux.lseek(self.fd, @intCast(pos), SEEK_SET);
        if (@as(isize, @bitCast(result)) < 0) {
            return error.SeekFailed;
        }
    }

    fn read(self: PosixFile, buffer: []u8) !usize {
        return posix.read(self.fd, buffer);
    }
};

/// Configuration for static file middleware
pub const Config = struct {
    /// Root directory to serve files from (absolute path)
    root: []const u8,
    /// URL prefix to strip before mapping to filesystem (e.g., "/static")
    prefix: []const u8 = "",
    /// Index file to serve for directory requests
    index: []const u8 = "index.html",
    /// Enable directory listing (security risk, disabled by default)
    directory_listing: bool = false,
    /// Maximum file size to serve (default 100MB)
    max_file_size: usize = 100 * 1024 * 1024,
    /// Enable ETag header for caching
    etag: bool = true,
    /// Enable Last-Modified header for caching
    last_modified: bool = true,
    /// Cache-Control max-age in seconds (0 = no cache header)
    max_age: u32 = 3600,
    /// Custom headers to add to all responses
    headers: []const struct { name: []const u8, value: []const u8 } = &.{},
};

/// MIME type mapping based on file extension
const MimeType = struct {
    ext: []const u8,
    mime: []const u8,
};

/// Common MIME types
const mime_types = [_]MimeType{
    // Text
    .{ .ext = "html", .mime = "text/html; charset=utf-8" },
    .{ .ext = "htm", .mime = "text/html; charset=utf-8" },
    .{ .ext = "css", .mime = "text/css; charset=utf-8" },
    .{ .ext = "js", .mime = "application/javascript; charset=utf-8" },
    .{ .ext = "mjs", .mime = "application/javascript; charset=utf-8" },
    .{ .ext = "json", .mime = "application/json; charset=utf-8" },
    .{ .ext = "xml", .mime = "application/xml; charset=utf-8" },
    .{ .ext = "txt", .mime = "text/plain; charset=utf-8" },
    .{ .ext = "md", .mime = "text/markdown; charset=utf-8" },
    .{ .ext = "csv", .mime = "text/csv; charset=utf-8" },

    // Images
    .{ .ext = "png", .mime = "image/png" },
    .{ .ext = "jpg", .mime = "image/jpeg" },
    .{ .ext = "jpeg", .mime = "image/jpeg" },
    .{ .ext = "gif", .mime = "image/gif" },
    .{ .ext = "webp", .mime = "image/webp" },
    .{ .ext = "svg", .mime = "image/svg+xml" },
    .{ .ext = "ico", .mime = "image/x-icon" },
    .{ .ext = "avif", .mime = "image/avif" },

    // Fonts
    .{ .ext = "woff", .mime = "font/woff" },
    .{ .ext = "woff2", .mime = "font/woff2" },
    .{ .ext = "ttf", .mime = "font/ttf" },
    .{ .ext = "otf", .mime = "font/otf" },
    .{ .ext = "eot", .mime = "application/vnd.ms-fontobject" },

    // Audio/Video
    .{ .ext = "mp3", .mime = "audio/mpeg" },
    .{ .ext = "wav", .mime = "audio/wav" },
    .{ .ext = "ogg", .mime = "audio/ogg" },
    .{ .ext = "mp4", .mime = "video/mp4" },
    .{ .ext = "webm", .mime = "video/webm" },
    .{ .ext = "avi", .mime = "video/x-msvideo" },

    // Documents
    .{ .ext = "pdf", .mime = "application/pdf" },
    .{ .ext = "zip", .mime = "application/zip" },
    .{ .ext = "gz", .mime = "application/gzip" },
    .{ .ext = "tar", .mime = "application/x-tar" },

    // WebAssembly
    .{ .ext = "wasm", .mime = "application/wasm" },

    // Source maps
    .{ .ext = "map", .mime = "application/json" },
};

/// Get MIME type for a file extension
pub fn getMimeType(path: []const u8) []const u8 {
    // Find the last dot in the path
    const dot_pos = std.mem.lastIndexOfScalar(u8, path, '.') orelse return "application/octet-stream";
    const ext = path[dot_pos + 1 ..];

    // Look up in mime_types table
    for (mime_types) |entry| {
        if (std.ascii.eqlIgnoreCase(ext, entry.ext)) {
            return entry.mime;
        }
    }

    return "application/octet-stream";
}

/// Parse Range header value
/// Returns start and optional end byte positions
fn parseRangeHeader(range_header: []const u8, file_size: u64) ?struct { start: u64, end: u64 } {
    // Format: bytes=start-end or bytes=start- or bytes=-suffix
    if (!std.mem.startsWith(u8, range_header, "bytes=")) return null;

    const range_spec = range_header[6..];

    // Handle suffix range (bytes=-500 means last 500 bytes)
    if (range_spec.len > 0 and range_spec[0] == '-') {
        const suffix = std.fmt.parseInt(u64, range_spec[1..], 10) catch return null;
        if (suffix > file_size) return null;
        return .{ .start = file_size - suffix, .end = file_size - 1 };
    }

    // Handle start-end or start-
    const dash_pos = std.mem.indexOfScalar(u8, range_spec, '-') orelse return null;
    const start_str = range_spec[0..dash_pos];
    const end_str = range_spec[dash_pos + 1 ..];

    const start = std.fmt.parseInt(u64, start_str, 10) catch return null;
    if (start >= file_size) return null;

    const end = if (end_str.len == 0)
        file_size - 1
    else
        std.fmt.parseInt(u64, end_str, 10) catch return null;

    if (end >= file_size or end < start) return null;

    return .{ .start = start, .end = end };
}

/// Format HTTP date from timestamp
fn formatHttpDate(buf: *[29]u8, timestamp: i64) void {
    const day_names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
    const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    const epoch_secs: std.time.epoch.EpochSeconds = .{ .secs = @intCast(timestamp) };
    const day_secs = epoch_secs.getDaySeconds();
    const epoch_day = epoch_secs.getEpochDay();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const wday_idx: usize = @intCast(@mod(epoch_day.day + 4, 7));

    @memcpy(buf[0..3], day_names[wday_idx]);
    buf[3] = ',';
    buf[4] = ' ';
    const day_num = month_day.day_index + 1;
    buf[5] = '0' + @as(u8, @intCast(day_num / 10));
    buf[6] = '0' + @as(u8, @intCast(day_num % 10));
    buf[7] = ' ';
    @memcpy(buf[8..11], month_names[@intFromEnum(month_day.month) - 1]);
    buf[11] = ' ';

    const year = year_day.year;
    buf[12] = '0' + @as(u8, @intCast(@divFloor(year, 1000)));
    buf[13] = '0' + @as(u8, @intCast(@mod(@divFloor(year, 100), 10)));
    buf[14] = '0' + @as(u8, @intCast(@mod(@divFloor(year, 10), 10)));
    buf[15] = '0' + @as(u8, @intCast(@mod(year, 10)));
    buf[16] = ' ';

    const hours = day_secs.getHoursIntoDay();
    const mins = day_secs.getMinutesIntoHour();
    const secs = day_secs.getSecondsIntoMinute();
    buf[17] = '0' + @as(u8, @intCast(hours / 10));
    buf[18] = '0' + @as(u8, @intCast(hours % 10));
    buf[19] = ':';
    buf[20] = '0' + @as(u8, @intCast(mins / 10));
    buf[21] = '0' + @as(u8, @intCast(mins % 10));
    buf[22] = ':';
    buf[23] = '0' + @as(u8, @intCast(secs / 10));
    buf[24] = '0' + @as(u8, @intCast(secs % 10));
    buf[25] = ' ';
    buf[26] = 'G';
    buf[27] = 'M';
    buf[28] = 'T';
}

/// Generate ETag from file metadata
fn generateETag(buf: *[32]u8, size: u64, mtime: i64) []const u8 {
    // ETag format: "size-mtime" in hex
    const written = std.fmt.bufPrint(buf, "\"{x}-{x}\"", .{ size, @as(u64, @bitCast(mtime)) }) catch return "\"0\"";
    return written;
}

/// Static file middleware that serves files from a directory
pub fn create(config: Config) StaticMiddleware {
    return .{ .config = config };
}

/// Stateful static file middleware
pub const StaticMiddleware = struct {
    config: Config,

    /// Get the middleware function
    pub fn middleware(self: *const StaticMiddleware) mw.Middleware {
        _ = self;
        // Return a simple passthrough for now - actual serving happens via handler
        return passthrough;
    }

    /// Create a handler that serves static files
    /// Use this with router.get("/*", static.handler())
    pub fn handler(self: *const StaticMiddleware) router.Handler {
        _ = self;
        return serveStatic;
    }
};

/// Passthrough middleware - just calls next
fn passthrough(ctx: *mw.Context, next: mw.Next) anyerror!void {
    return next.call(ctx);
}

/// Thread-local config pointer for handler
threadlocal var tl_config: ?*const Config = null;

/// Set the config for the current thread
pub fn setConfig(config: *const Config) void {
    tl_config = config;
}

/// Handler function that serves static files
fn serveStatic(opaque_ctx: *mw.Context) anyerror!void {
    const ctx: *router.Context = @ptrCast(@alignCast(opaque_ctx));
    const config = tl_config orelse {
        _ = ctx.response.notFound();
        try ctx.response.body.appendSlice(ctx.allocator, "Static file serving not configured");
        return;
    };

    try serveStaticWithConfig(ctx, config.*);
}

/// Serve a static file with the given configuration
pub fn serveStaticWithConfig(ctx: *router.Context, config: Config) !void {
    const request_path = ctx.request.path;

    // Strip prefix if configured
    const rel_path = if (config.prefix.len > 0 and std.mem.startsWith(u8, request_path, config.prefix))
        request_path[config.prefix.len..]
    else
        request_path;

    // Normalize path and check for directory traversal
    const normalized = try normalizePath(ctx.allocator, rel_path);
    defer ctx.allocator.free(normalized);

    // Build full filesystem path
    const full_path = try std.fs.path.join(ctx.allocator, &.{ config.root, normalized });
    defer ctx.allocator.free(full_path);

    // Try to open the file
    const file = PosixFile.open(full_path) catch |err| {
        if (err == error.IsDir) {
            // Try index file
            const index_path = try std.fs.path.join(ctx.allocator, &.{ full_path, config.index });
            defer ctx.allocator.free(index_path);

            const index_file = PosixFile.open(index_path) catch {
                _ = ctx.response.notFound();
                try ctx.response.body.appendSlice(ctx.allocator, "Not Found");
                return;
            };
            defer index_file.close();

            return serveFile(ctx, index_file, index_path, config);
        }

        _ = ctx.response.notFound();
        try ctx.response.body.appendSlice(ctx.allocator, "Not Found");
        return;
    };
    defer file.close();

    return serveFile(ctx, file, full_path, config);
}

/// Serve an open file
fn serveFile(ctx: *router.Context, file: PosixFile, path: []const u8, config: Config) !void {
    // Get file stats
    const stat = file.stat() catch {
        _ = ctx.response.internalServerError();
        try ctx.response.body.appendSlice(ctx.allocator, "Internal Server Error");
        return;
    };

    const file_size = stat.size;
    const mtime: i64 = stat.mtime;

    // Check file size limit
    if (file_size > config.max_file_size) {
        _ = ctx.response.setStatus(.payload_too_large);
        try ctx.response.body.appendSlice(ctx.allocator, "File Too Large");
        return;
    }

    // Generate ETag and Last-Modified
    var etag_buf: [32]u8 = undefined;
    const etag = generateETag(&etag_buf, file_size, mtime);

    var date_buf: [29]u8 = undefined;
    formatHttpDate(&date_buf, mtime);
    const last_modified = &date_buf;

    // Check If-None-Match (ETag)
    if (config.etag) {
        if (ctx.request.headers.get("If-None-Match")) |client_etag| {
            if (std.mem.eql(u8, client_etag, etag)) {
                _ = ctx.response.setStatus(.not_modified);
                return;
            }
        }
    }

    // Check If-Modified-Since
    if (config.last_modified) {
        if (ctx.request.headers.get("If-Modified-Since")) |client_date| {
            if (std.mem.eql(u8, client_date, last_modified)) {
                _ = ctx.response.setStatus(.not_modified);
                return;
            }
        }
    }

    // Set content type
    const mime_type = getMimeType(path);
    ctx.response.headers.set("Content-Type", mime_type) catch {};

    // Set cache headers
    if (config.etag) {
        ctx.response.headers.set("ETag", etag) catch {};
    }
    if (config.last_modified) {
        ctx.response.headers.set("Last-Modified", last_modified) catch {};
    }
    if (config.max_age > 0) {
        var cache_buf: [64]u8 = undefined;
        const cache_control = std.fmt.bufPrint(&cache_buf, "public, max-age={d}", .{config.max_age}) catch "public, max-age=3600";
        ctx.response.headers.set("Cache-Control", cache_control) catch {};
    }

    // Add Accept-Ranges header to indicate range support
    ctx.response.headers.set("Accept-Ranges", "bytes") catch {};

    // Add custom headers
    for (config.headers) |h| {
        ctx.response.headers.set(h.name, h.value) catch {};
    }

    // Handle Range request
    if (ctx.request.headers.get("Range")) |range_header| {
        if (parseRangeHeader(range_header, file_size)) |range| {
            return servePartialContent(ctx, file, file_size, range.start, range.end);
        } else {
            // Invalid range
            _ = ctx.response.setStatus(.range_not_satisfiable);
            var content_range_buf: [64]u8 = undefined;
            const content_range = std.fmt.bufPrint(&content_range_buf, "bytes */{d}", .{file_size}) catch "";
            ctx.response.headers.set("Content-Range", content_range) catch {};
            return;
        }
    }

    // Serve full file
    _ = ctx.response.ok();

    // Read file into response body
    try ctx.response.body.ensureTotalCapacity(ctx.allocator, file_size);

    const read_buf = ctx.response.body.allocatedSlice()[ctx.response.body.items.len..];
    var total_read: usize = 0;
    while (total_read < file_size) {
        const n = file.read(read_buf[total_read..]) catch {
            _ = ctx.response.internalServerError();
            ctx.response.body.clearRetainingCapacity();
            try ctx.response.body.appendSlice(ctx.allocator, "Error reading file");
            return;
        };
        if (n == 0) break;
        total_read += n;
    }
    ctx.response.body.items.len += total_read;
}

/// Serve partial content (Range request)
fn servePartialContent(ctx: *router.Context, file: PosixFile, file_size: u64, start: u64, end: u64) !void {
    const content_length = end - start + 1;

    _ = ctx.response.setStatus(.partial_content);

    // Set Content-Range header
    var content_range_buf: [64]u8 = undefined;
    const content_range = std.fmt.bufPrint(&content_range_buf, "bytes {d}-{d}/{d}", .{ start, end, file_size }) catch "";
    ctx.response.headers.set("Content-Range", content_range) catch {};

    // Seek to start position
    file.seekTo(start) catch {
        _ = ctx.response.internalServerError();
        try ctx.response.body.appendSlice(ctx.allocator, "Error seeking file");
        return;
    };

    // Read partial content
    try ctx.response.body.ensureTotalCapacity(ctx.allocator, content_length);

    const read_buf = ctx.response.body.allocatedSlice()[ctx.response.body.items.len..];
    var total_read: usize = 0;
    while (total_read < content_length) {
        const to_read = @min(read_buf.len - total_read, content_length - total_read);
        const n = file.read(read_buf[total_read..][0..to_read]) catch {
            _ = ctx.response.internalServerError();
            ctx.response.body.clearRetainingCapacity();
            try ctx.response.body.appendSlice(ctx.allocator, "Error reading file");
            return;
        };
        if (n == 0) break;
        total_read += n;
    }
    ctx.response.body.items.len += total_read;
}

/// Normalize a path and prevent directory traversal
fn normalizePath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    // Remove leading slashes
    var start: usize = 0;
    while (start < path.len and path[start] == '/') : (start += 1) {}

    const trimmed = path[start..];
    if (trimmed.len == 0) return try allocator.dupe(u8, "");

    // Split path and rebuild, skipping ".." components
    var result: std.ArrayListUnmanaged(u8) = .empty;
    errdefer result.deinit(allocator);

    var it = std.mem.splitScalar(u8, trimmed, '/');
    var first = true;
    while (it.next()) |component| {
        // Skip empty components and current directory references
        if (component.len == 0 or std.mem.eql(u8, component, ".")) continue;

        // Reject any attempt to go up directories
        if (std.mem.eql(u8, component, "..")) {
            return error.InvalidPath;
        }

        // Add separator if not first component
        if (!first) {
            try result.append(allocator, '/');
        }
        first = false;

        try result.appendSlice(allocator, component);
    }

    return result.toOwnedSlice(allocator);
}

/// Simple handler factory that creates a static file handler
/// Usage: router.get("/static/*", static.serve("/var/www/public", .{}))
pub fn serve(root: []const u8, options: struct {
    prefix: []const u8 = "",
    index: []const u8 = "index.html",
    max_age: u32 = 3600,
}) router.Handler {
    // Store config in a static location
    const S = struct {
        var config: Config = undefined;
        var initialized = false;
    };

    if (!S.initialized) {
        S.config = .{
            .root = root,
            .prefix = options.prefix,
            .index = options.index,
            .max_age = options.max_age,
        };
        S.initialized = true;
    }

    return struct {
        fn handler(ctx: *router.Context) anyerror!void {
            return serveStaticWithConfig(ctx, S.config);
        }
    }.handler;
}

test "getMimeType" {
    const testing = std.testing;

    try testing.expectEqualStrings("text/html; charset=utf-8", getMimeType("index.html"));
    try testing.expectEqualStrings("text/css; charset=utf-8", getMimeType("style.css"));
    try testing.expectEqualStrings("application/javascript; charset=utf-8", getMimeType("app.js"));
    try testing.expectEqualStrings("image/png", getMimeType("logo.png"));
    try testing.expectEqualStrings("image/jpeg", getMimeType("photo.jpg"));
    try testing.expectEqualStrings("application/json; charset=utf-8", getMimeType("data.json"));
    try testing.expectEqualStrings("application/octet-stream", getMimeType("file.unknown"));
    try testing.expectEqualStrings("application/octet-stream", getMimeType("noext"));

    // Case insensitive
    try testing.expectEqualStrings("text/html; charset=utf-8", getMimeType("INDEX.HTML"));
    try testing.expectEqualStrings("image/jpeg", getMimeType("photo.JPEG"));
}

test "parseRangeHeader" {
    const testing = std.testing;
    const file_size: u64 = 1000;

    // Normal range
    {
        const range = parseRangeHeader("bytes=0-499", file_size).?;
        try testing.expectEqual(@as(u64, 0), range.start);
        try testing.expectEqual(@as(u64, 499), range.end);
    }

    // Open-ended range
    {
        const range = parseRangeHeader("bytes=500-", file_size).?;
        try testing.expectEqual(@as(u64, 500), range.start);
        try testing.expectEqual(@as(u64, 999), range.end);
    }

    // Suffix range
    {
        const range = parseRangeHeader("bytes=-100", file_size).?;
        try testing.expectEqual(@as(u64, 900), range.start);
        try testing.expectEqual(@as(u64, 999), range.end);
    }

    // Invalid ranges
    try testing.expect(parseRangeHeader("bytes=1000-", file_size) == null); // start >= size
    try testing.expect(parseRangeHeader("bytes=500-400", file_size) == null); // end < start
    try testing.expect(parseRangeHeader("invalid", file_size) == null); // no bytes= prefix
}

test "normalizePath" {
    const testing = std.testing;
    const allocator = testing.allocator;

    {
        const result = try normalizePath(allocator, "/foo/bar/baz.txt");
        defer allocator.free(result);
        try testing.expectEqualStrings("foo/bar/baz.txt", result);
    }

    {
        const result = try normalizePath(allocator, "///multiple///slashes///");
        defer allocator.free(result);
        try testing.expectEqualStrings("multiple/slashes", result);
    }

    {
        const result = try normalizePath(allocator, "/./current/./dir/./");
        defer allocator.free(result);
        try testing.expectEqualStrings("current/dir", result);
    }

    // Directory traversal should fail
    try testing.expectError(error.InvalidPath, normalizePath(allocator, "/../etc/passwd"));
    try testing.expectError(error.InvalidPath, normalizePath(allocator, "/foo/../../bar"));

    {
        const result = try normalizePath(allocator, "");
        defer allocator.free(result);
        try testing.expectEqualStrings("", result);
    }
}
