const std = @import("std");
const posix = std.posix;
const linux = std.os.linux;

// Zig 0.16 compatibility: Use posix-level file operations
// instead of std.Io.Dir/File which require async Io context

/// POSIX-based directory handle for Zig 0.16 compatibility
const PosixDir = struct {
    fd: posix.fd_t,

    pub fn close(self: PosixDir) void {
        posix.close(self.fd);
    }

    pub fn createFile(self: PosixDir, sub_path: []const u8, _: anytype) !PosixFile {
        const path_z = try std.posix.toPosixPath(sub_path);
        const fd = try posix.openatZ(self.fd, &path_z, .{
            .ACCMODE = .WRONLY,
            .CREAT = true,
            .TRUNC = true,
        }, 0o644);
        return .{ .fd = fd };
    }
};

/// POSIX-based file handle for Zig 0.16 compatibility
const PosixFile = struct {
    fd: posix.fd_t,

    pub fn close(self: PosixFile) void {
        posix.close(self.fd);
    }

    pub fn writeAll(self: PosixFile, data: []const u8) !void {
        var written: usize = 0;
        while (written < data.len) {
            written += try posix.write(self.fd, data[written..]);
        }
    }

    pub fn read(self: PosixFile, buffer: []u8) !usize {
        return posix.read(self.fd, buffer);
    }
};

/// Open directory using POSIX APIs
fn openDirAbsolute(path: []const u8) !PosixDir {
    const path_z = try std.posix.toPosixPath(path);
    const fd = try posix.openZ(&path_z, .{
        .ACCMODE = .RDONLY,
        .DIRECTORY = true,
    }, 0);
    return .{ .fd = fd };
}

/// Create directory using POSIX APIs
fn makeDirAbsolute(path: []const u8) !void {
    const path_z = try std.posix.toPosixPath(path);
    try posix.mkdirZ(&path_z, 0o755);
}

/// Delete file using POSIX APIs
fn deleteFileAbsolute(path: []const u8) void {
    const path_z = std.posix.toPosixPath(path) catch return;
    _ = linux.unlink(&path_z);
}

/// Open file for reading using POSIX APIs
fn openFileAbsolute(path: []const u8) !PosixFile {
    const path_z = try std.posix.toPosixPath(path);
    const fd = try posix.openZ(&path_z, .{ .ACCMODE = .RDONLY }, 0);
    return .{ .fd = fd };
}

/// Create file for writing using POSIX APIs
fn createFileAbsolute(path: []const u8) !PosixFile {
    const path_z = try std.posix.toPosixPath(path);
    const fd = try posix.openZ(&path_z, .{
        .ACCMODE = .WRONLY,
        .CREAT = true,
        .TRUNC = true,
    }, 0o644);
    return .{ .fd = fd };
}

/// Check if file exists (for tests)
fn fileExists(path: []const u8) bool {
    const path_z = std.posix.toPosixPath(path) catch return false;
    _ = posix.openZ(&path_z, .{ .ACCMODE = .RDONLY }, 0) catch return false;
    return true;
}

/// Configuration for multipart parser
pub const MultipartConfig = struct {
    /// Maximum size for a single file (default: 10MB)
    max_file_size: usize = 10 * 1024 * 1024,
    /// Maximum total body size (default: 50MB)
    max_body_size: usize = 50 * 1024 * 1024,
    /// Maximum number of parts allowed (default: 100)
    max_part_count: usize = 100,
    /// Allowed content types for file uploads (null = allow all)
    allowed_content_types: ?[]const []const u8 = null,
    /// Maximum header size per part (default: 8KB)
    max_header_size: usize = 8 * 1024,
};

/// Represents a single part in a multipart message
pub const Part = struct {
    /// Name from Content-Disposition (required)
    name: []const u8,
    /// Filename from Content-Disposition (only for file uploads)
    filename: ?[]const u8,
    /// Content-Type of the part (optional)
    content_type: ?[]const u8,
    /// The actual content data
    data: []const u8,

    /// Whether this is a file upload (has filename)
    pub fn isFile(self: *const Part) bool {
        return self.filename != null;
    }
};

/// Multipart parsing errors
pub const MultipartError = error{
    /// Content-Type is not multipart/form-data
    InvalidContentType,
    /// Boundary not found in Content-Type header
    MissingBoundary,
    /// Malformed multipart structure
    InvalidFormat,
    /// Missing required Content-Disposition header
    MissingContentDisposition,
    /// Missing required 'name' in Content-Disposition
    MissingName,
    /// Part headers exceed max_header_size
    HeadersTooLarge,
    /// Single file exceeds max_file_size
    FileTooLarge,
    /// Total body exceeds max_body_size
    BodyTooLarge,
    /// Number of parts exceeds max_part_count
    TooManyParts,
    /// File content-type not in allowed list
    DisallowedContentType,
    /// Request body is empty or missing
    NoBody,
};

/// Parsed Content-Disposition header values
const ContentDisposition = struct {
    name: ?[]const u8,
    filename: ?[]const u8,
};

/// Iterator for multipart/form-data parts
pub const MultipartIterator = struct {
    data: []const u8,
    boundary: []const u8,
    pos: usize,
    part_count: usize,
    config: MultipartConfig,
    state: State,
    last_error: ?MultipartError,

    const State = enum {
        initial,
        parsing,
        finished,
        error_state,
    };

    pub fn init(data: []const u8, boundary: []const u8, config: MultipartConfig) MultipartIterator {
        return .{
            .data = data,
            .boundary = boundary,
            .pos = 0,
            .part_count = 0,
            .config = config,
            .state = .initial,
            .last_error = null,
        };
    }

    /// Get the next part, returns null when done or on error
    pub fn next(self: *MultipartIterator) ?Part {
        if (self.state == .finished or self.state == .error_state) {
            return null;
        }

        // Validate part count
        if (self.part_count >= self.config.max_part_count) {
            self.last_error = MultipartError.TooManyParts;
            self.state = .error_state;
            return null;
        }

        // Find the boundary
        const delimiter = if (self.state == .initial) blk: {
            // First boundary: --{boundary}
            break :blk self.findInitialBoundary();
        } else blk: {
            // Subsequent boundaries: \r\n--{boundary}
            break :blk self.findNextBoundary();
        };

        const boundary_end = delimiter orelse {
            if (self.state == .initial) {
                self.last_error = MultipartError.InvalidFormat;
                self.state = .error_state;
            } else {
                self.state = .finished;
            }
            return null;
        };

        self.pos = boundary_end;

        // Check for terminating boundary (--boundary--)
        if (self.pos + 2 <= self.data.len and
            std.mem.eql(u8, self.data[self.pos..][0..2], "--"))
        {
            self.state = .finished;
            return null;
        }

        // Skip CRLF after boundary
        if (!self.skipCRLF()) {
            self.last_error = MultipartError.InvalidFormat;
            self.state = .error_state;
            return null;
        }

        // Find end of headers (\r\n\r\n)
        const headers_end = self.findHeadersEnd() orelse {
            self.last_error = MultipartError.InvalidFormat;
            self.state = .error_state;
            return null;
        };

        const headers_data = self.data[self.pos..headers_end];
        if (headers_data.len > self.config.max_header_size) {
            self.last_error = MultipartError.HeadersTooLarge;
            self.state = .error_state;
            return null;
        }

        // Parse Content-Disposition and Content-Type
        const disposition = parseContentDisposition(headers_data) orelse {
            self.last_error = MultipartError.MissingContentDisposition;
            self.state = .error_state;
            return null;
        };

        if (disposition.name == null) {
            self.last_error = MultipartError.MissingName;
            self.state = .error_state;
            return null;
        }

        const part_content_type = parsePartContentType(headers_data);

        // Move past headers and \r\n\r\n
        self.pos = headers_end + 4;

        // Find next boundary to determine content end
        const content_end = self.findContentEnd() orelse {
            self.last_error = MultipartError.InvalidFormat;
            self.state = .error_state;
            return null;
        };

        const content = self.data[self.pos..content_end];

        // Validate file size
        if (content.len > self.config.max_file_size) {
            self.last_error = MultipartError.FileTooLarge;
            self.state = .error_state;
            return null;
        }

        // Validate content type if this is a file and restrictions exist
        if (disposition.filename != null) {
            if (self.config.allowed_content_types) |allowed| {
                const ct = part_content_type orelse "application/octet-stream";
                var found = false;
                for (allowed) |allowed_type| {
                    if (std.mem.eql(u8, ct, allowed_type)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    self.last_error = MultipartError.DisallowedContentType;
                    self.state = .error_state;
                    return null;
                }
            }
        }

        // Move position to the \r\n before next boundary
        self.pos = content_end;
        self.part_count += 1;
        self.state = .parsing;

        return Part{
            .name = disposition.name.?,
            .filename = disposition.filename,
            .content_type = part_content_type,
            .data = content,
        };
    }

    /// Check if there was an error during parsing
    pub fn getError(self: *const MultipartIterator) ?MultipartError {
        return self.last_error;
    }

    /// Reset iterator to beginning
    pub fn reset(self: *MultipartIterator) void {
        self.pos = 0;
        self.part_count = 0;
        self.state = .initial;
        self.last_error = null;
    }

    /// Find the initial boundary (--boundary)
    fn findInitialBoundary(self: *MultipartIterator) ?usize {
        // Look for --{boundary}
        var i: usize = 0;
        while (i + 2 + self.boundary.len <= self.data.len) : (i += 1) {
            if (self.data[i] == '-' and self.data[i + 1] == '-') {
                if (std.mem.eql(u8, self.data[i + 2 ..][0..self.boundary.len], self.boundary)) {
                    return i + 2 + self.boundary.len;
                }
            }
        }
        return null;
    }

    /// Find the next boundary (\r\n--boundary)
    fn findNextBoundary(self: *MultipartIterator) ?usize {
        var i = self.pos;
        while (i + 4 + self.boundary.len <= self.data.len) : (i += 1) {
            if (self.data[i] == '\r' and self.data[i + 1] == '\n' and
                self.data[i + 2] == '-' and self.data[i + 3] == '-')
            {
                if (std.mem.eql(u8, self.data[i + 4 ..][0..self.boundary.len], self.boundary)) {
                    return i + 4 + self.boundary.len;
                }
            }
        }
        return null;
    }

    /// Find the end of content (position of \r\n before next boundary)
    fn findContentEnd(self: *MultipartIterator) ?usize {
        var i = self.pos;
        while (i + 4 + self.boundary.len <= self.data.len) : (i += 1) {
            if (self.data[i] == '\r' and self.data[i + 1] == '\n' and
                self.data[i + 2] == '-' and self.data[i + 3] == '-')
            {
                if (std.mem.eql(u8, self.data[i + 4 ..][0..self.boundary.len], self.boundary)) {
                    return i;
                }
            }
        }
        return null;
    }

    /// Find the position of \r\n\r\n (end of headers)
    fn findHeadersEnd(self: *MultipartIterator) ?usize {
        const remaining = self.data[self.pos..];
        const offset = std.mem.indexOf(u8, remaining, "\r\n\r\n") orelse return null;
        return self.pos + offset;
    }

    /// Skip \r\n at current position
    fn skipCRLF(self: *MultipartIterator) bool {
        if (self.pos + 2 <= self.data.len and
            std.mem.eql(u8, self.data[self.pos..][0..2], "\r\n"))
        {
            self.pos += 2;
            return true;
        }
        return false;
    }
};

/// Extract boundary from Content-Type header
/// Example: "multipart/form-data; boundary=----WebKitFormBoundary"
pub fn extractBoundary(content_type: []const u8) ?[]const u8 {
    // Check if it's multipart/form-data
    const multipart_prefix = "multipart/form-data";
    if (content_type.len < multipart_prefix.len) return null;

    // Case-insensitive check for multipart/form-data
    var found_multipart = true;
    for (content_type[0..multipart_prefix.len], multipart_prefix) |a, b| {
        if (std.ascii.toLower(a) != b) {
            found_multipart = false;
            break;
        }
    }
    if (!found_multipart) return null;

    // Find boundary= parameter
    const boundary_prefix = "boundary=";
    var i: usize = multipart_prefix.len;
    while (i + boundary_prefix.len <= content_type.len) : (i += 1) {
        // Case-insensitive match for boundary=
        var matches = true;
        for (0..boundary_prefix.len) |j| {
            if (std.ascii.toLower(content_type[i + j]) != boundary_prefix[j]) {
                matches = false;
                break;
            }
        }
        if (matches) {
            const start = i + boundary_prefix.len;

            // Handle quoted boundary
            if (start < content_type.len and content_type[start] == '"') {
                const quote_start = start + 1;
                if (std.mem.indexOfScalarPos(u8, content_type, quote_start, '"')) |quote_end| {
                    return content_type[quote_start..quote_end];
                }
                return null; // Unclosed quote
            }

            // Unquoted boundary - ends at ; or whitespace or end of string
            var end = start;
            while (end < content_type.len and
                content_type[end] != ';' and
                content_type[end] != ' ' and
                content_type[end] != '\t')
            {
                end += 1;
            }

            if (end > start) {
                return content_type[start..end];
            }
            break;
        }
    }
    return null;
}

/// Parse Content-Disposition header from part headers
fn parseContentDisposition(headers_data: []const u8) ?ContentDisposition {
    var lines = std.mem.splitSequence(u8, headers_data, "\r\n");
    while (lines.next()) |line| {
        const prefix = "content-disposition:";
        if (line.len < prefix.len) continue;

        // Case-insensitive match
        var matches = true;
        for (0..prefix.len) |i| {
            if (std.ascii.toLower(line[i]) != prefix[i]) {
                matches = false;
                break;
            }
        }
        if (matches) {
            const value = std.mem.trimStart(u8, line[prefix.len..], " \t");
            return parseDispositionValue(value);
        }
    }
    return null;
}

/// Parse the value portion of Content-Disposition
fn parseDispositionValue(value: []const u8) ?ContentDisposition {
    // Must start with form-data
    const form_data = "form-data";
    if (value.len < form_data.len) return null;

    var is_form_data = true;
    for (0..form_data.len) |i| {
        if (std.ascii.toLower(value[i]) != form_data[i]) {
            is_form_data = false;
            break;
        }
    }
    if (!is_form_data) return null;

    var result = ContentDisposition{ .name = null, .filename = null };

    // Parse parameters: ; name="value"; filename="value"
    var params = std.mem.tokenizeAny(u8, value, ";");
    _ = params.next(); // Skip "form-data"

    while (params.next()) |param| {
        const trimmed = std.mem.trim(u8, param, " \t");

        if (startsWithIgnoreCase(trimmed, "name=")) {
            result.name = extractQuotedOrUnquoted(trimmed["name=".len..]);
        } else if (startsWithIgnoreCase(trimmed, "filename*=")) {
            // RFC 5987 extended parameter - for now, skip charset and use the value
            result.filename = parseExtendedFilename(trimmed["filename*=".len..]);
        } else if (startsWithIgnoreCase(trimmed, "filename=")) {
            result.filename = extractQuotedOrUnquoted(trimmed["filename=".len..]);
        }
    }

    return result;
}

/// Parse Content-Type header from part headers
fn parsePartContentType(headers_data: []const u8) ?[]const u8 {
    var lines = std.mem.splitSequence(u8, headers_data, "\r\n");
    while (lines.next()) |line| {
        const prefix = "content-type:";
        if (line.len < prefix.len) continue;

        // Case-insensitive match
        var matches = true;
        for (0..prefix.len) |i| {
            if (std.ascii.toLower(line[i]) != prefix[i]) {
                matches = false;
                break;
            }
        }
        if (matches) {
            const value = std.mem.trimStart(u8, line[prefix.len..], " \t");
            // Return just the media type, not parameters
            if (std.mem.indexOfScalar(u8, value, ';')) |semi| {
                return std.mem.trimEnd(u8, value[0..semi], " \t");
            }
            return value;
        }
    }
    return null;
}

/// Case-insensitive prefix check
fn startsWithIgnoreCase(haystack: []const u8, needle: []const u8) bool {
    if (haystack.len < needle.len) return false;
    for (0..needle.len) |i| {
        if (std.ascii.toLower(haystack[i]) != std.ascii.toLower(needle[i])) {
            return false;
        }
    }
    return true;
}

/// Extract value from quoted or unquoted string
fn extractQuotedOrUnquoted(value: []const u8) ?[]const u8 {
    if (value.len == 0) return null;

    if (value[0] == '"') {
        // Quoted string
        if (value.len < 2) return null;
        if (std.mem.indexOfScalarPos(u8, value, 1, '"')) |end| {
            return value[1..end];
        }
        return null;
    }

    // Unquoted - ends at whitespace, semicolon, or end
    var end: usize = 0;
    while (end < value.len and value[end] != ' ' and value[end] != '\t' and value[end] != ';') {
        end += 1;
    }
    return if (end > 0) value[0..end] else null;
}

/// Parse RFC 5987 extended filename (filename*=UTF-8''%E4%B8%AD%E6%96%87.txt)
fn parseExtendedFilename(value: []const u8) ?[]const u8 {
    // Find the charset and value separator (two single quotes)
    if (std.mem.indexOf(u8, value, "''")) |sep| {
        // Return the percent-encoded portion as-is
        // The user can URL-decode if needed using urlDecode from request.zig
        const encoded = value[sep + 2 ..];
        return if (encoded.len > 0) encoded else null;
    }
    return null;
}

// ============================================================================
// Streaming Multipart Support
// ============================================================================

/// Configuration for streaming multipart parser
pub const StreamingConfig = struct {
    /// Memory threshold before spilling to disk (default: 1MB)
    memory_threshold: usize = 1024 * 1024,
    /// Maximum total upload size (default: 100MB)
    max_upload_size: usize = 100 * 1024 * 1024,
    /// Maximum size per part (default: 50MB)
    max_part_size: usize = 50 * 1024 * 1024,
    /// Maximum number of parts allowed (default: 100)
    max_part_count: usize = 100,
    /// Temp directory for file uploads (null = system default /tmp)
    temp_dir: ?[]const u8 = null,
    /// Allowed content types for file uploads (null = allow all)
    allowed_content_types: ?[]const []const u8 = null,
    /// Maximum header size per part (default: 8KB)
    max_header_size: usize = 8 * 1024,
};

/// Streaming multipart parsing errors
pub const StreamingError = error{
    InvalidFormat,
    MissingContentDisposition,
    MissingName,
    HeadersTooLarge,
    PartTooLarge,
    TooManyParts,
    UploadTooLarge,
    DisallowedContentType,
    TempFileError,
    UnexpectedEndOfInput,
    InvalidState,
    OutOfMemory,
};

/// Manages cleanup of temporary files created during streaming
pub const TempFileCleanup = struct {
    allocator: std.mem.Allocator,
    files: std.ArrayListUnmanaged([]const u8),
    temp_dir: ?PosixDir,

    pub fn init(allocator: std.mem.Allocator) TempFileCleanup {
        return .{
            .allocator = allocator,
            .files = .{},
            .temp_dir = null,
        };
    }

    /// Register a temp file path for cleanup
    pub fn register(self: *TempFileCleanup, path: []const u8) !void {
        const owned_path = try self.allocator.dupe(u8, path);
        try self.files.append(self.allocator, owned_path);
    }

    /// Delete all registered temp files
    pub fn cleanup(self: *TempFileCleanup) void {
        for (self.files.items) |path| {
            deleteFileAbsolute(path);
            self.allocator.free(path);
        }
        self.files.clearRetainingCapacity();
    }

    /// Ensure temp directory exists
    pub fn ensureTempDir(self: *TempFileCleanup, config_dir: ?[]const u8) !PosixDir {
        if (self.temp_dir) |dir| return dir;

        const dir_path = config_dir orelse "/tmp/oxelot-http-uploads";

        // Try to open or create the temp directory
        self.temp_dir = openDirAbsolute(dir_path) catch |err| blk: {
            if (err == error.FileNotFound) {
                makeDirAbsolute(dir_path) catch return error.TempFileError;
                break :blk openDirAbsolute(dir_path) catch return error.TempFileError;
            }
            return error.TempFileError;
        };

        return self.temp_dir.?;
    }

    pub fn deinit(self: *TempFileCleanup) void {
        self.cleanup();
        self.files.deinit(self.allocator);
        if (self.temp_dir) |*dir| {
            dir.close();
        }
    }
};

/// Storage for a streaming part - either in memory or in a temp file
pub const PartStorage = union(enum) {
    memory: []const u8,
    file: FileStorage,

    pub const FileStorage = struct {
        path: []const u8,
    };
};

/// Represents a single part from streaming multipart parsing
pub const StreamingPart = struct {
    allocator: std.mem.Allocator,
    /// Name from Content-Disposition (required)
    name: []const u8,
    /// Filename from Content-Disposition (only for file uploads)
    filename: ?[]const u8,
    /// Content-Type of the part (optional)
    content_type: ?[]const u8,
    /// Total size of the part content
    size: usize,
    /// Storage location (memory or file)
    storage: PartStorage,

    /// Check if this is a file upload (has filename)
    pub fn isFile(self: *const StreamingPart) bool {
        return self.filename != null;
    }

    /// Check if content is stored in memory
    pub fn isInMemory(self: *const StreamingPart) bool {
        return self.storage == .memory;
    }

    /// Get content as slice (only works for memory storage)
    pub fn data(self: *const StreamingPart) ?[]const u8 {
        return switch (self.storage) {
            .memory => |mem| mem,
            .file => null,
        };
    }

    /// Get file path (for file-backed parts)
    pub fn filePath(self: *const StreamingPart) ?[]const u8 {
        return switch (self.storage) {
            .file => |f| f.path,
            .memory => null,
        };
    }

    /// Read all content into a newly allocated buffer
    pub fn readAll(self: *const StreamingPart, allocator: std.mem.Allocator) ![]u8 {
        switch (self.storage) {
            .memory => |mem| {
                return try allocator.dupe(u8, mem);
            },
            .file => |f| {
                const file = try openFileAbsolute(f.path);
                defer file.close();

                // Allocate buffer and read the entire file
                const buffer = try allocator.alloc(u8, self.size);
                errdefer allocator.free(buffer);

                var total_read: usize = 0;
                while (total_read < self.size) {
                    const bytes_read = try file.read(buffer[total_read..]);
                    if (bytes_read == 0) break;
                    total_read += bytes_read;
                }

                return buffer;
            },
        }
    }

    pub fn deinit(self: *StreamingPart) void {
        // Free owned strings
        self.allocator.free(self.name);
        if (self.filename) |f| self.allocator.free(f);
        if (self.content_type) |ct| self.allocator.free(ct);

        // Free memory storage if applicable
        switch (self.storage) {
            .memory => |mem| self.allocator.free(mem),
            .file => |f| self.allocator.free(f.path),
        }
    }
};

/// Builds a part during streaming, handling threshold-based spillover to disk
const PartBuilder = struct {
    allocator: std.mem.Allocator,
    name: ?[]const u8,
    filename: ?[]const u8,
    content_type: ?[]const u8,
    memory_buffer: std.ArrayListUnmanaged(u8),
    temp_file: ?PosixFile,
    temp_path: ?[]const u8,
    total_size: usize,
    config: StreamingConfig,
    temp_cleanup: *TempFileCleanup,

    fn init(allocator: std.mem.Allocator, config: StreamingConfig, temp_cleanup: *TempFileCleanup) PartBuilder {
        return .{
            .allocator = allocator,
            .name = null,
            .filename = null,
            .content_type = null,
            .memory_buffer = .{},
            .temp_file = null,
            .temp_path = null,
            .total_size = 0,
            .config = config,
            .temp_cleanup = temp_cleanup,
        };
    }

    /// Set part metadata from headers
    fn setMetadata(self: *PartBuilder, name: []const u8, filename: ?[]const u8, content_type: ?[]const u8) !void {
        self.name = try self.allocator.dupe(u8, name);
        if (filename) |f| {
            self.filename = try self.allocator.dupe(u8, f);
        }
        if (content_type) |ct| {
            self.content_type = try self.allocator.dupe(u8, ct);
        }
    }

    /// Append content data, spilling to disk if threshold exceeded
    fn append(self: *PartBuilder, chunk: []const u8) !void {
        self.total_size += chunk.len;

        // Check part size limit
        if (self.total_size > self.config.max_part_size) {
            return StreamingError.PartTooLarge;
        }

        if (self.temp_file != null) {
            // Already spilled to file
            try self.temp_file.?.writeAll(chunk);
        } else if (self.memory_buffer.items.len + chunk.len > self.config.memory_threshold) {
            // Need to spill to file
            try self.spillToFile();
            try self.temp_file.?.writeAll(chunk);
        } else {
            // Keep in memory
            try self.memory_buffer.appendSlice(self.allocator, chunk);
        }
    }

    /// Spill current memory buffer to a temp file
    fn spillToFile(self: *PartBuilder) !void {
        const dir = try self.temp_cleanup.ensureTempDir(self.config.temp_dir);

        // Generate unique filename using random bytes
        var random_bytes: [16]u8 = undefined;
        std.crypto.random.bytes(&random_bytes);
        const hex_name = std.fmt.bytesToHex(random_bytes, .lower);

        const filename = try std.fmt.allocPrint(self.allocator, "upload_{s}.tmp", .{&hex_name});
        errdefer self.allocator.free(filename);

        // Create full path
        const dir_path = self.config.temp_dir orelse "/tmp/oxelot-http-uploads";
        const full_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}", .{ dir_path, filename });
        errdefer self.allocator.free(full_path);
        self.allocator.free(filename);

        // Create and register temp file
        const file = dir.createFile(std.fs.path.basename(full_path), .{}) catch return StreamingError.TempFileError;
        errdefer file.close();

        try self.temp_cleanup.register(full_path);

        // Write existing memory buffer to file
        if (self.memory_buffer.items.len > 0) {
            file.writeAll(self.memory_buffer.items) catch return StreamingError.TempFileError;
        }

        // Clear memory buffer
        self.memory_buffer.deinit(self.allocator);
        self.memory_buffer = .{};

        self.temp_file = file;
        self.temp_path = full_path;
    }

    /// Finalize and create a StreamingPart
    fn finalize(self: *PartBuilder) !StreamingPart {
        const name = self.name orelse return StreamingError.MissingName;

        // Close temp file if open
        if (self.temp_file) |*file| {
            file.close();
        }

        const storage: PartStorage = if (self.temp_path) |path|
            .{ .file = .{ .path = path } }
        else blk: {
            const owned_data = try self.allocator.dupe(u8, self.memory_buffer.items);
            break :blk .{ .memory = owned_data };
        };

        const result = StreamingPart{
            .allocator = self.allocator,
            .name = name,
            .filename = self.filename,
            .content_type = self.content_type,
            .size = self.total_size,
            .storage = storage,
        };

        // Clear state (ownership transferred to result)
        self.name = null;
        self.filename = null;
        self.content_type = null;
        self.memory_buffer.deinit(self.allocator);
        self.memory_buffer = .{};
        self.temp_file = null;
        self.temp_path = null;
        self.total_size = 0;

        return result;
    }

    fn deinit(self: *PartBuilder) void {
        if (self.name) |n| self.allocator.free(n);
        if (self.filename) |f| self.allocator.free(f);
        if (self.content_type) |ct| self.allocator.free(ct);
        self.memory_buffer.deinit(self.allocator);
        if (self.temp_file) |*file| file.close();
        if (self.temp_path) |path| self.allocator.free(path);
    }

    fn reset(self: *PartBuilder) void {
        if (self.name) |n| self.allocator.free(n);
        if (self.filename) |f| self.allocator.free(f);
        if (self.content_type) |ct| self.allocator.free(ct);
        self.name = null;
        self.filename = null;
        self.content_type = null;
        self.memory_buffer.clearRetainingCapacity();
        if (self.temp_file) |*file| file.close();
        self.temp_file = null;
        if (self.temp_path) |path| self.allocator.free(path);
        self.temp_path = null;
        self.total_size = 0;
    }
};

/// State machine for parsing multipart data incrementally
pub const StreamingMultipartParser = struct {
    allocator: std.mem.Allocator,
    boundary: []const u8,
    boundary_with_prefix: []const u8, // "\r\n--{boundary}"
    config: StreamingConfig,
    state: State,
    carryover: std.ArrayListUnmanaged(u8),
    header_buffer: std.ArrayListUnmanaged(u8),
    current_part: ?PartBuilder,
    completed_parts: std.ArrayListUnmanaged(StreamingPart),
    temp_cleanup: TempFileCleanup,
    part_count: usize,
    total_received: usize,
    last_error: ?StreamingError,

    const State = enum {
        seeking_initial_boundary,
        parsing_part_headers,
        reading_part_content,
        finished,
        error_state,
    };

    pub fn init(allocator: std.mem.Allocator, boundary: []const u8, config: StreamingConfig) !StreamingMultipartParser {
        // Create boundary pattern with prefix for searching
        const boundary_with_prefix = try std.fmt.allocPrint(allocator, "\r\n--{s}", .{boundary});

        return .{
            .allocator = allocator,
            .boundary = try allocator.dupe(u8, boundary),
            .boundary_with_prefix = boundary_with_prefix,
            .config = config,
            .state = .seeking_initial_boundary,
            .carryover = .{},
            .header_buffer = .{},
            .current_part = null,
            .completed_parts = .{},
            .temp_cleanup = TempFileCleanup.init(allocator),
            .part_count = 0,
            .total_received = 0,
            .last_error = null,
        };
    }

    /// Feed a chunk of data to the parser
    /// Returns number of complete parts ready for retrieval
    pub fn feed(self: *StreamingMultipartParser, chunk: []const u8) !usize {
        if (self.state == .finished or self.state == .error_state) {
            return 0;
        }

        self.total_received += chunk.len;
        if (self.total_received > self.config.max_upload_size) {
            self.last_error = StreamingError.UploadTooLarge;
            self.state = .error_state;
            return StreamingError.UploadTooLarge;
        }

        // Combine carryover with new chunk for processing
        var combined = std.ArrayListUnmanaged(u8){};
        defer combined.deinit(self.allocator);

        if (self.carryover.items.len > 0) {
            try combined.appendSlice(self.allocator, self.carryover.items);
            self.carryover.clearRetainingCapacity();
        }
        try combined.appendSlice(self.allocator, chunk);

        var data = combined.items;
        var parts_completed: usize = 0;

        while (data.len > 0) {
            switch (self.state) {
                .seeking_initial_boundary => {
                    // Look for --{boundary}
                    const initial_boundary = try std.fmt.allocPrint(self.allocator, "--{s}", .{self.boundary});
                    defer self.allocator.free(initial_boundary);

                    if (std.mem.indexOf(u8, data, initial_boundary)) |pos| {
                        const after_boundary = pos + initial_boundary.len;
                        if (after_boundary + 2 <= data.len) {
                            // Check for terminating --
                            if (std.mem.eql(u8, data[after_boundary..][0..2], "--")) {
                                self.state = .finished;
                                return parts_completed;
                            }
                            // Check for CRLF
                            if (std.mem.eql(u8, data[after_boundary..][0..2], "\r\n")) {
                                data = data[after_boundary + 2 ..];
                                self.state = .parsing_part_headers;
                                continue;
                            }
                        }
                        // Need more data
                        try self.carryover.appendSlice(self.allocator, data[pos..]);
                        return parts_completed;
                    }

                    // Keep tail in carryover for boundary detection
                    const keep = @min(data.len, initial_boundary.len + 2);
                    if (data.len > keep) {
                        try self.carryover.appendSlice(self.allocator, data[data.len - keep ..]);
                    } else {
                        try self.carryover.appendSlice(self.allocator, data);
                    }
                    return parts_completed;
                },

                .parsing_part_headers => {
                    // Accumulate headers until \r\n\r\n
                    try self.header_buffer.appendSlice(self.allocator, data);

                    if (self.header_buffer.items.len > self.config.max_header_size) {
                        self.last_error = StreamingError.HeadersTooLarge;
                        self.state = .error_state;
                        return StreamingError.HeadersTooLarge;
                    }

                    if (std.mem.indexOf(u8, self.header_buffer.items, "\r\n\r\n")) |headers_end| {
                        const headers_data = self.header_buffer.items[0..headers_end];

                        // Parse headers
                        const disposition = parseContentDisposition(headers_data) orelse {
                            self.last_error = StreamingError.MissingContentDisposition;
                            self.state = .error_state;
                            return StreamingError.MissingContentDisposition;
                        };

                        if (disposition.name == null) {
                            self.last_error = StreamingError.MissingName;
                            self.state = .error_state;
                            return StreamingError.MissingName;
                        }

                        const part_content_type = parsePartContentType(headers_data);

                        // Validate content type if restrictions exist
                        if (disposition.filename != null) {
                            if (self.config.allowed_content_types) |allowed| {
                                const ct = part_content_type orelse "application/octet-stream";
                                var found = false;
                                for (allowed) |allowed_type| {
                                    if (std.mem.eql(u8, ct, allowed_type)) {
                                        found = true;
                                        break;
                                    }
                                }
                                if (!found) {
                                    self.last_error = StreamingError.DisallowedContentType;
                                    self.state = .error_state;
                                    return StreamingError.DisallowedContentType;
                                }
                            }
                        }

                        // Create part builder
                        var part_builder = PartBuilder.init(self.allocator, self.config, &self.temp_cleanup);
                        try part_builder.setMetadata(disposition.name.?, disposition.filename, part_content_type);
                        self.current_part = part_builder;

                        // Data after headers - copy to combined buffer for continued processing
                        const remaining_start = headers_end + 4;
                        const remaining_data = self.header_buffer.items[remaining_start..];

                        // Clear and reuse combined for remaining data
                        combined.clearRetainingCapacity();
                        try combined.appendSlice(self.allocator, remaining_data);
                        self.header_buffer.clearRetainingCapacity();

                        data = combined.items;
                        self.state = .reading_part_content;
                        continue;
                    }

                    // Need more data for headers
                    return parts_completed;
                },

                .reading_part_content => {
                    // Look for boundary in data
                    if (std.mem.indexOf(u8, data, self.boundary_with_prefix)) |boundary_pos| {
                        // Found boundary - content ends before it
                        if (boundary_pos > 0) {
                            try self.current_part.?.append(data[0..boundary_pos]);
                        }

                        // Check part count
                        self.part_count += 1;
                        if (self.part_count > self.config.max_part_count) {
                            self.last_error = StreamingError.TooManyParts;
                            self.state = .error_state;
                            return StreamingError.TooManyParts;
                        }

                        // Finalize part
                        const part = try self.current_part.?.finalize();
                        try self.completed_parts.append(self.allocator, part);
                        self.current_part = null;
                        parts_completed += 1;

                        // Move past boundary
                        const after_boundary = boundary_pos + self.boundary_with_prefix.len;
                        if (after_boundary + 2 <= data.len) {
                            // Check for terminating --
                            if (std.mem.eql(u8, data[after_boundary..][0..2], "--")) {
                                self.state = .finished;
                                return parts_completed;
                            }
                            // Check for CRLF
                            if (std.mem.eql(u8, data[after_boundary..][0..2], "\r\n")) {
                                data = data[after_boundary + 2 ..];
                                self.state = .parsing_part_headers;
                                continue;
                            }
                        }

                        // Need more data to determine what's after boundary
                        try self.carryover.appendSlice(self.allocator, data[boundary_pos..]);
                        return parts_completed;
                    }

                    // No boundary found - append most data but keep potential partial boundary
                    const safe_len = if (data.len > self.boundary_with_prefix.len)
                        data.len - self.boundary_with_prefix.len
                    else
                        0;

                    if (safe_len > 0) {
                        try self.current_part.?.append(data[0..safe_len]);
                    }

                    // Keep tail for boundary detection
                    try self.carryover.appendSlice(self.allocator, data[safe_len..]);
                    return parts_completed;
                },

                .finished, .error_state => return parts_completed,
            }
        }

        return parts_completed;
    }

    /// Signal end of input - finalize any remaining parts
    pub fn finish(self: *StreamingMultipartParser) !void {
        if (self.state == .error_state) {
            return self.last_error orelse StreamingError.InvalidState;
        }

        // Process any remaining carryover
        if (self.carryover.items.len > 0) {
            _ = try self.feed(&[_]u8{});
        }

        if (self.state != .finished) {
            self.last_error = StreamingError.UnexpectedEndOfInput;
            self.state = .error_state;
            return StreamingError.UnexpectedEndOfInput;
        }
    }

    /// Get next completed part (call after feed returns > 0)
    pub fn nextPart(self: *StreamingMultipartParser) ?StreamingPart {
        if (self.completed_parts.items.len == 0) return null;
        return self.completed_parts.orderedRemove(0);
    }

    /// Check if there was an error during parsing
    pub fn getError(self: *const StreamingMultipartParser) ?StreamingError {
        return self.last_error;
    }

    /// Check if parsing is complete
    pub fn isFinished(self: *const StreamingMultipartParser) bool {
        return self.state == .finished;
    }

    /// Cleanup all resources including temp files
    pub fn deinit(self: *StreamingMultipartParser) void {
        self.allocator.free(self.boundary);
        self.allocator.free(self.boundary_with_prefix);
        self.carryover.deinit(self.allocator);
        self.header_buffer.deinit(self.allocator);

        if (self.current_part) |*part| {
            part.deinit();
        }

        for (self.completed_parts.items) |*part| {
            part.deinit();
        }
        self.completed_parts.deinit(self.allocator);

        self.temp_cleanup.deinit();
    }
};

// Tests
test "extract boundary - simple" {
    const ct = "multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW";
    try std.testing.expectEqualStrings("----WebKitFormBoundary7MA4YWxkTrZu0gW", extractBoundary(ct).?);
}

test "extract boundary - quoted" {
    const ct = "multipart/form-data; boundary=\"my-boundary\"";
    try std.testing.expectEqualStrings("my-boundary", extractBoundary(ct).?);
}

test "extract boundary - case insensitive" {
    const ct = "MULTIPART/FORM-DATA; BOUNDARY=test-boundary";
    try std.testing.expectEqualStrings("test-boundary", extractBoundary(ct).?);
}

test "extract boundary - not multipart" {
    const ct = "application/json";
    try std.testing.expect(extractBoundary(ct) == null);
}

test "parse simple multipart - single field" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"field1\"\r\n" ++
        "\r\n" ++
        "value1\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});
    const part = iter.next().?;
    try std.testing.expectEqualStrings("field1", part.name);
    try std.testing.expectEqualStrings("value1", part.data);
    try std.testing.expect(part.filename == null);
    try std.testing.expect(!part.isFile());
    try std.testing.expect(iter.next() == null);
    try std.testing.expect(iter.getError() == null);
}

test "parse multipart - file upload" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n" ++
        "Content-Type: text/plain\r\n" ++
        "\r\n" ++
        "Hello, World!\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});
    const part = iter.next().?;
    try std.testing.expectEqualStrings("file", part.name);
    try std.testing.expectEqualStrings("test.txt", part.filename.?);
    try std.testing.expectEqualStrings("text/plain", part.content_type.?);
    try std.testing.expectEqualStrings("Hello, World!", part.data);
    try std.testing.expect(part.isFile());
    try std.testing.expect(iter.next() == null);
    try std.testing.expect(iter.getError() == null);
}

test "parse multipart - multiple parts" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"name\"\r\n" ++
        "\r\n" ++
        "John Doe\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"email\"\r\n" ++
        "\r\n" ++
        "john@example.com\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"avatar\"; filename=\"photo.jpg\"\r\n" ++
        "Content-Type: image/jpeg\r\n" ++
        "\r\n" ++
        "JPEG_DATA_HERE\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});

    const part1 = iter.next().?;
    try std.testing.expectEqualStrings("name", part1.name);
    try std.testing.expectEqualStrings("John Doe", part1.data);

    const part2 = iter.next().?;
    try std.testing.expectEqualStrings("email", part2.name);
    try std.testing.expectEqualStrings("john@example.com", part2.data);

    const part3 = iter.next().?;
    try std.testing.expectEqualStrings("avatar", part3.name);
    try std.testing.expectEqualStrings("photo.jpg", part3.filename.?);
    try std.testing.expectEqualStrings("image/jpeg", part3.content_type.?);
    try std.testing.expectEqualStrings("JPEG_DATA_HERE", part3.data);

    try std.testing.expect(iter.next() == null);
    try std.testing.expect(iter.getError() == null);
}

test "max file size exceeded" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"large.bin\"\r\n" ++
        "\r\n" ++
        "12345678901234567890\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{ .max_file_size = 10 });
    try std.testing.expect(iter.next() == null);
    try std.testing.expectEqual(MultipartError.FileTooLarge, iter.getError().?);
}

test "max part count exceeded" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"a\"\r\n" ++
        "\r\n" ++
        "1\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"b\"\r\n" ++
        "\r\n" ++
        "2\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"c\"\r\n" ++
        "\r\n" ++
        "3\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{ .max_part_count = 2 });
    _ = iter.next();
    _ = iter.next();
    try std.testing.expect(iter.next() == null);
    try std.testing.expectEqual(MultipartError.TooManyParts, iter.getError().?);
}

test "content type validation" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"script.sh\"\r\n" ++
        "Content-Type: application/x-sh\r\n" ++
        "\r\n" ++
        "#!/bin/bash\r\n" ++
        "--boundary--";

    const allowed = [_][]const u8{ "image/jpeg", "image/png" };
    var iter = MultipartIterator.init(body, "boundary", .{ .allowed_content_types = &allowed });
    try std.testing.expect(iter.next() == null);
    try std.testing.expectEqual(MultipartError.DisallowedContentType, iter.getError().?);
}

test "missing content-disposition" {
    const body =
        "--boundary\r\n" ++
        "Content-Type: text/plain\r\n" ++
        "\r\n" ++
        "data\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});
    try std.testing.expect(iter.next() == null);
    try std.testing.expectEqual(MultipartError.MissingContentDisposition, iter.getError().?);
}

test "iterator reset" {
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"field\"\r\n" ++
        "\r\n" ++
        "value\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});
    _ = iter.next();
    try std.testing.expect(iter.next() == null);

    iter.reset();
    const part = iter.next().?;
    try std.testing.expectEqualStrings("field", part.name);
}

test "binary content with CRLF" {
    // Test that binary content containing \r\n doesn't break parsing
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"data\"\r\n" ++
        "\r\n" ++
        "line1\r\nline2\r\nline3\r\n" ++
        "--boundary--";

    var iter = MultipartIterator.init(body, "boundary", .{});
    const part = iter.next().?;
    try std.testing.expectEqualStrings("line1\r\nline2\r\nline3", part.data);
}

// ============================================================================
// Streaming Multipart Tests
// ============================================================================

test "streaming parser - single chunk" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"field1\"\r\n" ++
        "\r\n" ++
        "value1\r\n" ++
        "--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{});
    defer parser.deinit();

    const parts_ready = try parser.feed(body);
    try std.testing.expectEqual(@as(usize, 1), parts_ready);

    var part = parser.nextPart().?;
    defer part.deinit();

    try std.testing.expectEqualStrings("field1", part.name);
    try std.testing.expectEqualStrings("value1", part.data().?);
    try std.testing.expect(part.isInMemory());
    try std.testing.expect(!part.isFile());
}

test "streaming parser - chunked input" {
    const allocator = std.testing.allocator;

    // Split the body across multiple chunks
    const chunk1 = "--boundary\r\nContent-Disposition: form-data; name=\"fie";
    const chunk2 = "ld1\"\r\n\r\nval";
    const chunk3 = "ue1\r\n--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{});
    defer parser.deinit();

    var total_parts: usize = 0;
    total_parts += try parser.feed(chunk1);
    try std.testing.expectEqual(@as(usize, 0), total_parts);

    total_parts += try parser.feed(chunk2);
    try std.testing.expectEqual(@as(usize, 0), total_parts);

    total_parts += try parser.feed(chunk3);
    try std.testing.expectEqual(@as(usize, 1), total_parts);

    var part = parser.nextPart().?;
    defer part.deinit();

    try std.testing.expectEqualStrings("field1", part.name);
    try std.testing.expectEqualStrings("value1", part.data().?);
}

test "streaming parser - multiple parts" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"name\"\r\n" ++
        "\r\n" ++
        "John\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"email\"\r\n" ++
        "\r\n" ++
        "john@example.com\r\n" ++
        "--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{});
    defer parser.deinit();

    const parts_ready = try parser.feed(body);
    try std.testing.expectEqual(@as(usize, 2), parts_ready);

    var part1 = parser.nextPart().?;
    defer part1.deinit();
    try std.testing.expectEqualStrings("name", part1.name);
    try std.testing.expectEqualStrings("John", part1.data().?);

    var part2 = parser.nextPart().?;
    defer part2.deinit();
    try std.testing.expectEqualStrings("email", part2.name);
    try std.testing.expectEqualStrings("john@example.com", part2.data().?);
}

test "streaming parser - file upload" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"test.txt\"\r\n" ++
        "Content-Type: text/plain\r\n" ++
        "\r\n" ++
        "Hello, World!\r\n" ++
        "--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{});
    defer parser.deinit();

    _ = try parser.feed(body);

    var part = parser.nextPart().?;
    defer part.deinit();

    try std.testing.expectEqualStrings("file", part.name);
    try std.testing.expectEqualStrings("test.txt", part.filename.?);
    try std.testing.expectEqualStrings("text/plain", part.content_type.?);
    try std.testing.expectEqualStrings("Hello, World!", part.data().?);
    try std.testing.expect(part.isFile());
    try std.testing.expect(part.isInMemory());
}

test "streaming parser - too many parts" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"a\"\r\n" ++
        "\r\n" ++
        "1\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"b\"\r\n" ++
        "\r\n" ++
        "2\r\n" ++
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"c\"\r\n" ++
        "\r\n" ++
        "3\r\n" ++
        "--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{ .max_part_count = 2 });
    defer parser.deinit();

    const result = parser.feed(body);
    try std.testing.expectError(StreamingError.TooManyParts, result);
}

test "streaming parser - upload too large" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"field\"\r\n" ++
        "\r\n" ++
        "this is some data that exceeds the limit\r\n" ++
        "--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{ .max_upload_size = 20 });
    defer parser.deinit();

    const result = parser.feed(body);
    try std.testing.expectError(StreamingError.UploadTooLarge, result);
}

test "streaming parser - content type validation" {
    const allocator = std.testing.allocator;
    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"script.sh\"\r\n" ++
        "Content-Type: application/x-sh\r\n" ++
        "\r\n" ++
        "#!/bin/bash\r\n" ++
        "--boundary--";

    const allowed = [_][]const u8{ "image/jpeg", "image/png" };
    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{ .allowed_content_types = &allowed });
    defer parser.deinit();

    const result = parser.feed(body);
    try std.testing.expectError(StreamingError.DisallowedContentType, result);
}

test "streaming parser - boundary across chunks" {
    const allocator = std.testing.allocator;

    // Deliberately split the boundary across chunks
    const chunk1 = "--boundary\r\nContent-Disposition: form-data; name=\"field\"\r\n\r\ndata here\r";
    const chunk2 = "\n--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{});
    defer parser.deinit();

    var total_parts: usize = 0;
    total_parts += try parser.feed(chunk1);
    total_parts += try parser.feed(chunk2);

    try std.testing.expectEqual(@as(usize, 1), total_parts);

    var part = parser.nextPart().?;
    defer part.deinit();
    try std.testing.expectEqualStrings("data here", part.data().?);
}

test "streaming parser - large file spillover to temp file" {
    const allocator = std.testing.allocator;

    // Use a small memory threshold to force spillover
    const memory_threshold = 100;
    const file_size = 500; // Larger than threshold

    // Generate file content
    var file_content: [file_size]u8 = undefined;
    for (&file_content, 0..) |*b, i| {
        b.* = @truncate(i % 256);
    }

    // Build multipart body
    const header =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"largefile\"; filename=\"large.bin\"\r\n" ++
        "Content-Type: application/octet-stream\r\n" ++
        "\r\n";
    const footer = "\r\n--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{
        .memory_threshold = memory_threshold,
        .temp_dir = "/tmp",
    });
    defer parser.deinit();

    // Feed header
    _ = try parser.feed(header);

    // Feed file content in chunks (simulating streaming)
    var offset: usize = 0;
    const chunk_size = 50;
    while (offset < file_content.len) {
        const end = @min(offset + chunk_size, file_content.len);
        _ = try parser.feed(file_content[offset..end]);
        offset = end;
    }

    // Feed footer
    _ = try parser.feed(footer);
    try parser.finish();

    // Get the part
    var part = parser.nextPart().?;
    defer part.deinit();

    // Verify it's a file and spilled to disk
    try std.testing.expect(part.isFile());
    try std.testing.expect(!part.isInMemory());
    try std.testing.expectEqual(file_size, part.size);
    try std.testing.expectEqualStrings("largefile", part.name);
    try std.testing.expectEqualStrings("large.bin", part.filename.?);

    // Verify temp file exists and path is valid
    const temp_path = part.filePath().?;
    try std.testing.expect(temp_path.len > 0);

    // Read the file content and verify it matches
    const read_data = try part.readAll(allocator);
    defer allocator.free(read_data);
    try std.testing.expectEqual(file_size, read_data.len);
    try std.testing.expectEqualSlices(u8, &file_content, read_data);

    // Simulate moving to permanent storage
    const permanent_path = "/tmp/oxelot-http-test-permanent.bin";
    defer deleteFileAbsolute(permanent_path);

    // Copy to permanent location (can't rename across filesystems)
    {
        const src_file = try openFileAbsolute(temp_path);
        defer src_file.close();
        const dest_file = try createFileAbsolute(permanent_path);
        defer dest_file.close();
        var buf: [1024]u8 = undefined;
        while (true) {
            const bytes_read = try src_file.read(&buf);
            if (bytes_read == 0) break;
            try dest_file.writeAll(buf[0..bytes_read]);
        }
    }

    // Verify permanent file exists and has correct content
    {
        const perm_file = try openFileAbsolute(permanent_path);
        defer perm_file.close();
        const permanent_content = try allocator.alloc(u8, file_size);
        defer allocator.free(permanent_content);
        _ = try perm_file.read(permanent_content);
        try std.testing.expectEqualSlices(u8, &file_content, permanent_content);
    }

    // Temp file cleanup happens when part.deinit() is called
    // After this test, verify temp file is deleted
}

test "streaming parser - temp file cleanup on deinit" {
    const allocator = std.testing.allocator;

    const memory_threshold = 50;
    const file_size = 200;

    var file_content: [file_size]u8 = undefined;
    @memset(&file_content, 'X');

    const body =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"file\"; filename=\"test.bin\"\r\n" ++
        "Content-Type: application/octet-stream\r\n" ++
        "\r\n" ++
        file_content ++
        "\r\n--boundary--";

    var temp_path_copy: [256]u8 = undefined;
    var temp_path_len: usize = 0;

    {
        var parser = try StreamingMultipartParser.init(allocator, "boundary", .{
            .memory_threshold = memory_threshold,
            .temp_dir = "/tmp",
        });
        defer parser.deinit();

        _ = try parser.feed(body);
        try parser.finish();

        var part = parser.nextPart().?;
        defer part.deinit();

        // Verify it spilled to temp file
        try std.testing.expect(!part.isInMemory());

        // Save the temp path for later verification
        const temp_path = part.filePath().?;
        @memcpy(temp_path_copy[0..temp_path.len], temp_path);
        temp_path_len = temp_path.len;

        // Verify temp file exists
        try std.testing.expect(fileExists(temp_path));
    }

    // After parser and part are deinitialized, temp file should be deleted
    const temp_path = temp_path_copy[0..temp_path_len];
    try std.testing.expect(!fileExists(temp_path));
}

test "streaming parser - chunked large file with multiple parts" {
    const allocator = std.testing.allocator;

    const memory_threshold = 100;

    // Part 1: Small field (stays in memory)
    const part1_header =
        "--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"username\"\r\n" ++
        "\r\n";
    const part1_data = "john_doe";

    // Part 2: Large file (spills to disk)
    const part2_header =
        "\r\n--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"avatar\"; filename=\"photo.jpg\"\r\n" ++
        "Content-Type: image/jpeg\r\n" ++
        "\r\n";
    var part2_data: [300]u8 = undefined;
    @memset(&part2_data, 0xFF); // Fake JPEG data

    // Part 3: Another small field
    const part3_header =
        "\r\n--boundary\r\n" ++
        "Content-Disposition: form-data; name=\"email\"\r\n" ++
        "\r\n";
    const part3_data = "john@example.com";

    const footer = "\r\n--boundary--";

    var parser = try StreamingMultipartParser.init(allocator, "boundary", .{
        .memory_threshold = memory_threshold,
        .temp_dir = "/tmp",
    });
    defer parser.deinit();

    // Feed in realistic chunks
    _ = try parser.feed(part1_header);
    _ = try parser.feed(part1_data);
    _ = try parser.feed(part2_header);
    // Feed large file in small chunks
    var offset: usize = 0;
    while (offset < part2_data.len) {
        const end = @min(offset + 64, part2_data.len);
        _ = try parser.feed(part2_data[offset..end]);
        offset = end;
    }
    _ = try parser.feed(part3_header);
    _ = try parser.feed(part3_data);
    _ = try parser.feed(footer);
    try parser.finish();

    // Verify part 1: small field in memory
    {
        var part = parser.nextPart().?;
        defer part.deinit();
        try std.testing.expectEqualStrings("username", part.name);
        try std.testing.expect(part.isInMemory());
        try std.testing.expect(!part.isFile());
        try std.testing.expectEqualStrings("john_doe", part.data().?);
    }

    // Verify part 2: large file on disk
    {
        var part = parser.nextPart().?;
        defer part.deinit();
        try std.testing.expectEqualStrings("avatar", part.name);
        try std.testing.expectEqualStrings("photo.jpg", part.filename.?);
        try std.testing.expectEqualStrings("image/jpeg", part.content_type.?);
        try std.testing.expect(!part.isInMemory());
        try std.testing.expect(part.isFile());
        try std.testing.expectEqual(@as(usize, 300), part.size);

        // Read and verify content
        const content = try part.readAll(allocator);
        defer allocator.free(content);
        try std.testing.expectEqualSlices(u8, &part2_data, content);
    }

    // Verify part 3: small field in memory
    {
        var part = parser.nextPart().?;
        defer part.deinit();
        try std.testing.expectEqualStrings("email", part.name);
        try std.testing.expect(part.isInMemory());
        try std.testing.expectEqualStrings("john@example.com", part.data().?);
    }

    // No more parts
    try std.testing.expect(parser.nextPart() == null);
}
