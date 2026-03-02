// HTTP Client for oxelot-http
//
// Features:
// - DNS resolution with caching
// - Connection pooling with keep-alive
// - All HTTP methods
// - Redirect following
// - Timeout handling
// - Chunked transfer encoding
// - TLS/HTTPS support

const std = @import("std");
const posix = std.posix;
const Allocator = std.mem.Allocator;
const Method = @import("method.zig").Method;
const Status = @import("status.zig").Status;
const Headers = @import("headers.zig").Headers;
const client_response = @import("client/response.zig");
const pool_mod = @import("client/pool.zig");
const tls_mod = @import("client/tls.zig");
pub const Response = client_response.Response;
pub const ConnectionPool = pool_mod.ConnectionPool;
pub const PoolConfig = pool_mod.PoolConfig;
pub const PoolStats = pool_mod.PoolStats;
pub const TlsConnection = tls_mod.TlsConnection;
pub const deinitCaBundle = tls_mod.deinitCaBundle;
pub const ClientCertificate = tls_mod.ClientCertificate;
pub const loadClientCertificate = tls_mod.loadClientCertificate;
pub const deinitClientCertificate = tls_mod.deinitClientCertificate;

/// Client configuration
pub const Config = struct {
    /// Request timeout in milliseconds
    timeout_ms: u32 = 30_000,
    /// Connection timeout in milliseconds
    connect_timeout_ms: u32 = 5_000,
    /// Maximum number of redirects to follow
    max_redirects: u8 = 5,
    /// Whether to automatically follow redirects
    follow_redirects: bool = true,
    /// Maximum response body size
    max_response_size: usize = 10 * 1024 * 1024, // 10MB
    /// User-Agent header value
    user_agent: []const u8 = "oxelot-http/1.0",
    /// Initial read buffer size
    read_buffer_size: usize = 8192,
    /// Enable connection pooling
    use_pool: bool = true,
    /// Connection pool size
    pool_size: u16 = 10,
    /// Pool idle timeout in seconds
    pool_idle_timeout: u32 = 60,
    /// DNS cache TTL in seconds
    dns_ttl_seconds: i64 = 300,
    /// Skip TLS certificate verification (insecure, for testing only)
    tls_insecure: bool = false,
    /// Path to client certificate file (PEM format) for mutual TLS
    tls_client_cert_path: ?[]const u8 = null,
    /// Path to client private key file (PEM format) for mutual TLS
    tls_client_key_path: ?[]const u8 = null,
};

/// HTTP client errors
pub const Error = error{
    InvalidUrl,
    HostNotFound,
    ConnectionFailed,
    ConnectionTimeout,
    RequestTimeout,
    TooManyRedirects,
    InvalidRedirect,
    ResponseTooLarge,
    InvalidResponse,
    IncompleteResponse,
    SocketError,
    WriteError,
    ReadError,
    PoolExhausted,
    HostKeyTooLong,
    TlsError,
} || Allocator.Error;

/// HTTP Client
pub const Client = struct {
    allocator: Allocator,
    config: Config,
    pool: ?*ConnectionPool,
    client_cert: ?tls_mod.ClientCertificate,

    pub fn init(allocator: Allocator, config: Config) Client {
        // Load client certificate if paths are provided
        const client_cert: ?tls_mod.ClientCertificate = if (config.tls_client_cert_path != null and config.tls_client_key_path != null)
            tls_mod.loadClientCertificate(allocator, config.tls_client_cert_path.?, config.tls_client_key_path.?) catch null
        else
            null;

        return .{
            .allocator = allocator,
            .config = config,
            .pool = null,
            .client_cert = client_cert,
        };
    }

    pub fn deinit(self: *Client) void {
        if (self.pool) |p| {
            p.deinit();
            self.allocator.destroy(p);
            self.pool = null;
        }
        if (self.client_cert) |*cert| {
            tls_mod.deinitClientCertificate(cert, self.allocator);
            self.client_cert = null;
        }
    }

    /// Get or create the connection pool
    fn getPool(self: *Client) !*ConnectionPool {
        if (self.pool) |p| return p;

        const p = try self.allocator.create(ConnectionPool);
        errdefer self.allocator.destroy(p);

        p.* = try ConnectionPool.init(self.allocator, .{
            .max_connections = self.config.pool_size,
            .idle_timeout_seconds = self.config.pool_idle_timeout,
            .dns_ttl_seconds = self.config.dns_ttl_seconds,
            .connect_timeout_ms = self.config.connect_timeout_ms,
            .socket_timeout_ms = self.config.timeout_ms,
        });

        self.pool = p;
        return p;
    }

    /// Get pool statistics (returns null if pool not initialized)
    pub fn poolStats(self: *Client) ?PoolStats {
        if (self.pool) |p| {
            return p.stats();
        }
        return null;
    }

    /// Close idle connections in the pool
    pub fn closeIdleConnections(self: *Client) void {
        if (self.pool) |p| {
            p.closeIdle();
        }
    }

    /// Simple GET request
    pub fn get(self: *Client, url: []const u8) Error!Response {
        return self.request(.GET, url, .{});
    }

    /// Simple HEAD request
    pub fn head(self: *Client, url: []const u8) Error!Response {
        return self.request(.HEAD, url, .{});
    }

    /// Simple DELETE request
    pub fn delete(self: *Client, url: []const u8) Error!Response {
        return self.request(.DELETE, url, .{});
    }

    /// POST request with body
    pub fn post(self: *Client, url: []const u8, options: RequestOptions) Error!Response {
        return self.request(.POST, url, options);
    }

    /// PUT request with body
    pub fn put(self: *Client, url: []const u8, options: RequestOptions) Error!Response {
        return self.request(.PUT, url, options);
    }

    /// PATCH request with body
    pub fn patch(self: *Client, url: []const u8, options: RequestOptions) Error!Response {
        return self.request(.PATCH, url, options);
    }

    /// Request options for methods that take a body
    pub const RequestOptions = struct {
        body: ?[]const u8 = null,
        content_type: ?[]const u8 = null,
        headers: ?[]const Header = null,
    };

    pub const Header = struct {
        name: []const u8,
        value: []const u8,
    };

    /// Make an HTTP request
    pub fn request(
        self: *Client,
        method: Method,
        url: []const u8,
        options: RequestOptions,
    ) Error!Response {
        return self.executeRequest(method, url, options, 0);
    }

    fn executeRequest(
        self: *Client,
        method: Method,
        url: []const u8,
        options: RequestOptions,
        redirect_count: u8,
    ) Error!Response {
        // Parse URL
        const uri = std.Uri.parse(url) catch return error.InvalidUrl;

        const host = if (uri.host) |h| switch (h) {
            .raw => |raw| raw,
            .percent_encoded => |pe| pe,
        } else return error.InvalidUrl;

        const is_https = if (uri.scheme.len > 0)
            std.mem.eql(u8, uri.scheme, "https")
        else
            false;

        const port: u16 = uri.port orelse if (is_https) 443 else 80;
        const path = if (uri.path.percent_encoded.len > 0) uri.path.percent_encoded else "/";

        // HTTPS uses TLS (no connection pooling for now)
        if (is_https) {
            return self.executeHttpsRequest(method, host, port, path, url, options, redirect_count);
        }

        // Use connection pool if enabled
        if (self.config.use_pool) {
            return self.executePooledRequest(method, host, port, path, url, options, redirect_count);
        }

        // Non-pooled request (original behavior)
        return self.executeDirectRequest(method, host, port, path, url, options, redirect_count);
    }

    /// Execute request using connection pool
    fn executePooledRequest(
        self: *Client,
        method: Method,
        host: []const u8,
        port: u16,
        path: []const u8,
        url: []const u8,
        options: RequestOptions,
        redirect_count: u8,
    ) Error!Response {
        const p = try self.getPool();

        // Acquire connection from pool (handles DNS caching internally)
        var pooled_conn = p.acquire(host, port) catch |err| {
            return switch (err) {
                error.PoolExhausted => error.PoolExhausted,
                error.HostNotFound => error.HostNotFound,
                error.ConnectionFailed => error.ConnectionFailed,
                error.SocketError => error.SocketError,
                error.HostKeyTooLong => error.HostKeyTooLong,
                else => error.ConnectionFailed,
            };
        };

        // Build request with keep-alive
        const request_data = try self.buildRequestPooled(method, host, path, options);
        defer self.allocator.free(request_data);

        // Send request
        _ = posix.write(pooled_conn.socket, request_data) catch {
            pooled_conn.invalidate();
            p.release(pooled_conn);
            return error.WriteError;
        };

        // Read response
        var response_buf: std.ArrayListUnmanaged(u8) = .empty;
        defer response_buf.deinit(self.allocator);

        var read_buf: [8192]u8 = undefined;
        var read_error = false;
        while (true) {
            const n = posix.read(pooled_conn.socket, &read_buf) catch |err| {
                if (err == error.WouldBlock) {
                    read_error = true;
                    break;
                }
                pooled_conn.invalidate();
                p.release(pooled_conn);
                return error.ReadError;
            };

            if (n == 0) break; // EOF - connection closed by server

            try response_buf.appendSlice(self.allocator, read_buf[0..n]);

            if (response_buf.items.len > self.config.max_response_size) {
                pooled_conn.invalidate();
                p.release(pooled_conn);
                return error.ResponseTooLarge;
            }

            // Check if we have complete response
            if (try self.isResponseComplete(response_buf.items)) {
                break;
            }
        }

        if (read_error) {
            pooled_conn.invalidate();
            p.release(pooled_conn);
            return error.RequestTimeout;
        }

        // Check if server wants to close connection
        const should_close = self.shouldCloseConnection(response_buf.items);
        if (should_close) {
            pooled_conn.invalidate();
        }

        // Release connection back to pool
        p.release(pooled_conn);

        // Parse and handle response
        return self.parseAndHandleResponse(response_buf.items, url, method, options, redirect_count);
    }

    /// Execute direct request without pool (original behavior)
    fn executeDirectRequest(
        self: *Client,
        method: Method,
        host: []const u8,
        port: u16,
        path: []const u8,
        url: []const u8,
        options: RequestOptions,
        redirect_count: u8,
    ) Error!Response {
        // Resolve host to address
        const sockaddr = try self.resolveHost(host, port);

        // Create socket
        const socket = posix.socket(
            sockaddr.family,
            posix.SOCK.STREAM,
            0,
        ) catch return error.SocketError;
        errdefer posix.close(socket);

        // Set socket timeouts
        try self.setSocketTimeouts(socket);

        // Connect
        posix.connect(socket, @ptrCast(&sockaddr), @sizeOf(posix.sockaddr.in)) catch
            return error.ConnectionFailed;

        // Build request with Connection: close
        const request_data = try self.buildRequest(method, host, path, options);
        defer self.allocator.free(request_data);

        // Send request
        _ = posix.write(socket, request_data) catch return error.WriteError;

        // Read response
        var response_buf: std.ArrayListUnmanaged(u8) = .empty;
        defer response_buf.deinit(self.allocator);

        var read_buf: [8192]u8 = undefined;
        while (true) {
            const n = posix.read(socket, &read_buf) catch |err| {
                if (err == error.WouldBlock) {
                    return error.RequestTimeout;
                }
                return error.ReadError;
            };

            if (n == 0) break; // EOF

            try response_buf.appendSlice(self.allocator, read_buf[0..n]);

            if (response_buf.items.len > self.config.max_response_size) {
                return error.ResponseTooLarge;
            }

            // Check if we have complete headers and body
            if (try self.isResponseComplete(response_buf.items)) {
                break;
            }
        }

        posix.close(socket);

        // Parse and handle response
        return self.parseAndHandleResponse(response_buf.items, url, method, options, redirect_count);
    }

    /// Execute HTTPS request with TLS
    fn executeHttpsRequest(
        self: *Client,
        method: Method,
        host: []const u8,
        port: u16,
        path: []const u8,
        url: []const u8,
        options: RequestOptions,
        redirect_count: u8,
    ) Error!Response {
        // Resolve host to address
        const sockaddr = try self.resolveHost(host, port);

        // Create socket
        const socket = posix.socket(
            sockaddr.family,
            posix.SOCK.STREAM,
            0,
        ) catch return error.SocketError;
        errdefer posix.close(socket);

        // Set socket timeouts
        try self.setSocketTimeouts(socket);

        // Connect
        posix.connect(socket, @ptrCast(&sockaddr), @sizeOf(posix.sockaddr.in)) catch
            return error.ConnectionFailed;

        // Establish TLS connection
        var tls_conn = tls_mod.TlsConnection.init(
            self.allocator,
            socket,
            host,
            .{
                .insecure = self.config.tls_insecure,
                .client_cert = if (self.client_cert) |*cert| cert else null,
            },
        ) catch return error.TlsError;
        defer tls_conn.deinit();

        // Build request with Connection: close
        const request_data = try self.buildRequest(method, host, path, options);
        defer self.allocator.free(request_data);

        // Send request over TLS
        tls_conn.writeAll(request_data) catch return error.WriteError;

        // Read response over TLS
        var response_buf: std.ArrayListUnmanaged(u8) = .empty;
        defer response_buf.deinit(self.allocator);

        var read_buf: [8192]u8 = undefined;
        while (true) {
            const n = tls_conn.read(&read_buf) catch |err| {
                if (err == error.ConnectionClosed) break;
                return error.ReadError;
            };

            if (n == 0) break; // EOF

            try response_buf.appendSlice(self.allocator, read_buf[0..n]);

            if (response_buf.items.len > self.config.max_response_size) {
                return error.ResponseTooLarge;
            }

            // Check if we have complete headers and body
            if (try self.isResponseComplete(response_buf.items)) {
                break;
            }
        }

        // Socket is closed when tls_conn.deinit() is called and we close it here
        posix.close(socket);

        // Parse and handle response
        return self.parseAndHandleResponse(response_buf.items, url, method, options, redirect_count);
    }

    /// Check if server wants to close connection
    fn shouldCloseConnection(self: *Client, data: []const u8) bool {
        _ = self;
        const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse return true;
        const headers = data[0..header_end];

        // Look for Connection: close header
        var iter = std.mem.splitSequence(u8, headers, "\r\n");
        while (iter.next()) |line| {
            if (std.ascii.startsWithIgnoreCase(line, "connection:")) {
                const value_start = std.mem.indexOf(u8, line, ":").? + 1;
                const value = std.mem.trim(u8, line[value_start..], " ");
                return std.ascii.eqlIgnoreCase(value, "close");
            }
        }
        return false; // Default to keep-alive for HTTP/1.1
    }

    /// Parse response and handle redirects
    fn parseAndHandleResponse(
        self: *Client,
        data: []const u8,
        url: []const u8,
        method: Method,
        options: RequestOptions,
        redirect_count: u8,
    ) Error!Response {
        // Parse response
        var response = client_response.parseResponse(
            self.allocator,
            data,
            url,
        ) catch |err| {
            return switch (err) {
                error.IncompleteResponse => error.IncompleteResponse,
                error.InvalidResponse => error.InvalidResponse,
                else => error.InvalidResponse,
            };
        };

        // Handle redirects
        if (self.config.follow_redirects and response.isRedirect()) {
            if (redirect_count >= self.config.max_redirects) {
                response.deinit();
                return error.TooManyRedirects;
            }

            const location = response.headers.get("Location") orelse {
                response.deinit();
                return error.InvalidRedirect;
            };

            // Resolve redirect URL (handle relative URLs)
            const new_url = try self.resolveRedirectUrl(url, location);
            defer self.allocator.free(new_url);

            // Save status before deinit
            const status = response.status;
            response.deinit();

            // For 303 See Other, always use GET
            // For 307/308, preserve the original method
            const new_method: Method = switch (status) {
                .see_other => .GET,
                .temporary_redirect, .permanent_redirect => method,
                else => .GET, // 301, 302 traditionally change to GET
            };

            const new_options: RequestOptions = switch (status) {
                .temporary_redirect, .permanent_redirect => options,
                else => .{}, // Don't send body on 301/302/303
            };

            return self.executeRequest(new_method, new_url, new_options, redirect_count + 1);
        }

        return response;
    }

    /// Resolve hostname to sockaddr
    fn resolveHost(self: *Client, host: []const u8, port: u16) !posix.sockaddr.in {
        _ = self;

        // Handle localhost specially
        if (std.mem.eql(u8, host, "localhost") or std.mem.eql(u8, host, "127.0.0.1")) {
            return posix.sockaddr.in{
                .family = posix.AF.INET,
                .port = @byteSwap(port),
                .addr = @byteSwap(@as(u32, 0x7F000001)), // 127.0.0.1
            };
        }

        // Try parsing as IPv4
        if (parseIpv4(host)) |addr| {
            return posix.sockaddr.in{
                .family = posix.AF.INET,
                .port = @byteSwap(port),
                .addr = addr,
            };
        }

        // Fall back to DNS resolution via libc
        const c = std.c;
        var hints: c.addrinfo = .{
            .flags = .{},
            .family = posix.AF.INET,
            .socktype = posix.SOCK.STREAM,
            .protocol = 0,
            .addrlen = 0,
            .addr = null,
            .canonname = null,
            .next = null,
        };

        var result: ?*c.addrinfo = null;

        // Need null-terminated string for getaddrinfo
        var host_buf: [256]u8 = undefined;
        if (host.len >= host_buf.len) return error.HostNotFound;
        @memcpy(host_buf[0..host.len], host);
        host_buf[host.len] = 0;

        const ret = c.getaddrinfo(@ptrCast(&host_buf), null, &hints, &result);
        if (@intFromEnum(ret) != 0 or result == null) {
            return error.HostNotFound;
        }
        defer c.freeaddrinfo(result.?);

        // Extract IPv4 address from result
        const addr_in: *const posix.sockaddr.in = @ptrCast(@alignCast(result.?.addr));
        return posix.sockaddr.in{
            .family = posix.AF.INET,
            .port = @byteSwap(port),
            .addr = addr_in.addr,
        };
    }

    /// Parse IPv4 address string to network byte order u32
    fn parseIpv4(host: []const u8) ?u32 {
        var parts = std.mem.splitScalar(u8, host, '.');
        var ip: u32 = 0;
        var count: u8 = 0;

        while (parts.next()) |part| : (count += 1) {
            if (count >= 4) return null;
            const octet = std.fmt.parseInt(u8, part, 10) catch return null;
            ip = (ip << 8) | @as(u32, octet);
        }

        if (count != 4) return null;
        return @byteSwap(ip); // Convert to network byte order
    }

    /// Set socket timeouts
    fn setSocketTimeouts(self: *Client, socket: posix.socket_t) !void {
        const timeout = posix.timeval{
            .sec = @intCast(self.config.timeout_ms / 1000),
            .usec = @intCast((self.config.timeout_ms % 1000) * 1000),
        };

        posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&timeout)) catch {};
        posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.SNDTIMEO, std.mem.asBytes(&timeout)) catch {};
    }

    /// Build HTTP request bytes
    fn buildRequest(
        self: *Client,
        method: Method,
        host: []const u8,
        path: []const u8,
        options: RequestOptions,
    ) ![]u8 {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        // Request line
        const request_line = try std.fmt.allocPrint(self.allocator, "{s} {s} HTTP/1.1\r\n", .{ method.toString(), path });
        defer self.allocator.free(request_line);
        try buf.appendSlice(self.allocator, request_line);

        // Required headers
        const host_header = try std.fmt.allocPrint(self.allocator, "Host: {s}\r\n", .{host});
        defer self.allocator.free(host_header);
        try buf.appendSlice(self.allocator, host_header);

        const ua_header = try std.fmt.allocPrint(self.allocator, "User-Agent: {s}\r\n", .{self.config.user_agent});
        defer self.allocator.free(ua_header);
        try buf.appendSlice(self.allocator, ua_header);

        try buf.appendSlice(self.allocator, "Connection: close\r\n");
        try buf.appendSlice(self.allocator, "Accept: */*\r\n");

        // Custom headers
        if (options.headers) |headers| {
            for (headers) |h| {
                const header_line = try std.fmt.allocPrint(self.allocator, "{s}: {s}\r\n", .{ h.name, h.value });
                defer self.allocator.free(header_line);
                try buf.appendSlice(self.allocator, header_line);
            }
        }

        // Body headers and content
        if (options.body) |body| {
            if (options.content_type) |ct| {
                const ct_header = try std.fmt.allocPrint(self.allocator, "Content-Type: {s}\r\n", .{ct});
                defer self.allocator.free(ct_header);
                try buf.appendSlice(self.allocator, ct_header);
            }
            const cl_header = try std.fmt.allocPrint(self.allocator, "Content-Length: {d}\r\n", .{body.len});
            defer self.allocator.free(cl_header);
            try buf.appendSlice(self.allocator, cl_header);
            try buf.appendSlice(self.allocator, "\r\n");
            try buf.appendSlice(self.allocator, body);
        } else {
            try buf.appendSlice(self.allocator, "\r\n");
        }

        return buf.toOwnedSlice(self.allocator);
    }

    /// Build HTTP request bytes with keep-alive for pooled connections
    fn buildRequestPooled(
        self: *Client,
        method: Method,
        host: []const u8,
        path: []const u8,
        options: RequestOptions,
    ) ![]u8 {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        // Request line
        const request_line = try std.fmt.allocPrint(self.allocator, "{s} {s} HTTP/1.1\r\n", .{ method.toString(), path });
        defer self.allocator.free(request_line);
        try buf.appendSlice(self.allocator, request_line);

        // Required headers
        const host_header = try std.fmt.allocPrint(self.allocator, "Host: {s}\r\n", .{host});
        defer self.allocator.free(host_header);
        try buf.appendSlice(self.allocator, host_header);

        const ua_header = try std.fmt.allocPrint(self.allocator, "User-Agent: {s}\r\n", .{self.config.user_agent});
        defer self.allocator.free(ua_header);
        try buf.appendSlice(self.allocator, ua_header);

        // Keep-alive for pooled connections
        try buf.appendSlice(self.allocator, "Connection: keep-alive\r\n");
        try buf.appendSlice(self.allocator, "Accept: */*\r\n");

        // Custom headers
        if (options.headers) |headers| {
            for (headers) |h| {
                const header_line = try std.fmt.allocPrint(self.allocator, "{s}: {s}\r\n", .{ h.name, h.value });
                defer self.allocator.free(header_line);
                try buf.appendSlice(self.allocator, header_line);
            }
        }

        // Body headers and content
        if (options.body) |body| {
            if (options.content_type) |ct| {
                const ct_header = try std.fmt.allocPrint(self.allocator, "Content-Type: {s}\r\n", .{ct});
                defer self.allocator.free(ct_header);
                try buf.appendSlice(self.allocator, ct_header);
            }
            const cl_header = try std.fmt.allocPrint(self.allocator, "Content-Length: {d}\r\n", .{body.len});
            defer self.allocator.free(cl_header);
            try buf.appendSlice(self.allocator, cl_header);
            try buf.appendSlice(self.allocator, "\r\n");
            try buf.appendSlice(self.allocator, body);
        } else {
            try buf.appendSlice(self.allocator, "\r\n");
        }

        return buf.toOwnedSlice(self.allocator);
    }

    /// Check if response is complete
    fn isResponseComplete(self: *Client, data: []const u8) !bool {
        _ = self;

        // Find header end
        const header_end = std.mem.indexOf(u8, data, "\r\n\r\n") orelse return false;

        // Parse headers to find Content-Length or Transfer-Encoding
        const headers_data = data[0..header_end];

        // Check for chunked encoding
        if (std.mem.indexOf(u8, headers_data, "Transfer-Encoding: chunked") != null or
            std.mem.indexOf(u8, headers_data, "transfer-encoding: chunked") != null)
        {
            // Look for final chunk marker "0\r\n\r\n"
            const body_start = header_end + 4;
            if (body_start >= data.len) return false;

            // Simple check: look for "0\r\n" followed by optional headers and "\r\n"
            const body = data[body_start..];
            return std.mem.indexOf(u8, body, "\r\n0\r\n") != null or
                std.mem.startsWith(u8, body, "0\r\n");
        }

        // Check for Content-Length
        var iter = std.mem.splitSequence(u8, headers_data, "\r\n");
        while (iter.next()) |line| {
            if (std.ascii.startsWithIgnoreCase(line, "content-length:")) {
                const value_start = std.mem.indexOf(u8, line, ":").? + 1;
                const value = std.mem.trim(u8, line[value_start..], " ");
                const content_length = std.fmt.parseInt(usize, value, 10) catch continue;
                const body_start = header_end + 4;
                return data.len >= body_start + content_length;
            }
        }

        // No Content-Length and not chunked - assume complete when connection closes
        return true;
    }

    /// Resolve redirect URL (handles relative URLs)
    fn resolveRedirectUrl(self: *Client, base_url: []const u8, location: []const u8) ![]u8 {
        // If location is absolute, use it directly
        if (std.mem.startsWith(u8, location, "http://") or
            std.mem.startsWith(u8, location, "https://"))
        {
            return self.allocator.dupe(u8, location);
        }

        // Parse base URL to get scheme and host
        const base_uri = std.Uri.parse(base_url) catch return error.InvalidRedirect;

        const host = if (base_uri.host) |h| switch (h) {
            .raw => |raw| raw,
            .percent_encoded => |pe| pe,
        } else return error.InvalidRedirect;

        // Build absolute URL
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buf.deinit(self.allocator);

        const base_part = try std.fmt.allocPrint(self.allocator, "{s}://{s}", .{ base_uri.scheme, host });
        defer self.allocator.free(base_part);
        try buf.appendSlice(self.allocator, base_part);

        if (base_uri.port) |port| {
            const port_part = try std.fmt.allocPrint(self.allocator, ":{d}", .{port});
            defer self.allocator.free(port_part);
            try buf.appendSlice(self.allocator, port_part);
        }

        if (std.mem.startsWith(u8, location, "/")) {
            // Absolute path
            try buf.appendSlice(self.allocator, location);
        } else {
            // Relative path - append to base path
            const base_path = base_uri.path.percent_encoded;
            if (std.mem.lastIndexOf(u8, base_path, "/")) |last_slash| {
                try buf.appendSlice(self.allocator, base_path[0 .. last_slash + 1]);
            } else {
                try buf.appendSlice(self.allocator, "/");
            }
            try buf.appendSlice(self.allocator, location);
        }

        return buf.toOwnedSlice(self.allocator);
    }
};

/// Create a request builder for fluent API
pub fn request(client: *Client, method: Method, url: []const u8) RequestBuilder {
    return RequestBuilder.init(client, method, url);
}

/// Fluent request builder
pub const RequestBuilder = struct {
    client: *Client,
    method: Method,
    url: []const u8,
    headers_buf: [32]Client.Header = undefined,
    headers_len: usize = 0,
    body_data: ?[]const u8 = null,
    content_type_val: ?[]const u8 = null,

    pub fn init(client: *Client, method: Method, url: []const u8) RequestBuilder {
        return .{
            .client = client,
            .method = method,
            .url = url,
        };
    }

    /// Add a header
    pub fn header(self: *RequestBuilder, name: []const u8, value: []const u8) *RequestBuilder {
        if (self.headers_len < self.headers_buf.len) {
            self.headers_buf[self.headers_len] = .{ .name = name, .value = value };
            self.headers_len += 1;
        }
        return self;
    }

    /// Set request body
    pub fn body(self: *RequestBuilder, data: []const u8) *RequestBuilder {
        self.body_data = data;
        return self;
    }

    /// Set content type
    pub fn contentType(self: *RequestBuilder, ct: []const u8) *RequestBuilder {
        self.content_type_val = ct;
        return self;
    }

    /// Set JSON body (convenience for JSON content type)
    pub fn jsonBody(self: *RequestBuilder, data: []const u8) *RequestBuilder {
        self.body_data = data;
        self.content_type_val = "application/json";
        return self;
    }

    /// Execute the request
    pub fn send(self: *RequestBuilder) Error!Response {
        const headers_slice: ?[]const Client.Header = if (self.headers_len > 0)
            self.headers_buf[0..self.headers_len]
        else
            null;

        return self.client.request(self.method, self.url, .{
            .body = self.body_data,
            .content_type = self.content_type_val,
            .headers = headers_slice,
        });
    }
};

test "client init" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    try std.testing.expectEqual(@as(u32, 30_000), client.config.timeout_ms);
    try std.testing.expectEqual(@as(u8, 5), client.config.max_redirects);
}

test "build request" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    const req = try client.buildRequest(.GET, "example.com", "/api/test", .{});
    defer std.testing.allocator.free(req);

    try std.testing.expect(std.mem.startsWith(u8, req, "GET /api/test HTTP/1.1\r\n"));
    try std.testing.expect(std.mem.indexOf(u8, req, "Host: example.com\r\n") != null);
}

test "build request with body" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    const req = try client.buildRequest(.POST, "example.com", "/api/data", .{
        .body = "{\"test\": true}",
        .content_type = "application/json",
    });
    defer std.testing.allocator.free(req);

    try std.testing.expect(std.mem.indexOf(u8, req, "Content-Type: application/json\r\n") != null);
    try std.testing.expect(std.mem.indexOf(u8, req, "Content-Length: 14\r\n") != null);
    try std.testing.expect(std.mem.endsWith(u8, req, "{\"test\": true}"));
}

test "resolve redirect url - absolute" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    const url = try client.resolveRedirectUrl(
        "http://example.com/old",
        "http://other.com/new",
    );
    defer std.testing.allocator.free(url);

    try std.testing.expectEqualStrings("http://other.com/new", url);
}

test "resolve redirect url - absolute path" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    const url = try client.resolveRedirectUrl(
        "http://example.com/old/path",
        "/new/path",
    );
    defer std.testing.allocator.free(url);

    try std.testing.expectEqualStrings("http://example.com/new/path", url);
}

test "resolve redirect url - relative path" {
    var client = Client.init(std.testing.allocator, .{});
    defer client.deinit();

    const url = try client.resolveRedirectUrl(
        "http://example.com/dir/old",
        "new",
    );
    defer std.testing.allocator.free(url);

    try std.testing.expectEqualStrings("http://example.com/dir/new", url);
}
