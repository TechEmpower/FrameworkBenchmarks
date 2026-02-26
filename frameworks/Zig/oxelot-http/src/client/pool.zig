// HTTP Connection Pool
//
// Thread-safe connection pool with:
// - Atomic CAS-based connection acquisition
// - Host:port matching for connection reuse
// - Keep-alive support
// - Idle connection cleanup
// - DNS caching with TTL

const std = @import("std");
const posix = std.posix;
const Allocator = std.mem.Allocator;
const c = std.c;

/// Get current timestamp in seconds
fn getTimestamp() i64 {
    const ts = posix.clock_gettime(.REALTIME) catch return 0;
    return ts.sec;
}

/// Cached socket address
pub const CachedAddress = struct {
    family: posix.sa_family_t,
    addr: u32, // IPv4 address in network byte order
    port: u16, // port in network byte order
};

/// DNS cache entry
const DnsCacheEntry = struct {
    address: CachedAddress,
    expires_at: i64, // timestamp when entry expires
};

/// DNS cache with TTL
pub const DnsCache = struct {
    entries: std.StringHashMapUnmanaged(DnsCacheEntry),
    allocator: Allocator,
    ttl_seconds: i64,
    mutex: std.Thread.Mutex,

    pub fn init(allocator: Allocator, ttl_seconds: i64) DnsCache {
        return .{
            .entries = .{},
            .allocator = allocator,
            .ttl_seconds = ttl_seconds,
            .mutex = .{},
        };
    }

    pub fn deinit(self: *DnsCache) void {
        // Free all keys
        var iter = self.entries.keyIterator();
        while (iter.next()) |key| {
            self.allocator.free(key.*);
        }
        self.entries.deinit(self.allocator);
    }

    /// Look up cached address or resolve and cache
    pub fn resolve(self: *DnsCache, host: []const u8, port: u16) !CachedAddress {
        const now = getTimestamp();

        // Create cache key
        var key_buf: [512]u8 = undefined;
        const key = std.fmt.bufPrint(&key_buf, "{s}:{d}", .{ host, port }) catch
            return self.resolveAndCache(host, port, now);

        self.mutex.lock();
        defer self.mutex.unlock();

        // Check cache
        if (self.entries.get(key)) |entry| {
            if (entry.expires_at > now) {
                return entry.address;
            }
            // Expired - remove it
            if (self.entries.fetchRemove(key)) |kv| {
                self.allocator.free(kv.key);
            }
        }

        // Not in cache or expired - resolve
        const address = try resolveHostInternal(host, port);

        // Cache it
        const owned_key = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(owned_key);

        try self.entries.put(self.allocator, owned_key, .{
            .address = address,
            .expires_at = now + self.ttl_seconds,
        });

        return address;
    }

    fn resolveAndCache(self: *DnsCache, host: []const u8, port: u16, now: i64) !CachedAddress {
        _ = self;
        _ = now;
        return resolveHostInternal(host, port);
    }

    /// Clear expired entries
    pub fn cleanup(self: *DnsCache) void {
        const now = getTimestamp();

        self.mutex.lock();
        defer self.mutex.unlock();

        var to_remove: std.ArrayListUnmanaged([]const u8) = .empty;
        defer to_remove.deinit(self.allocator);

        var iter = self.entries.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.expires_at <= now) {
                to_remove.append(self.allocator, entry.key_ptr.*) catch continue;
            }
        }

        for (to_remove.items) |key| {
            if (self.entries.fetchRemove(key)) |kv| {
                self.allocator.free(kv.key);
            }
        }
    }
};

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

/// Resolve host to address (internal helper)
fn resolveHostInternal(host: []const u8, port: u16) !CachedAddress {
    // Handle localhost specially
    if (std.mem.eql(u8, host, "localhost") or std.mem.eql(u8, host, "127.0.0.1")) {
        return CachedAddress{
            .family = posix.AF.INET,
            .addr = @byteSwap(@as(u32, 0x7F000001)), // 127.0.0.1
            .port = @byteSwap(port),
        };
    }

    // Try parsing as IPv4
    if (parseIpv4(host)) |addr| {
        return CachedAddress{
            .family = posix.AF.INET,
            .addr = addr,
            .port = @byteSwap(port),
        };
    }

    // For real DNS resolution, use getaddrinfo via libc
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
    // getaddrinfo returns 0 (success) or a negative EAI error code
    if (@intFromEnum(ret) != 0 or result == null) {
        return error.HostNotFound;
    }
    defer c.freeaddrinfo(result.?);

    // Extract IPv4 address from result
    const addr_in: *const posix.sockaddr.in = @ptrCast(@alignCast(result.?.addr));
    return CachedAddress{
        .family = posix.AF.INET,
        .addr = addr_in.addr,
        .port = @byteSwap(port),
    };
}

/// A pooled HTTP connection
pub const PooledConnection = struct {
    socket: posix.socket_t,
    host_key: []const u8, // "host:port" - owned
    in_use: std.atomic.Value(bool),
    last_used: i64, // timestamp
    valid: bool, // connection still usable

    /// Check if this connection matches the given host:port
    pub fn matches(self: *const PooledConnection, host_key: []const u8) bool {
        return std.mem.eql(u8, self.host_key, host_key);
    }

    /// Mark connection as invalid (will be closed on release)
    pub fn invalidate(self: *PooledConnection) void {
        self.valid = false;
    }
};

/// Connection pool configuration
pub const PoolConfig = struct {
    /// Maximum connections per host
    max_connections: u16 = 10,
    /// Idle timeout in seconds (connections unused longer are closed)
    idle_timeout_seconds: u32 = 60,
    /// DNS cache TTL in seconds
    dns_ttl_seconds: i64 = 300, // 5 minutes
    /// Connect timeout in milliseconds
    connect_timeout_ms: u32 = 5_000,
    /// Socket timeout in milliseconds
    socket_timeout_ms: u32 = 30_000,
};

/// HTTP Connection Pool
pub const ConnectionPool = struct {
    connections: []PooledConnection,
    allocator: Allocator,
    config: PoolConfig,
    dns_cache: DnsCache,
    mutex: std.Thread.Mutex,

    pub fn init(allocator: Allocator, config: PoolConfig) !ConnectionPool {
        const connections = try allocator.alloc(PooledConnection, config.max_connections);
        errdefer allocator.free(connections);

        // Initialize all slots as empty
        for (connections) |*conn| {
            conn.* = .{
                .socket = -1,
                .host_key = "",
                .in_use = std.atomic.Value(bool).init(false),
                .last_used = 0,
                .valid = false,
            };
        }

        return .{
            .connections = connections,
            .allocator = allocator,
            .config = config,
            .dns_cache = DnsCache.init(allocator, config.dns_ttl_seconds),
            .mutex = .{},
        };
    }

    pub fn deinit(self: *ConnectionPool) void {
        // Close all connections
        for (self.connections) |*conn| {
            if (conn.socket >= 0) {
                posix.close(conn.socket);
            }
            if (conn.host_key.len > 0) {
                self.allocator.free(conn.host_key);
            }
        }
        self.allocator.free(self.connections);
        self.dns_cache.deinit();
    }

    /// Acquire a connection to the given host:port
    /// Returns an existing pooled connection or creates a new one
    pub fn acquire(self: *ConnectionPool, host: []const u8, port: u16) !*PooledConnection {
        var key_buf: [512]u8 = undefined;
        const host_key = std.fmt.bufPrint(&key_buf, "{s}:{d}", .{ host, port }) catch
            return error.HostKeyTooLong;

        // First, try to find an existing connection to this host
        for (self.connections) |*conn| {
            if (conn.valid and conn.matches(host_key)) {
                if (conn.in_use.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
                    // Got it! Check if still valid
                    if (self.isSocketValid(conn.socket)) {
                        conn.last_used = getTimestamp();
                        return conn;
                    }
                    // Socket died - close and create new
                    posix.close(conn.socket);
                    conn.valid = false;
                    conn.in_use.store(false, .release);
                }
            }
        }

        // No existing connection - find an empty slot or reuse old one
        return self.createConnection(host, port, host_key);
    }

    /// Create a new connection in an available slot
    fn createConnection(self: *ConnectionPool, host: []const u8, port: u16, host_key: []const u8) !*PooledConnection {
        // Resolve address (uses DNS cache)
        const cached_addr = try self.dns_cache.resolve(host, port);

        // Create socket
        const socket = posix.socket(
            cached_addr.family,
            posix.SOCK.STREAM,
            0,
        ) catch return error.SocketError;
        errdefer posix.close(socket);

        // Set socket options
        try self.configureSocket(socket);

        // Build sockaddr from cached address
        const sockaddr = posix.sockaddr.in{
            .family = cached_addr.family,
            .port = cached_addr.port,
            .addr = cached_addr.addr,
        };

        // Connect
        posix.connect(socket, @ptrCast(&sockaddr), @sizeOf(posix.sockaddr.in)) catch
            return error.ConnectionFailed;

        // Find a slot
        self.mutex.lock();
        defer self.mutex.unlock();

        // Look for empty slot first
        for (self.connections) |*conn| {
            if (!conn.valid and conn.in_use.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
                // Clean up old host_key if any
                if (conn.host_key.len > 0) {
                    self.allocator.free(conn.host_key);
                }
                if (conn.socket >= 0) {
                    posix.close(conn.socket);
                }

                conn.socket = socket;
                conn.host_key = try self.allocator.dupe(u8, host_key);
                conn.last_used = getTimestamp();
                conn.valid = true;
                return conn;
            }
        }

        // No empty slots - find oldest idle connection to evict
        var oldest: ?*PooledConnection = null;
        var oldest_time: i64 = std.math.maxInt(i64);

        for (self.connections) |*conn| {
            if (conn.in_use.load(.acquire) == false and conn.last_used < oldest_time) {
                oldest = conn;
                oldest_time = conn.last_used;
            }
        }

        if (oldest) |conn| {
            if (conn.in_use.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
                // Evict this connection
                if (conn.socket >= 0) {
                    posix.close(conn.socket);
                }
                if (conn.host_key.len > 0) {
                    self.allocator.free(conn.host_key);
                }

                conn.socket = socket;
                conn.host_key = try self.allocator.dupe(u8, host_key);
                conn.last_used = getTimestamp();
                conn.valid = true;
                return conn;
            }
        }

        // Pool is full and all connections are in use
        posix.close(socket);
        return error.PoolExhausted;
    }

    /// Release a connection back to the pool
    pub fn release(self: *ConnectionPool, conn: *PooledConnection) void {
        _ = self;
        if (!conn.valid) {
            // Connection was invalidated - close socket
            if (conn.socket >= 0) {
                posix.close(conn.socket);
                conn.socket = -1;
            }
        }
        conn.last_used = getTimestamp();
        conn.in_use.store(false, .release);
    }

    /// Close idle connections that have exceeded the timeout
    pub fn closeIdle(self: *ConnectionPool) void {
        const now = getTimestamp();
        const timeout = @as(i64, @intCast(self.config.idle_timeout_seconds));

        for (self.connections) |*conn| {
            if (conn.valid and !conn.in_use.load(.acquire)) {
                if (now - conn.last_used > timeout) {
                    // Try to acquire to close
                    if (conn.in_use.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
                        if (conn.socket >= 0) {
                            posix.close(conn.socket);
                            conn.socket = -1;
                        }
                        conn.valid = false;
                        conn.in_use.store(false, .release);
                    }
                }
            }
        }

        // Also cleanup DNS cache
        self.dns_cache.cleanup();
    }

    /// Configure socket with timeouts and options
    fn configureSocket(self: *ConnectionPool, socket: posix.socket_t) !void {
        // Set TCP_NODELAY
        const nodelay: u32 = 1;
        posix.setsockopt(socket, posix.IPPROTO.TCP, posix.TCP.NODELAY, std.mem.asBytes(&nodelay)) catch {};

        // Set timeouts
        const timeout = posix.timeval{
            .sec = @intCast(self.config.socket_timeout_ms / 1000),
            .usec = @intCast((self.config.socket_timeout_ms % 1000) * 1000),
        };
        posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.RCVTIMEO, std.mem.asBytes(&timeout)) catch {};
        posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.SNDTIMEO, std.mem.asBytes(&timeout)) catch {};

        // Enable keep-alive
        const keepalive: u32 = 1;
        posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.KEEPALIVE, std.mem.asBytes(&keepalive)) catch {};
    }

    /// Check if socket is still valid
    fn isSocketValid(self: *ConnectionPool, socket: posix.socket_t) bool {
        _ = self;
        if (socket < 0) return false;

        // Try a non-blocking peek to check if connection is still alive
        var buf: [1]u8 = undefined;
        const flags = posix.MSG.PEEK | posix.MSG.DONTWAIT;
        const result = posix.recv(socket, &buf, flags);

        if (result) |n| {
            // n == 0 means connection closed by peer
            return n != 0;
        } else |err| {
            // EAGAIN/EWOULDBLOCK means socket is fine, just no data
            return err == error.WouldBlock;
        }
    }

    /// Get pool statistics
    pub fn stats(self: *ConnectionPool) PoolStats {
        var total: u16 = 0;
        var in_use: u16 = 0;
        var valid: u16 = 0;

        for (self.connections) |*conn| {
            total += 1;
            if (conn.in_use.load(.acquire)) in_use += 1;
            if (conn.valid) valid += 1;
        }

        return .{
            .total_slots = total,
            .in_use = in_use,
            .available = valid - in_use,
            .dns_cache_entries = @intCast(self.dns_cache.entries.count()),
        };
    }
};

/// Pool statistics
pub const PoolStats = struct {
    total_slots: u16,
    in_use: u16,
    available: u16,
    dns_cache_entries: u32,
};

// Errors
pub const PoolError = error{
    PoolExhausted,
    HostKeyTooLong,
    SocketError,
    ConnectionFailed,
    HostNotFound,
};

test "dns cache basic" {
    var cache = DnsCache.init(std.testing.allocator, 300);
    defer cache.deinit();

    // Resolve localhost
    const addr = try cache.resolve("127.0.0.1", 8080);
    try std.testing.expectEqual(@byteSwap(@as(u16, 8080)), addr.port);

    // Should be cached now
    const addr2 = try cache.resolve("127.0.0.1", 8080);
    try std.testing.expectEqual(addr.addr, addr2.addr);
    try std.testing.expectEqual(addr.port, addr2.port);
}

test "connection pool init/deinit" {
    var pool = try ConnectionPool.init(std.testing.allocator, .{
        .max_connections = 5,
    });
    defer pool.deinit();

    const s = pool.stats();
    try std.testing.expectEqual(@as(u16, 5), s.total_slots);
    try std.testing.expectEqual(@as(u16, 0), s.in_use);
}
