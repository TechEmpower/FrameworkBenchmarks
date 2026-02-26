// io_uring-based HTTP server for oxelot-http
//
// Features:
// - Multi-threaded with SO_REUSEPORT
// - One io_uring per thread for maximum throughput
// - HTTP pipelining support
// - Configurable thread count, ring size, and optimizations

const std = @import("std");
const posix = std.posix;
const IoUring = std.os.linux.IoUring;
const linux = std.os.linux;

const request_mod = @import("request.zig");
const Request = request_mod.Request;
const parseRequest = request_mod.parseRequest;
const pico = request_mod.pico;
const Response = @import("response.zig").Response;
const Router = @import("router.zig").Router;
const Context = @import("router.zig").Context;
const WebSocketHandler = @import("router.zig").WebSocketHandler;
const websocket = @import("websocket.zig");
const multipart = @import("multipart.zig");

// Tags for io_uring operations
const ACCEPT_TAG: u64 = 0xFFFFFFFFFFFFFFFF;
const EVENTFD_TAG: u64 = 0xFFFFFFFFFFFFFFFE;
const WRITE_TAG_BIT: u64 = 0x8000000000000000;
const WS_WRITE_TAG_BIT: u64 = 0x4000000000000000; // WebSocket write (separate from HTTP writes)
pub const DB_POLL_TAG_BIT: u64 = 0x2000000000000000; // Async DB poll

// ============================================================================
// WebSocket Write Queue (for cross-thread writes via io_uring)
// ============================================================================

/// Request to write data to a WebSocket connection
pub const WsWriteRequest = struct {
    socket: posix.socket_t,
    data: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *WsWriteRequest) void {
        self.allocator.free(self.data);
    }
};

/// Context for in-flight WebSocket writes (used as io_uring user_data)
/// This allows multiple concurrent writes per connection without race conditions
const WsWriteContext = struct {
    data: []u8,
    allocator: std.mem.Allocator,

    fn deinit(self: *WsWriteContext, ctx_allocator: std.mem.Allocator) void {
        self.allocator.free(self.data);
        ctx_allocator.destroy(self);
    }
};

/// Thread-safe queue for WebSocket writes
/// External threads push writes here, io_uring thread drains and submits via ring.write()
pub const WsWriteQueue = struct {
    items: std.ArrayListUnmanaged(WsWriteRequest),
    mutex: std.Thread.Mutex,
    eventfd: posix.fd_t,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !WsWriteQueue {
        // EFD_NONBLOCK = O_NONBLOCK = 0o4000 on x86_64
        const EFD_NONBLOCK: u32 = 0o4000;
        const efd = try posix.eventfd(0, EFD_NONBLOCK);
        return .{
            .items = .empty,
            .mutex = .{},
            .eventfd = efd,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *WsWriteQueue) void {
        // Free any remaining items
        for (self.items.items) |*req| {
            req.deinit();
        }
        self.items.deinit(self.allocator);
        posix.close(self.eventfd);
    }

    /// Push a write request and signal the event loop
    pub fn push(self: *WsWriteQueue, req: WsWriteRequest) !void {
        self.mutex.lock();
        const was_empty = self.items.items.len == 0;
        try self.items.append(self.allocator, req);
        self.mutex.unlock();

        // Only signal when queue transitions from empty to non-empty
        // This batches multiple writes into a single eventfd wakeup
        if (was_empty) {
            const val: u64 = 1;
            _ = posix.write(self.eventfd, std.mem.asBytes(&val)) catch {};
        }
    }

    /// Drain all pending items into the output list (called from io_uring thread)
    pub fn drainInto(self: *WsWriteQueue, out: *std.ArrayListUnmanaged(WsWriteRequest), out_allocator: std.mem.Allocator) void {
        self.mutex.lock();
        defer self.mutex.unlock();

        // Move all items to output
        out.appendSlice(out_allocator, self.items.items) catch return;
        self.items.clearRetainingCapacity();
    }
};

// Sharded WebSocket write queues (one per io_uring thread for parallelism)
var global_ws_write_queues: ?[]*WsWriteQueue = null;
var global_ws_num_queues: usize = 0;

// Socket to queue mapping - tracks which io_uring thread owns each WebSocket connection
var global_socket_queue_map: ?*std.AutoHashMap(posix.socket_t, *WsWriteQueue) = null;
var global_socket_queue_mutex: std.Thread.Mutex = .{};

/// Register a socket with its owning io_uring thread's queue
/// Called when WebSocket upgrade completes
pub fn registerSocketQueue(socket: posix.socket_t, queue: *WsWriteQueue) void {
    global_socket_queue_mutex.lock();
    defer global_socket_queue_mutex.unlock();
    if (global_socket_queue_map) |map| {
        map.put(socket, queue) catch {};
    }
}

/// Unregister a socket (called on connection close)
pub fn unregisterSocketQueue(socket: posix.socket_t) void {
    global_socket_queue_mutex.lock();
    defer global_socket_queue_mutex.unlock();
    if (global_socket_queue_map) |map| {
        _ = map.remove(socket);
    }
}

/// Queue a WebSocket write for io_uring processing
/// Called from worker threads, routed to the io_uring thread that owns the connection
pub fn queueWebSocketWrite(socket: posix.socket_t, data: []u8, allocator: std.mem.Allocator) !void {
    // Look up which queue owns this socket
    global_socket_queue_mutex.lock();
    const queue = if (global_socket_queue_map) |map| map.get(socket) else null;
    global_socket_queue_mutex.unlock();

    const target_queue = queue orelse blk: {
        // Fallback to first queue if not registered (shouldn't happen)
        const queues = global_ws_write_queues orelse return error.NotInitialized;
        if (queues.len == 0) return error.NotInitialized;
        break :blk queues[0];
    };

    try target_queue.push(.{
        .socket = socket,
        .data = data,
        .allocator = allocator,
    });
}

// HTTP Date header caching (updated once per second per thread)
const day_names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

threadlocal var cached_date: [29]u8 = undefined;
threadlocal var cached_timestamp: i64 = 0;

fn formatHttpDate(buf: []u8, timestamp: i64) void {
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

pub fn getHttpDate() []const u8 {
    const ts = posix.clock_gettime(.REALTIME) catch return &cached_date;
    if (ts.sec != cached_timestamp) {
        formatHttpDate(&cached_date, ts.sec);
        cached_timestamp = ts.sec;
    }
    return &cached_date;
}

// Threadlocal async poll state — accessible by handlers via getAsyncPollState()
threadlocal var tl_async_poll_state: ?*anyopaque = null;
threadlocal var tl_async_poll_ring: ?*IoUring = null;

/// Get the per-thread async poll state (e.g. DbConnPool pointer).
/// Returns null if async poll is not configured.
pub fn getAsyncPollState() ?*anyopaque {
    return tl_async_poll_state;
}

/// Get the per-thread io_uring ring. Used by async handlers to submit poll requests.
pub fn getAsyncPollRing() ?*IoUring {
    return tl_async_poll_ring;
}

// Threadlocal current connection pointer — set before calling handler so async
// handlers can reference the connection for DB callbacks.
threadlocal var tl_current_conn_ptr: usize = 0;

/// Get the current HTTP connection pointer (for use in async handler callbacks).
pub fn getCurrentConnPtr() usize {
    return tl_current_conn_ptr;
}

/// Server configuration
pub const Config = struct {
    /// Number of worker threads (default: CPU count)
    threads: ?usize = null,
    /// io_uring ring size (default: 4096)
    ring_size: u13 = 4096,
    /// CQE batch size for processing (default: 512)
    cqe_batch_size: usize = 512,
    /// Enable CPU affinity (pin threads to cores)
    cpu_affinity: bool = false,
    /// Enable SQPOLL mode (requires root)
    sqpoll: bool = false,
    /// Enable registered buffers for zero-copy I/O
    registered_buffers: bool = false,
    /// Maximum connections per thread
    max_connections: usize = 4096,
    /// Read buffer size per connection
    read_buffer_size: usize = 8192,
    /// Enable TCP_NODELAY
    tcp_nodelay: bool = true,
    /// Server name for Server header
    server_name: []const u8 = "oxelot-http",

    // Streaming multipart configuration
    /// Memory threshold for streaming uploads before spilling to disk (default: 1MB)
    streaming_threshold: usize = 1024 * 1024,
    /// Temp directory for streaming uploads (null = /tmp/oxelot-http-uploads)
    temp_dir: ?[]const u8 = null,
    /// Maximum upload size for streaming (default: 100MB)
    max_upload_size: usize = 100 * 1024 * 1024,
    /// Enable automatic streaming for large multipart uploads
    enable_streaming_multipart: bool = true,

    // Async DB poll support (generic callback mechanism)
    /// Per-thread init: called once per worker thread with the io_uring ring.
    /// Returns opaque per-thread state (e.g. a DbConnPool pointer).
    async_poll_init: ?*const fn (*IoUring) ?*anyopaque = null,
    /// Per-thread deinit: called when worker thread exits.
    async_poll_deinit: ?*const fn (*anyopaque) void = null,
    /// Handle a DB poll CQE. Receives the per-thread state, io_uring ring,
    /// the db_conn index from the tag, and a response writer.
    /// For each completed request, call writer.add(conn_ptr, response_data).
    async_poll_handler: ?*const fn (*anyopaque, *IoUring, u8, *AsyncPollResponseWriter) void = null,
};

/// Writer used by async poll callbacks to return responses
pub const AsyncPollResponseWriter = struct {
    items: [256]ResponseItem,
    count: usize,

    pub const ResponseItem = struct {
        conn_ptr: usize,
        data: []const u8,
    };

    pub fn init() AsyncPollResponseWriter {
        return .{
            .items = undefined,
            .count = 0,
        };
    }

    pub fn add(self: *AsyncPollResponseWriter, conn_ptr: usize, data: []const u8) void {
        if (self.count < 256) {
            self.items[self.count] = .{ .conn_ptr = conn_ptr, .data = data };
            self.count += 1;
        }
    }
};

const ConnectionState = enum {
    reading,
    reading_streaming_body, // Streaming multipart body
    writing,
    websocket, // Upgraded to WebSocket protocol
    waiting_for_db, // Async DB request pending
};

const Connection = struct {
    socket: posix.socket_t,
    read_buffer: []u8,
    read_len: usize,
    state: ConnectionState,
    parse_pos: usize,
    response_buffer: ?[]u8,
    response_owned: bool = true,
    buf_index: u16,
    // WebSocket fields
    ws_handler: ?WebSocketHandler = null,
    ws_codec: ?*websocket.Codec = null,
    // Streaming multipart fields
    streaming_parser: ?*multipart.StreamingMultipartParser = null,
    streaming_header_end: usize = 0,
    streaming_content_length: usize = 0,
    streaming_body_received: usize = 0,
};

const ConnectionPool = struct {
    allocator: std.mem.Allocator,
    slots: []Connection,
    buffers: []u8,
    free_list: std.ArrayListUnmanaged(u16),
    iovecs: []posix.iovec,
    buffer_size: usize,
    max_connections: usize,

    fn init(allocator: std.mem.Allocator, max_connections: usize, buffer_size: usize) !ConnectionPool {
        const slots = try allocator.alloc(Connection, max_connections);
        errdefer allocator.free(slots);

        // Allocate contiguous buffer for all connections
        const buffers = try allocator.alloc(u8, max_connections * buffer_size);
        errdefer allocator.free(buffers);

        var free_list: std.ArrayListUnmanaged(u16) = .empty;
        try free_list.ensureTotalCapacity(allocator, max_connections);

        // Initialize slots and free list
        for (0..max_connections) |i| {
            const idx: u16 = @intCast(i);
            const buf_start = i * buffer_size;
            slots[i] = .{
                .socket = -1,
                .read_buffer = buffers[buf_start..][0..buffer_size],
                .read_len = 0,
                .state = .reading,
                .parse_pos = 0,
                .response_buffer = null,
                .buf_index = idx,
            };
            try free_list.append(allocator, idx);
        }

        // Create iovecs for registered buffers
        const iovecs = try allocator.alloc(posix.iovec, max_connections);
        for (0..max_connections) |i| {
            const buf_start = i * buffer_size;
            iovecs[i] = .{
                .base = buffers[buf_start..].ptr,
                .len = buffer_size,
            };
        }

        return .{
            .allocator = allocator,
            .slots = slots,
            .buffers = buffers,
            .free_list = free_list,
            .iovecs = iovecs,
            .buffer_size = buffer_size,
            .max_connections = max_connections,
        };
    }

    fn deinit(self: *ConnectionPool) void {
        // Free any outstanding response buffers, WebSocket codecs, and streaming parsers
        for (self.slots) |*conn| {
            if (conn.response_buffer) |buf| {
                if (conn.response_owned) self.allocator.free(buf);
            }
            if (conn.ws_codec) |codec| {
                codec.deinit();
                self.allocator.destroy(codec);
            }
            if (conn.streaming_parser) |parser| {
                parser.deinit();
                self.allocator.destroy(parser);
            }
        }
        self.allocator.free(self.iovecs);
        self.free_list.deinit(self.allocator);
        self.allocator.free(self.buffers);
        self.allocator.free(self.slots);
    }

    fn alloc(self: *ConnectionPool) ?*Connection {
        if (self.free_list.pop()) |idx| {
            return &self.slots[idx];
        }
        return null;
    }

    fn free(self: *ConnectionPool, conn: *Connection) void {
        // Free response buffer if any
        if (conn.response_buffer) |buf| {
            if (conn.response_owned) self.allocator.free(buf);
            conn.response_buffer = null;
            conn.response_owned = true;
        }
        // Free WebSocket codec if any
        if (conn.ws_codec) |codec| {
            codec.deinit();
            self.allocator.destroy(codec);
            conn.ws_codec = null;
        }
        // Free streaming parser if any
        if (conn.streaming_parser) |parser| {
            parser.deinit();
            self.allocator.destroy(parser);
            conn.streaming_parser = null;
        }
        conn.ws_handler = null;
        conn.read_len = 0;
        conn.parse_pos = 0;
        conn.state = .reading;
        conn.streaming_header_end = 0;
        conn.streaming_content_length = 0;
        conn.streaming_body_received = 0;
        self.free_list.append(self.allocator, conn.buf_index) catch {};
    }
};

/// Pre-parsed HTTP request info from picohttpparser (avoids double-parsing)
const ParsedRequestInfo = struct {
    method: []const u8,
    path: []const u8,
    minor_version: c_int,
    headers: [64]pico.phr_header,
    num_headers: usize,
    header_end: usize,
    req_end: usize, // total request length including body
    content_length: usize, // 0 if no body
};

/// Parse HTTP request and return both the request boundary and the parsed info.
/// The parsed info can be passed to processRequest to avoid re-parsing.
fn parseAndFindRequestEnd(buffer: []const u8) ?ParsedRequestInfo {
    var info: ParsedRequestInfo = undefined;
    var method: [*c]const u8 = undefined;
    var method_len: usize = undefined;
    var path: [*c]const u8 = undefined;
    var path_len: usize = undefined;
    info.num_headers = 64;

    const ret = pico.phr_parse_request(
        buffer.ptr,
        buffer.len,
        &method,
        &method_len,
        &path,
        &path_len,
        &info.minor_version,
        &info.headers,
        &info.num_headers,
        0,
    );

    if (ret > 0) {
        const header_end: usize = @intCast(ret);
        info.header_end = header_end;
        info.method = method[0..method_len];
        info.path = path[0..path_len];
        info.content_length = 0;

        // Check for Content-Length header to include body in the request boundary
        for (info.headers[0..info.num_headers]) |h| {
            const name = h.name[0..h.name_len];
            if (std.ascii.eqlIgnoreCase(name, "Content-Length")) {
                const value = h.value[0..h.value_len];
                const content_len = std.fmt.parseInt(usize, value, 10) catch 0;
                if (content_len > 0) {
                    info.content_length = content_len;
                    const total_len = header_end + content_len;
                    if (buffer.len >= total_len) {
                        info.req_end = total_len;
                        return info;
                    }
                    return null; // Need more data
                }
                break;
            }
        }

        info.req_end = header_end;
        return info;
    }
    return null;
}

/// Result of detecting streaming multipart request
const StreamingDetection = struct {
    header_end: usize,
    content_length: usize,
    boundary: []const u8,
};

/// Detect if request should use streaming multipart mode
/// Returns detection info if streaming should be used, null otherwise
fn detectStreamingRequest(buffer: []const u8, config: Config) ?StreamingDetection {
    if (!config.enable_streaming_multipart) return null;

    // Quick check: streaming only applies to POST/PUT (skip GET/DELETE/etc.)
    if (buffer.len < 4) return null;
    if (buffer[0] != 'P') return null; // Not POST or PUT

    var method: [*c]const u8 = undefined;
    var method_len: usize = undefined;
    var path: [*c]const u8 = undefined;
    var path_len: usize = undefined;
    var minor_version: c_int = undefined;
    var headers: [64]pico.phr_header = undefined;
    var num_headers: usize = 64;

    const ret = pico.phr_parse_request(
        buffer.ptr,
        buffer.len,
        &method,
        &method_len,
        &path,
        &path_len,
        &minor_version,
        &headers,
        &num_headers,
        0,
    );

    if (ret <= 0) return null; // Incomplete or error

    const header_end: usize = @intCast(ret);

    var content_length: ?usize = null;
    var content_type: ?[]const u8 = null;

    // Extract Content-Length and Content-Type headers
    for (headers[0..num_headers]) |h| {
        const name = h.name[0..h.name_len];
        const value = h.value[0..h.value_len];

        if (std.ascii.eqlIgnoreCase(name, "Content-Length")) {
            content_length = std.fmt.parseInt(usize, value, 10) catch null;
        } else if (std.ascii.eqlIgnoreCase(name, "Content-Type")) {
            content_type = value;
        }
    }

    // Need both Content-Length and Content-Type
    const cl = content_length orelse return null;
    const ct = content_type orelse return null;

    // Must be multipart/form-data
    if (std.mem.indexOf(u8, ct, "multipart/form-data") == null) return null;

    // Must exceed streaming threshold
    if (cl <= config.streaming_threshold) return null;

    // Must not exceed max upload size
    if (cl > config.max_upload_size) return null;

    // Extract boundary
    const boundary = multipart.extractBoundary(ct) orelse return null;

    return .{
        .header_end = header_end,
        .content_length = cl,
        .boundary = boundary,
    };
}

/// Worker thread context
const WorkerContext = struct {
    thread_id: usize,
    address: u32,
    port: u16,
    config: Config,
    router: *Router,
    running: *std.atomic.Value(bool),
    ws_write_queue: ?*WsWriteQueue, // This thread's WebSocket write queue
};

/// HTTP Server
pub const Server = struct {
    allocator: std.mem.Allocator,
    router: *Router,
    address: u32,
    port: u16,
    config: Config,
    running: std.atomic.Value(bool),
    threads: std.ArrayListUnmanaged(std.Thread),
    ws_write_queues: ?[]*WsWriteQueue, // Sharded queues (one per thread)
    num_ws_queues: usize,
    socket_queue_map: ?*std.AutoHashMap(posix.socket_t, *WsWriteQueue),

    /// Initialize a new server
    pub fn init(allocator: std.mem.Allocator, router: *Router, config: Config) Server {
        // Queues are created in listen() after we know thread count
        return .{
            .allocator = allocator,
            .router = router,
            .address = 0,
            .port = 0,
            .config = config,
            .running = std.atomic.Value(bool).init(false),
            .threads = .empty,
            .ws_write_queues = null,
            .num_ws_queues = 0,
            .socket_queue_map = null,
        };
    }

    /// Deinitialize the server
    pub fn deinit(self: *Server) void {
        self.stop();
        self.threads.deinit(self.allocator);

        // Cleanup socket-to-queue map
        if (self.socket_queue_map) |map| {
            global_socket_queue_map = null;
            map.deinit();
            self.allocator.destroy(map);
        }

        // Cleanup WebSocket write queues
        if (self.ws_write_queues) |queues| {
            global_ws_write_queues = null;
            global_ws_num_queues = 0;
            for (queues) |queue| {
                queue.deinit();
                self.allocator.destroy(queue);
            }
            self.allocator.free(queues);
        }
    }

    /// Parse address string to u32
    fn parseAddress(address: []const u8) u32 {
        if (std.mem.eql(u8, address, "0.0.0.0") or address.len == 0) {
            return 0;
        }

        var result: u32 = 0;
        var part: u8 = 0;
        var shift: u5 = 0;

        for (address) |c| {
            if (c == '.') {
                result |= @as(u32, part) << shift;
                shift += 8;
                part = 0;
            } else if (c >= '0' and c <= '9') {
                part = part * 10 + (c - '0');
            }
        }
        result |= @as(u32, part) << shift;

        return result;
    }

    /// Start listening on the given address and port
    pub fn listen(self: *Server, address: []const u8, port: u16) !void {
        self.address = parseAddress(address);
        self.port = port;
        self.running.store(true, .seq_cst);

        const num_threads = self.config.threads orelse std.Thread.getCpuCount() catch 4;

        std.log.info("Starting oxelot-http server", .{});
        std.log.info("  Address: {s}:{}", .{ address, port });
        std.log.info("  Threads: {}", .{num_threads});
        std.log.info("  Ring size: {}", .{self.config.ring_size});

        // Create sharded WebSocket write queues (one per thread)
        const queues = try self.allocator.alloc(*WsWriteQueue, num_threads);
        errdefer self.allocator.free(queues);

        for (0..num_threads) |i| {
            const queue = try self.allocator.create(WsWriteQueue);
            errdefer self.allocator.destroy(queue);
            queue.* = try WsWriteQueue.init(self.allocator);
            queues[i] = queue;
        }

        // Create socket-to-queue mapping for routing writes to correct thread
        const socket_map = try self.allocator.create(std.AutoHashMap(posix.socket_t, *WsWriteQueue));
        socket_map.* = std.AutoHashMap(posix.socket_t, *WsWriteQueue).init(self.allocator);
        self.socket_queue_map = socket_map;
        global_socket_queue_map = socket_map;

        self.ws_write_queues = queues;
        self.num_ws_queues = num_threads;
        global_ws_write_queues = queues;
        global_ws_num_queues = num_threads;

        std.log.info("  WebSocket queues: {} (sharded)", .{num_threads});

        // Spawn worker threads (each gets its own queue)
        for (0..num_threads) |i| {
            const ctx = WorkerContext{
                .thread_id = i,
                .address = self.address,
                .port = port,
                .config = self.config,
                .router = self.router,
                .running = &self.running,
                .ws_write_queue = queues[i],
            };
            const thread = try std.Thread.spawn(.{}, workerThread, .{ctx});
            try self.threads.append(self.allocator, thread);
        }

        std.log.info("Server ready on {s}:{}", .{ address, port });
    }

    /// Block until server stops
    pub fn wait(self: *Server) void {
        for (self.threads.items) |thread| {
            thread.join();
        }
    }

    /// Stop the server
    pub fn stop(self: *Server) void {
        self.running.store(false, .seq_cst);
        // Threads will exit on next io_uring timeout
    }

    /// Listen and block (convenience method)
    pub fn run(self: *Server, address: []const u8, port: u16) !void {
        try self.listen(address, port);
        self.wait();
    }
};

/// Worker thread function
fn workerThread(ctx: WorkerContext) void {
    const allocator = std.heap.c_allocator;

    // Set CPU affinity if enabled
    if (ctx.config.cpu_affinity) {
        var cpu_set: std.os.linux.cpu_set_t = std.mem.zeroes(std.os.linux.cpu_set_t);
        const core_id = ctx.thread_id % 64;
        cpu_set[0] = @as(usize, 1) << @intCast(core_id);
        std.os.linux.sched_setaffinity(0, &cpu_set) catch {};
    }

    // Create listening socket with SO_REUSEPORT
    const listen_sock = posix.socket(
        posix.AF.INET,
        posix.SOCK.STREAM | posix.SOCK.NONBLOCK,
        posix.IPPROTO.TCP,
    ) catch |err| {
        std.log.err("Thread {}: Failed to create socket: {}", .{ ctx.thread_id, err });
        return;
    };
    defer posix.close(listen_sock);

    // Socket options
    posix.setsockopt(listen_sock, posix.SOL.SOCKET, posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1))) catch {};
    posix.setsockopt(listen_sock, posix.SOL.SOCKET, posix.SO.REUSEPORT, &std.mem.toBytes(@as(c_int, 1))) catch {};

    const addr: posix.sockaddr.in = .{
        .port = @byteSwap(ctx.port),
        .addr = @bitCast([4]u8{
            @truncate(ctx.address),
            @truncate(ctx.address >> 8),
            @truncate(ctx.address >> 16),
            @truncate(ctx.address >> 24),
        }),
        .zero = [_]u8{0} ** 8,
    };

    posix.bind(listen_sock, @ptrCast(&addr), @sizeOf(posix.sockaddr.in)) catch |err| {
        std.log.err("Thread {}: Failed to bind: {}", .{ ctx.thread_id, err });
        return;
    };
    posix.listen(listen_sock, 4096) catch |err| {
        std.log.err("Thread {}: Failed to listen: {}", .{ ctx.thread_id, err });
        return;
    };

    // Initialize io_uring
    const flags: u32 = if (ctx.config.sqpoll) linux.IORING_SETUP_SQPOLL else 0;
    var ring = IoUring.init(ctx.config.ring_size, flags) catch |err| {
        if (ctx.config.sqpoll) {
            std.log.warn("Thread {}: SQPOLL failed, falling back: {}", .{ ctx.thread_id, err });
            var fallback_ring = IoUring.init(ctx.config.ring_size, 0) catch |err2| {
                std.log.err("Thread {}: Failed to init io_uring: {}", .{ ctx.thread_id, err2 });
                return;
            };
            runEventLoop(ctx, allocator, &fallback_ring, listen_sock);
            return;
        }
        std.log.err("Thread {}: Failed to init io_uring: {}", .{ ctx.thread_id, err });
        return;
    };

    runEventLoop(ctx, allocator, &ring, listen_sock);
}

fn runEventLoop(ctx: WorkerContext, allocator: std.mem.Allocator, ring: *IoUring, listen_sock: posix.socket_t) void {
    defer ring.deinit();

    // Initialize connection pool
    var pool = ConnectionPool.init(
        allocator,
        ctx.config.max_connections,
        ctx.config.read_buffer_size,
    ) catch {
        std.log.err("Thread {}: Failed to init connection pool", .{ctx.thread_id});
        return;
    };
    defer pool.deinit();

    // Register buffers if enabled
    if (ctx.config.registered_buffers) {
        ring.register_buffers(pool.iovecs) catch |err| {
            std.log.warn("Thread {}: Failed to register buffers: {}", .{ ctx.thread_id, err });
        };
    }

    // Connection tracking
    var connections = std.AutoHashMap(posix.socket_t, *Connection).init(allocator);
    defer {
        var it = connections.valueIterator();
        while (it.next()) |conn| {
            posix.close(conn.*.socket);
            pool.free(conn.*);
        }
        connections.deinit();
    }

    // Initialize async poll state (e.g. per-thread DbConnPool)
    var async_poll_state: ?*anyopaque = null;
    if (ctx.config.async_poll_init) |init_fn| {
        async_poll_state = init_fn(ring);
    }
    defer {
        if (async_poll_state) |state| {
            if (ctx.config.async_poll_deinit) |deinit_fn| {
                deinit_fn(state);
            }
        }
    }

    // Store async poll state in threadlocal for handler access
    tl_async_poll_state = async_poll_state;
    tl_async_poll_ring = ring;

    // Submit initial accept
    _ = ring.accept(ACCEPT_TAG, listen_sock, null, null, 0) catch return;

    // Register eventfd for this thread's WebSocket write queue
    const ws_queue = ctx.ws_write_queue;
    if (ws_queue) |queue| {
        _ = ring.poll_add(EVENTFD_TAG, queue.eventfd, linux.POLL.IN) catch |err| {
            std.log.warn("Thread {}: Failed to register WebSocket eventfd: {}", .{ ctx.thread_id, err });
        };
    }

    _ = ring.submit() catch return;

    var cqe_buf = allocator.alloc(linux.io_uring_cqe, ctx.config.cqe_batch_size) catch return;
    defer allocator.free(cqe_buf);

    std.log.debug("Thread {} ready", .{ctx.thread_id});

    while (ctx.running.load(.seq_cst)) {
        const count = ring.copy_cqes(cqe_buf, 1) catch |err| {
            if (err == error.SignalInterrupt) continue;
            continue;
        };

        if (count == 0) continue;

        for (cqe_buf[0..count]) |cqe| {
            const user_data = cqe.user_data;
            const is_db_poll = (user_data & DB_POLL_TAG_BIT) != 0 and (user_data & WRITE_TAG_BIT) == 0 and (user_data & WS_WRITE_TAG_BIT) == 0;
            const is_ws_write = (user_data & WS_WRITE_TAG_BIT) != 0;
            const is_write = (user_data & WRITE_TAG_BIT) != 0;
            const conn_ptr = user_data & ~(WRITE_TAG_BIT | WS_WRITE_TAG_BIT | DB_POLL_TAG_BIT);

            if (user_data == ACCEPT_TAG) {
                handleAccept(ring, &pool, &connections, listen_sock, cqe, ctx.config) catch {};
            } else if (user_data == EVENTFD_TAG) {
                // WebSocket write queue has pending items
                handleWebSocketWriteQueue(ring, &connections, allocator, ws_queue) catch {};
            } else if (is_db_poll) {
                // Async DB poll — delegate to application callback
                if (ctx.config.async_poll_handler) |handler| {
                    if (async_poll_state) |state| {
                        const db_conn_idx: u8 = @intCast(conn_ptr & 0xFF);
                        var responses = AsyncPollResponseWriter.init();
                        handler(state, ring, db_conn_idx, &responses);

                        // Write responses back to HTTP connections
                        for (responses.items[0..responses.count]) |item| {
                            const http_conn: *Connection = @ptrFromInt(item.conn_ptr);
                            // Verify connection is still valid and waiting for DB
                            if (connections.get(http_conn.socket)) |stored_conn| {
                                if (@intFromPtr(stored_conn) == item.conn_ptr and stored_conn.state == .waiting_for_db) {
                                    stored_conn.state = .writing;
                                    // The response data must be heap-allocated by the callback
                                    stored_conn.response_buffer = @constCast(item.data);
                                    const write_tag = @intFromPtr(stored_conn) | WRITE_TAG_BIT;
                                    _ = ring.send(write_tag, stored_conn.socket, item.data, 0) catch {
                                        _ = connections.remove(stored_conn.socket);
                                        posix.close(stored_conn.socket);
                                        pool.free(stored_conn);
                                    };
                                }
                            }
                        }
                    }
                }
            } else if (is_ws_write) {
                // WebSocket write completion - just free the context
                const ws_ctx: *WsWriteContext = @ptrFromInt(conn_ptr);
                ws_ctx.deinit(allocator);
            } else if (is_write) {
                handleWrite(ring, &pool, &connections, conn_ptr, cqe, ctx.router, allocator, ws_queue) catch {};
            } else {
                handleRead(ring, &pool, &connections, conn_ptr, cqe, ctx.router, allocator, ctx.config) catch {};
            }
        }

        _ = ring.submit() catch {};
    }

    // Clear threadlocal before exit
    tl_async_poll_state = null;
    tl_async_poll_ring = null;

    std.log.debug("Thread {} stopped", .{ctx.thread_id});
}

/// Handle WebSocket write queue - drain pending writes and submit via io_uring
fn handleWebSocketWriteQueue(
    ring: *IoUring,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    allocator: std.mem.Allocator,
    ws_queue: ?*WsWriteQueue,
) !void {
    const queue = ws_queue orelse return;

    // Consume the eventfd value to clear it
    var buf: [8]u8 = undefined;
    _ = posix.read(queue.eventfd, &buf) catch {};

    // Drain pending writes
    var pending: std.ArrayListUnmanaged(WsWriteRequest) = .empty;
    defer pending.deinit(allocator);
    queue.drainInto(&pending, allocator);

    // Submit each write via io_uring
    // Each write gets its own WsWriteContext to avoid race conditions
    for (pending.items) |req| {
        // Check if connection still exists
        if (connections.get(req.socket) == null) {
            // Connection closed, free the data
            req.allocator.free(req.data);
            continue;
        }

        // Create write context (holds buffer ownership)
        const ctx = allocator.create(WsWriteContext) catch {
            req.allocator.free(req.data);
            continue;
        };
        ctx.* = .{
            .data = req.data,
            .allocator = req.allocator,
        };

        // Submit write via io_uring with WS_WRITE_TAG_BIT
        const write_tag = @intFromPtr(ctx) | WS_WRITE_TAG_BIT;
        _ = ring.send(write_tag, req.socket, req.data, 0) catch {
            ctx.deinit(allocator);
            continue;
        };
    }

    // Re-arm the eventfd poll
    _ = ring.poll_add(EVENTFD_TAG, queue.eventfd, linux.POLL.IN) catch {};
}

fn handleAccept(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    listen_sock: posix.socket_t,
    cqe: linux.io_uring_cqe,
    config: Config,
) !void {
    // Always queue another accept
    _ = try ring.accept(ACCEPT_TAG, listen_sock, null, null, 0);

    if (cqe.res < 0) return;

    const client_sock: posix.socket_t = cqe.res;

    // TCP_NODELAY
    if (config.tcp_nodelay) {
        posix.setsockopt(
            client_sock,
            posix.IPPROTO.TCP,
            posix.TCP.NODELAY,
            &std.mem.toBytes(@as(c_int, 1)),
        ) catch {};
    }

    // Allocate connection
    const conn = pool.alloc() orelse {
        posix.close(client_sock);
        return;
    };
    conn.socket = client_sock;
    conn.read_len = 0;
    conn.state = .reading;
    conn.parse_pos = 0;

    try connections.put(client_sock, conn);

    // Submit read
    const read_tag = @intFromPtr(conn);
    const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer };
    _ = ring.recv(read_tag, client_sock, buffer, 0) catch {
        _ = connections.remove(client_sock);
        pool.free(conn);
        posix.close(client_sock);
    };
}

fn handleWrite(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    conn_ptr: u64,
    cqe: linux.io_uring_cqe,
    router: *Router,
    allocator: std.mem.Allocator,
    ws_queue: ?*WsWriteQueue,
) !void {
    const conn: *Connection = @ptrFromInt(conn_ptr);

    if (cqe.res < 0) {
        // Unregister socket from queue mapping on close
        unregisterSocketQueue(conn.socket);
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
        return;
    }

    // Free previous response buffer (only if we own it)
    if (conn.response_buffer) |buf| {
        if (conn.response_owned) {
            pool.allocator.free(buf);
        }
        conn.response_buffer = null;
        conn.response_owned = true;
    }

    // Check if this was a WebSocket upgrade - transition to WebSocket mode
    // Only do this once on the initial upgrade response, not on subsequent WebSocket writes
    if (conn.ws_handler != null and conn.ws_codec != null and conn.state != .websocket) {
        conn.state = .websocket;
        conn.read_len = 0;
        conn.parse_pos = 0;

        // Register this socket with its owning queue for write routing
        if (ws_queue) |queue| {
            registerSocketQueue(conn.socket, queue);
        }

        // Call onOpen handler
        if (conn.ws_handler.?.onOpen) |onOpen| {
            var ws_conn = websocket.Connection.init(allocator, conn.socket);
            onOpen(&ws_conn);
        }

        // Queue read for WebSocket frames
        const read_tag = @intFromPtr(conn);
        const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer };
        _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
        return;
    }

    // For WebSocket data writes, just return - the read loop handles everything
    if (conn.state == .websocket) {
        return;
    }

    // Handle pipelining - process all available requests and batch responses
    if (conn.parse_pos > 0 and conn.parse_pos < conn.read_len) {
        const remaining = conn.read_len - conn.parse_pos;
        std.mem.copyForwards(u8, conn.read_buffer[0..remaining], conn.read_buffer[conn.parse_pos..conn.read_len]);
        conn.read_len = remaining;
        conn.parse_pos = 0;

        const batched = processPipelinedRequests(allocator, router, conn);
        if (batched) |response_data| {
            conn.state = .writing;
            conn.response_buffer = response_data;
            const write_tag = @intFromPtr(conn) | WRITE_TAG_BIT;
            _ = ring.send(write_tag, conn.socket, response_data, 0) catch {
                _ = connections.remove(conn.socket);
                posix.close(conn.socket);
                pool.free(conn);
            };
            return;
        }
    } else {
        conn.read_len = 0;
        conn.parse_pos = 0;
    }

    conn.state = .reading;

    // Queue next read
    const read_tag = @intFromPtr(conn);
    const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer[conn.read_len..] };
    _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
    };
}

fn handleWebSocketRead(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    conn: *Connection,
    allocator: std.mem.Allocator,
) void {
    const codec = conn.ws_codec orelse return;
    const handler = conn.ws_handler orelse return;

    var ws_conn = websocket.Connection.init(allocator, conn.socket);

    // Process all complete frames in the buffer
    var offset: usize = 0;
    while (offset < conn.read_len) {
        const frame_result = codec.parseFrame(conn.read_buffer[offset..conn.read_len]) catch |err| {
            // Protocol error - close connection
            if (handler.onError) |onError| {
                onError(&ws_conn, err);
            }
            ws_conn.close(.protocol_error, "Frame parse error") catch {};
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
            return;
        };

        if (frame_result == null) {
            // Incomplete frame, need more data
            break;
        }

        var frame = frame_result.?.frame;
        offset += frame_result.?.consumed;

        // Handle different frame types
        switch (frame.header.opcode) {
            .ping => {
                // Send pong with same payload
                ws_conn.sendPong(frame.payload) catch {};
                codec.freeFrame(&frame);
            },
            .pong => {
                // Pong received, no action needed
                codec.freeFrame(&frame);
            },
            .close => {
                // Close handshake received
                const close_code = frame.closeCode();
                const close_reason = frame.closeReason();

                // Call onClose handler
                if (handler.onClose) |onClose| {
                    onClose(&ws_conn, close_code, close_reason);
                }

                // Send close frame back
                ws_conn.close(close_code orelse .normal, close_reason) catch {};

                // Free frame before freeing connection (which frees codec)
                codec.freeFrame(&frame);

                // Unregister socket from queue mapping before closing
                unregisterSocketQueue(conn.socket);

                _ = connections.remove(conn.socket);
                posix.close(conn.socket);
                pool.free(conn);
                return;
            },
            .text, .binary, .continuation => {
                // Process the frame through the codec to handle fragmentation
                const message = codec.processFrame(frame) catch |err| {
                    if (handler.onError) |onError| {
                        onError(&ws_conn, err);
                    }
                    codec.freeFrame(&frame);
                    continue;
                };

                codec.freeFrame(&frame);

                if (message) |msg| {
                    // Complete message received
                    defer {
                        var m = msg;
                        m.deinit();
                    }

                    if (handler.onMessage) |onMessage| {
                        onMessage(&ws_conn, msg.getPayload(), msg.opcode);
                    }
                }
            },
        }
    }

    // Shift remaining data to start of buffer
    if (offset > 0 and offset < conn.read_len) {
        const remaining = conn.read_len - offset;
        std.mem.copyForwards(u8, conn.read_buffer[0..remaining], conn.read_buffer[offset..conn.read_len]);
        conn.read_len = remaining;
    } else if (offset >= conn.read_len) {
        conn.read_len = 0;
    }

    // Queue next read
    const read_tag = @intFromPtr(conn);
    const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer[conn.read_len..] };
    _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
    };
}

fn handleRead(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    conn_ptr: u64,
    cqe: linux.io_uring_cqe,
    router: *Router,
    allocator: std.mem.Allocator,
    config: Config,
) !void {
    const conn: *Connection = @ptrFromInt(conn_ptr);

    if (cqe.res <= 0) {
        // Connection closed - notify WebSocket handler if in WebSocket mode
        if (conn.state == .websocket) {
            // Unregister socket from queue mapping
            unregisterSocketQueue(conn.socket);

            if (conn.ws_handler) |handler| {
                if (handler.onClose) |onClose| {
                    var ws_conn = websocket.Connection.init(allocator, conn.socket);
                    onClose(&ws_conn, .abnormal, null);
                }
            }
        }
        // Cleanup streaming parser if present
        if (conn.streaming_parser) |parser| {
            parser.deinit();
            allocator.destroy(parser);
            conn.streaming_parser = null;
        }
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
        return;
    }

    const bytes_read: usize = @intCast(cqe.res);
    conn.read_len += bytes_read;

    // Handle WebSocket mode
    if (conn.state == .websocket) {
        handleWebSocketRead(ring, pool, connections, conn, allocator);
        return;
    }

    // Handle streaming multipart body mode
    if (conn.state == .reading_streaming_body) {
        handleStreamingBodyRead(ring, pool, connections, conn, cqe, router, allocator, config) catch {};
        return;
    }

    // Check for streaming multipart request before checking for complete request
    // This allows us to start streaming before the full body arrives
    if (detectStreamingRequest(conn.read_buffer[0..conn.read_len], config)) |detection| {
        // Initialize streaming parser
        const streaming_config = multipart.StreamingConfig{
            .memory_threshold = config.streaming_threshold,
            .max_upload_size = config.max_upload_size,
            .temp_dir = config.temp_dir,
        };

        const parser = allocator.create(multipart.StreamingMultipartParser) catch {
            sendErrorResponse(ring, pool, connections, conn, 500, "Internal Server Error");
            return;
        };

        parser.* = multipart.StreamingMultipartParser.init(allocator, detection.boundary, streaming_config) catch {
            allocator.destroy(parser);
            sendErrorResponse(ring, pool, connections, conn, 500, "Internal Server Error");
            return;
        };

        conn.streaming_parser = parser;
        conn.streaming_header_end = detection.header_end;
        conn.streaming_content_length = detection.content_length;
        conn.streaming_body_received = 0;
        conn.state = .reading_streaming_body;

        // Feed any body data we already have
        if (conn.read_len > detection.header_end) {
            const initial_body = conn.read_buffer[detection.header_end..conn.read_len];
            const bytes_to_feed = @min(initial_body.len, detection.content_length);

            if (bytes_to_feed > 0) {
                const consumed = parser.feed(initial_body[0..bytes_to_feed]) catch |err| {
                    std.log.warn("Streaming parser error: {}", .{err});
                    sendErrorResponse(ring, pool, connections, conn, 400, "Bad Request");
                    return;
                };
                conn.streaming_body_received += consumed;
            }
        }

        // Check if we already have all the data
        if (conn.streaming_body_received >= detection.content_length) {
            parser.finish() catch |err| {
                std.log.warn("Streaming parser finish error: {}", .{err});
                sendErrorResponse(ring, pool, connections, conn, 400, "Bad Request");
                return;
            };

            conn.state = .writing;

            const result = processStreamingRequest(allocator, router, conn, config) catch |err| {
                std.log.warn("Streaming request processing error: {}", .{err});
                sendErrorResponse(ring, pool, connections, conn, 500, "Internal Server Error");
                return;
            };

            conn.response_buffer = result;

            const write_tag = @intFromPtr(conn) | WRITE_TAG_BIT;
            _ = ring.send(write_tag, conn.socket, result, 0) catch {
                if (conn.streaming_parser) |p| {
                    p.deinit();
                    allocator.destroy(p);
                    conn.streaming_parser = null;
                }
                _ = connections.remove(conn.socket);
                posix.close(conn.socket);
                pool.free(conn);
            };
            return;
        }

        // Need more data - queue another read
        const read_tag = @intFromPtr(conn);
        // Reset buffer to only keep headers
        conn.read_len = detection.header_end;
        const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer[conn.read_len..] };
        _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
            if (conn.streaming_parser) |p| {
                p.deinit();
                allocator.destroy(p);
                conn.streaming_parser = null;
            }
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
        return;
    }

    // Check for complete request(s) (non-streaming) — batch pipelined responses
    if (processPipelinedRequests(allocator, router, conn)) |response_data| {
        conn.state = .writing;
        conn.response_buffer = response_data;
        const write_tag = @intFromPtr(conn) | WRITE_TAG_BIT;
        _ = ring.send(write_tag, conn.socket, response_data, 0) catch {
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
    } else if (conn.state == .waiting_for_db) {
        // Handler submitted async DB request — don't queue read, wait for poll callback
        return;
    } else if (conn.read_len < conn.read_buffer.len) {
        // Need more data
        const read_tag = @intFromPtr(conn);
        const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer[conn.read_len..] };
        _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
    } else {
        // Buffer full, request too large
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
    }
}

/// Process all complete pipelined requests in the connection buffer.
/// Returns a single batched response buffer, or null on failure.
/// Updates conn.parse_pos and conn.read_len to reflect consumed data.
fn processPipelinedRequests(allocator: std.mem.Allocator, router: *Router, conn: *Connection) ?[]u8 {
    const ResponseEntry = struct {
        data: []const u8,
        owned: bool, // true = heap-allocated (must free), false = zero-copy slice
    };

    // Process all complete requests, collecting response slices
    var responses: [32]ResponseEntry = undefined;
    var num_responses: usize = 0;
    var total_len: usize = 0;
    var offset: usize = 0;

    // Set current connection pointer for async handlers
    tl_current_conn_ptr = @intFromPtr(conn);

    while (num_responses < 32) {
        const buf = conn.read_buffer[offset..conn.read_len];
        if (buf.len == 0) break;

        const info = parseAndFindRequestEnd(buf) orelse break;

        const result = processRequestPreparsed(allocator, router, buf[0..info.req_end], &info) catch {
            // On error, send error for this request and stop batching
            const err_resp = "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 21\r\n\r\nInternal Server Error";
            if (num_responses == 0) {
                // First request failed — return a copy of the error
                const buf2 = allocator.alloc(u8, err_resp.len) catch return null;
                @memcpy(buf2, err_resp);
                offset += info.req_end;
                conn.parse_pos = offset;
                return buf2;
            }
            break;
        };

        switch (result) {
            .response => |response_data| {
                responses[num_responses] = .{ .data = response_data, .owned = true };
                total_len += response_data.len;
                num_responses += 1;
                offset += info.req_end;
            },
            .raw_response => |raw_data| {
                responses[num_responses] = .{ .data = raw_data, .owned = false };
                total_len += raw_data.len;
                num_responses += 1;
                offset += info.req_end;
            },
            .websocket_upgrade => |upgrade| {
                // WebSocket upgrade can't be batched — if it's the first request,
                // handle it directly; otherwise stop batching here
                if (num_responses == 0) {
                    responses[0] = .{ .data = upgrade.response, .owned = true };
                    total_len = upgrade.response.len;
                    num_responses = 1;
                    offset += info.req_end;
                } else {
                    allocator.free(upgrade.response);
                }
                break;
            },
            .async_pending => {
                // Handler submitted an async DB operation.
                // Set connection to waiting_for_db — response will come via poll callback.
                offset += info.req_end;
                conn.parse_pos = offset;
                conn.state = .waiting_for_db;
                if (num_responses > 0) {
                    for (responses[0..num_responses]) |r| {
                        if (r.owned) allocator.free(@constCast(r.data));
                    }
                }
                return null;
            },
        }
    }

    if (num_responses == 0) return null;

    conn.parse_pos = offset;

    // Fast path: single response
    if (num_responses == 1) {
        if (responses[0].owned) {
            return @constCast(responses[0].data);
        }
        // Zero-copy: return the raw slice directly without allocating
        conn.response_owned = false;
        return @constCast(responses[0].data);
    }

    // Batch: concatenate all responses into one buffer
    const batched = allocator.alloc(u8, total_len) catch {
        for (responses[0..num_responses]) |r| {
            if (r.owned) allocator.free(@constCast(r.data));
        }
        return null;
    };
    var pos: usize = 0;
    for (responses[0..num_responses]) |r| {
        @memcpy(batched[pos..][0..r.data.len], r.data);
        pos += r.data.len;
        if (r.owned) allocator.free(@constCast(r.data));
    }
    return batched;
}

/// Result of processing a request
const RequestResult = union(enum) {
    /// Normal HTTP response (heap-allocated, caller must free)
    response: []u8,
    /// Raw response (points to threadlocal buffer, zero-copy — do NOT free)
    raw_response: []const u8,
    /// WebSocket upgrade - contains the upgrade response and handler
    websocket_upgrade: struct {
        response: []u8,
        handler: WebSocketHandler,
    },
    /// Async operation pending — don't send response yet
    async_pending: void,
};

/// Process an HTTP request through the router and return response bytes or upgrade
fn processRequest(allocator: std.mem.Allocator, router: *Router, request_data: []const u8) !RequestResult {
    // Parse the request (fallback path — used when no pre-parsed info available)
    var request = (try parseRequest(allocator, request_data)) orelse return error.ParseError;
    defer request.deinit();
    return processRequestWithParsed(allocator, router, request_data, &request);
}

fn processRequestPreparsed(allocator: std.mem.Allocator, router: *Router, request_data: []const u8, info: *const ParsedRequestInfo) !RequestResult {
    // Build Request from pre-parsed picohttpparser results (avoids re-parsing)
    var request = Request.initFromParsed(allocator, info.method, info.path, info.minor_version, info.headers[0..info.num_headers], if (info.content_length > 0) request_data[info.header_end..][0..info.content_length] else null);
    defer request.deinit();
    return processRequestWithParsed(allocator, router, request_data, &request);
}

fn processRequestWithParsed(allocator: std.mem.Allocator, router: *Router, request_data: []const u8, request: *Request) !RequestResult {
    _ = request_data;

    // Check for WebSocket upgrade request
    if (request.method == .GET) {
        const HeaderLookup = struct {
            req: *Request,
            pub fn get(self: @This(), name: []const u8) ?[]const u8 {
                return self.req.header(name);
            }
        };
        const lookup = HeaderLookup{ .req = request };

        const upgrade_check = websocket.isUpgradeRequest(lookup);
        if (upgrade_check.is_upgrade) {
            if (router.findWebSocketRoute(request.path)) |ws_handler| {
                var response_buf: [256]u8 = undefined;
                const response_len = websocket.generateUpgradeResponse(
                    upgrade_check.client_key.?,
                    &response_buf,
                );

                const response_data = try allocator.alloc(u8, response_len);
                @memcpy(response_data, response_buf[0..response_len]);

                return .{ .websocket_upgrade = .{
                    .response = response_data,
                    .handler = ws_handler,
                } };
            }
        }
    }

    // Normal HTTP request
    var response = Response.init(allocator);
    defer response.deinit();

    try router.handle(request, &response);

    // Check if handler deferred response (async DB operation pending)
    if (response.async_pending) {
        return .{ .async_pending = {} };
    }

    // Raw response path — handler already built complete HTTP response
    if (response.raw_response) |raw| {
        // Return as zero-copy slice; caller must check response_owned
        return .{ .raw_response = raw };
    }

    // Normal path — add server headers and serialize
    _ = response.header("Server", "oxelot-http");
    _ = response.header("Date", getHttpDate());
    return .{ .response = try response.serialize(allocator) };
}

/// Handle read completion in streaming multipart mode
fn handleStreamingBodyRead(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    conn: *Connection,
    cqe: linux.io_uring_cqe,
    router: *Router,
    allocator: std.mem.Allocator,
    config: Config,
) !void {
    if (cqe.res <= 0) {
        // Connection closed - cleanup streaming parser
        if (conn.streaming_parser) |parser| {
            parser.deinit();
            allocator.destroy(parser);
            conn.streaming_parser = null;
        }
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
        return;
    }

    const bytes_read: usize = @intCast(cqe.res);
    conn.read_len += bytes_read;

    // Feed the newly received data to the streaming parser
    const parser = conn.streaming_parser orelse {
        // Should not happen, but handle gracefully
        std.log.err("Streaming parser missing in streaming body state", .{});
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
        return;
    };

    // Calculate how much new body data we have
    const body_start = conn.streaming_header_end;
    if (conn.read_len > body_start) {
        const new_body_data = conn.read_buffer[body_start..conn.read_len];
        const bytes_to_feed = @min(new_body_data.len, conn.streaming_content_length - conn.streaming_body_received);

        if (bytes_to_feed > 0) {
            const consumed = parser.feed(new_body_data[0..bytes_to_feed]) catch |err| {
                std.log.warn("Streaming parser error: {}", .{err});
                sendErrorResponse(ring, pool, connections, conn, 400, "Bad Request");
                return;
            };
            conn.streaming_body_received += consumed;

            // Shift buffer if needed - we've consumed some data
            if (consumed > 0 and consumed < new_body_data.len) {
                const remaining = new_body_data.len - consumed;
                std.mem.copyForwards(u8, conn.read_buffer[body_start..], new_body_data[consumed..]);
                conn.read_len = body_start + remaining;
            } else if (consumed == new_body_data.len) {
                conn.read_len = body_start;
            }
        }
    }

    // Check if we have received all body data
    if (conn.streaming_body_received >= conn.streaming_content_length) {
        // Finish parsing
        parser.finish() catch |err| {
            std.log.warn("Streaming parser finish error: {}", .{err});
            sendErrorResponse(ring, pool, connections, conn, 400, "Bad Request");
            return;
        };

        conn.state = .writing;

        // Process the streaming request
        const result = processStreamingRequest(allocator, router, conn, config) catch |err| {
            std.log.warn("Streaming request processing error: {}", .{err});
            sendErrorResponse(ring, pool, connections, conn, 500, "Internal Server Error");
            return;
        };

        conn.response_buffer = result;

        // Queue write
        const write_tag = @intFromPtr(conn) | WRITE_TAG_BIT;
        _ = ring.send(write_tag, conn.socket, result, 0) catch {
            if (conn.streaming_parser) |p| {
                p.deinit();
                allocator.destroy(p);
                conn.streaming_parser = null;
            }
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
    } else if (conn.read_len < conn.read_buffer.len) {
        // Need more data
        const read_tag = @intFromPtr(conn);
        const buffer = IoUring.RecvBuffer{ .buffer = conn.read_buffer[conn.read_len..] };
        _ = ring.recv(read_tag, conn.socket, buffer, 0) catch {
            if (conn.streaming_parser) |p| {
                p.deinit();
                allocator.destroy(p);
                conn.streaming_parser = null;
            }
            _ = connections.remove(conn.socket);
            posix.close(conn.socket);
            pool.free(conn);
        };
    } else {
        // Buffer full - this shouldn't happen with proper streaming, but handle gracefully
        std.log.warn("Buffer full during streaming multipart read", .{});
        sendErrorResponse(ring, pool, connections, conn, 413, "Request Entity Too Large");
    }
}

/// Helper to send error response and cleanup connection
fn sendErrorResponse(
    ring: *IoUring,
    pool: *ConnectionPool,
    connections: *std.AutoHashMap(posix.socket_t, *Connection),
    conn: *Connection,
    status_code: u16,
    message: []const u8,
) void {
    // Cleanup streaming parser if present
    if (conn.streaming_parser) |parser| {
        parser.deinit();
        pool.allocator.destroy(parser);
        conn.streaming_parser = null;
    }

    // Format error response
    var buf: [256]u8 = undefined;
    const response = std.fmt.bufPrint(&buf, "HTTP/1.1 {} {s}\r\nContent-Length: {}\r\n\r\n{s}", .{
        status_code,
        message,
        message.len,
        message,
    }) catch {
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
        return;
    };

    conn.state = .writing;
    const write_tag = @intFromPtr(conn) | WRITE_TAG_BIT;
    _ = ring.send(write_tag, conn.socket, response, 0) catch {
        _ = connections.remove(conn.socket);
        posix.close(conn.socket);
        pool.free(conn);
    };
}

/// Process a streaming multipart request
fn processStreamingRequest(
    allocator: std.mem.Allocator,
    router: *Router,
    conn: *Connection,
    config: Config,
) ![]u8 {
    _ = config;

    // Parse the headers portion to create the request
    const header_data = conn.read_buffer[0..conn.streaming_header_end];
    var request = (try parseRequest(allocator, header_data)) orelse return error.ParseError;
    defer request.deinit();

    // Attach the streaming parser to the request
    request.streaming_parser = conn.streaming_parser;

    // Create response
    var response = Response.init(allocator);
    defer response.deinit();

    // Add server headers
    _ = response.header("Server", "oxelot-http");
    _ = response.header("Date", getHttpDate());

    // Route the request
    try router.handle(&request, &response);

    // Cleanup streaming parser after handler returns
    if (conn.streaming_parser) |parser| {
        parser.deinit();
        allocator.destroy(parser);
        conn.streaming_parser = null;
    }

    return try response.serialize(allocator);
}

test "server config defaults" {
    const config = Config{};
    try std.testing.expectEqual(@as(u13, 4096), config.ring_size);
    try std.testing.expectEqual(@as(usize, 512), config.cqe_batch_size);
    try std.testing.expect(config.tcp_nodelay);
}

test "address parsing" {
    try std.testing.expectEqual(@as(u32, 0), Server.parseAddress("0.0.0.0"));
    try std.testing.expectEqual(@as(u32, 0x0100007F), Server.parseAddress("127.0.0.1"));
}
