// Async PostgreSQL connection pool for io_uring integration.
//
// Each io_uring worker thread owns a DbConnPool with N non-blocking PGconn
// instances. HTTP handlers submit DbRequests which are queued, sent via
// PQsendQueryPrepared, and polled via io_uring poll_add. When PostgreSQL
// responds, results are consumed and a callback resumes the HTTP response.
//
// Batched pipelining: connections stay in pipeline mode permanently. Multiple
// requests are multiplexed on each connection with individual PQpipelineSync
// markers. Results are matched to requests by PIPELINE_SYNC boundaries.

const std = @import("std");
const c = @import("../pg.zig").c;

const posix = std.posix;
const IoUring = std.os.linux.IoUring;
const linux = std.os.linux;

// Tag bit set on io_uring user_data for DB poll CQEs
pub const DB_POLL_TAG_BIT: u64 = 0x2000000000000000;

/// Pool configuration
pub const DbPoolConfig = struct {
    conn_string: [:0]const u8,
    num_conns: u8 = 4,
    /// Prepared statements to create on each connection: {name, sql, n_params}
    prepared_statements: []const PreparedStmt = &.{},
};

pub const PreparedStmt = struct {
    name: [*:0]const u8,
    sql: [*:0]const u8,
    n_params: c_int,
};

/// What kind of DB operation is pending
pub const DbOperation = union(enum) {
    /// Single prepared query (e.g. /db)
    single_query: struct {
        stmt_name: [*:0]const u8,
        params: [8]?[*:0]const u8,
        n_params: u16,
    },
    /// Pipelined N queries with individual param buffers (e.g. /queries, /updates read phase)
    pipeline_query: struct {
        stmt_name: [*:0]const u8,
        param_bufs: [500][16]u8,
        count: u16,
    },
    /// Fortune query (no params, returns multiple rows with id + message)
    fortune_query: struct {
        stmt_name: [*:0]const u8,
    },
    /// Raw prepared query with 2 params (e.g. UNNEST update)
    raw_prepared: struct {
        stmt_name: [*:0]const u8,
        params: [2]?[*:0]const u8,
        n_params: u16,
    },
};

/// Fortune row for async results
pub const FortuneRow = struct {
    id: i32,
    message: [256]u8,
    message_len: u16,
};

/// Accumulated DB results
pub const DbResult = struct {
    worlds: [500]World,
    world_count: u16 = 0,
    fortunes: [128]FortuneRow,
    fortune_count: u16 = 0,
    err: bool = false,
};

pub const World = struct {
    id: i32,
    randomnumber: i32,
};

/// Phase of a multi-step DB operation (e.g. updates: read then write)
pub const Phase = enum {
    initial,
    updates_write, // After reads complete, submit the UNNEST update
};

/// Callback invoked when DB results are ready.
/// conn_ptr: the HTTP Connection pointer (as usize)
/// result: accumulated results from the query
/// Returns: the raw HTTP response bytes to write, or null on error
pub const DbCallback = *const fn (conn_ptr: usize, result: *DbResult, phase: Phase, pool: *DbConnPool) ?[]const u8;

/// Maximum in-flight requests per DbConn
const MAX_REQUESTS_PER_CONN: usize = 64;

/// A pending DB request tied to an HTTP connection
pub const DbRequest = struct {
    http_conn_ptr: usize, // @intFromPtr of the HTTP Connection
    operation: DbOperation,
    callback: DbCallback,
    result_data: DbResult,
    phase: Phase,
    // Pipeline consumption state
    pipeline_results_remaining: u16,
    pipeline_results_consumed: u16,
    active: bool,
};

/// Single non-blocking PGconn
const DbConn = struct {
    conn: *c.PGconn,
    fd: posix.socket_t,
    poll_armed: bool,
    // FIFO queue of pending requests
    requests: [MAX_REQUESTS_PER_CONN]DbRequest,
    head: u16, // next slot to dequeue (oldest)
    tail: u16, // next slot to enqueue
    count: u16,
    // Track whether we're in pipeline mode
    in_pipeline: bool,

    fn init(conn_string: [*:0]const u8, prepared: []const PreparedStmt) !DbConn {
        const pg_conn = c.PQconnectdb(conn_string) orelse {
            return error.ConnectionFailed;
        };

        if (c.PQstatus(pg_conn) != c.CONNECTION_OK) {
            const err_msg = c.PQerrorMessage(pg_conn);
            std.log.err("Async PG connection failed: {s}", .{std.mem.span(err_msg)});
            c.PQfinish(pg_conn);
            return error.ConnectionFailed;
        }

        // Set non-blocking mode
        if (c.PQsetnonblocking(pg_conn, 1) != 0) {
            const err_msg = c.PQerrorMessage(pg_conn);
            std.log.err("PQsetnonblocking failed: {s}", .{std.mem.span(err_msg)});
            c.PQfinish(pg_conn);
            return error.ConnectionFailed;
        }

        // Prepare statements (done synchronously at init time)
        for (prepared) |stmt| {
            const result = c.PQprepare(pg_conn, stmt.name, stmt.sql, stmt.n_params, null) orelse {
                c.PQfinish(pg_conn);
                return error.PrepareFailed;
            };
            const status = c.PQresultStatus(result);
            c.PQclear(result);
            if (status != c.PGRES_COMMAND_OK) {
                const err_msg = c.PQerrorMessage(pg_conn);
                std.log.err("Prepare failed: {s}", .{std.mem.span(err_msg)});
                c.PQfinish(pg_conn);
                return error.PrepareFailed;
            }
        }

        const fd = c.PQsocket(pg_conn);
        if (fd < 0) {
            c.PQfinish(pg_conn);
            return error.ConnectionFailed;
        }

        var db = DbConn{
            .conn = pg_conn,
            .fd = fd,
            .poll_armed = false,
            .requests = undefined,
            .head = 0,
            .tail = 0,
            .count = 0,
            .in_pipeline = false,
        };
        // Initialize request slots
        for (&db.requests) |*r| {
            r.active = false;
        }
        return db;
    }

    fn deinit(self: *DbConn) void {
        c.PQfinish(self.conn);
    }

    fn enqueue(self: *DbConn, req: DbRequest) ?*DbRequest {
        if (self.count >= MAX_REQUESTS_PER_CONN) return null;
        self.requests[self.tail] = req;
        self.requests[self.tail].active = true;
        const ptr = &self.requests[self.tail];
        self.tail = @intCast((@as(u32, self.tail) + 1) % MAX_REQUESTS_PER_CONN);
        self.count += 1;
        return ptr;
    }

    fn peekHead(self: *DbConn) ?*DbRequest {
        if (self.count == 0) return null;
        if (!self.requests[self.head].active) return null;
        return &self.requests[self.head];
    }

    fn dequeue(self: *DbConn) void {
        if (self.count == 0) return;
        self.requests[self.head].active = false;
        self.head = @intCast((@as(u32, self.head) + 1) % MAX_REQUESTS_PER_CONN);
        self.count -= 1;
    }

    /// Ensure we're in pipeline mode, entering it if necessary.
    fn ensurePipeline(self: *DbConn) bool {
        if (self.in_pipeline) return true;
        if (c.PQenterPipelineMode(self.conn) != 1) return false;
        self.in_pipeline = true;
        return true;
    }

    /// Send the queries for an operation, followed by PQpipelineSync + flush.
    /// The connection must already be in pipeline mode.
    fn sendOperation(self: *DbConn, op: *const DbOperation) bool {
        switch (op.*) {
            .single_query => |*sq| {
                if (c.PQsendQueryPrepared(
                    self.conn,
                    sq.stmt_name,
                    @intCast(sq.n_params),
                    @ptrCast(&sq.params),
                    null,
                    null,
                    1, // binary format
                ) != 1) return false;
            },
            .pipeline_query => |*pq| {
                for (0..pq.count) |i| {
                    const params = [_]?[*:0]const u8{@ptrCast(&pq.param_bufs[i])};
                    if (c.PQsendQueryPrepared(
                        self.conn,
                        pq.stmt_name,
                        1,
                        @ptrCast(&params),
                        null,
                        null,
                        1, // binary format
                    ) != 1) return false;
                }
            },
            .fortune_query => |*fq| {
                if (c.PQsendQueryPrepared(
                    self.conn,
                    fq.stmt_name,
                    0,
                    null,
                    null,
                    null,
                    0, // text format (VARCHAR message)
                ) != 1) return false;
            },
            .raw_prepared => |*rp| {
                if (c.PQsendQueryPrepared(
                    self.conn,
                    rp.stmt_name,
                    @intCast(rp.n_params),
                    @ptrCast(&rp.params),
                    null,
                    null,
                    0, // text format
                ) != 1) return false;
            },
        }

        if (c.PQpipelineSync(self.conn) != 1) return false;
        _ = c.PQflush(self.conn);
        return true;
    }
};

/// Per-thread pool of non-blocking PGconn instances
pub const DbConnPool = struct {
    conns: []DbConn,
    num_conns: u8,
    allocator: std.mem.Allocator,
    config: DbPoolConfig,

    pub fn init(allocator: std.mem.Allocator, config: DbPoolConfig) !DbConnPool {
        const conns = try allocator.alloc(DbConn, config.num_conns);
        errdefer allocator.free(conns);

        var initialized: u8 = 0;
        errdefer {
            for (conns[0..initialized]) |*conn| conn.deinit();
        }

        for (conns) |*conn| {
            conn.* = try DbConn.init(config.conn_string.ptr, config.prepared_statements);
            initialized += 1;
        }

        std.log.info("Async PG pool initialized with {} connections, {} prepared statements", .{
            config.num_conns,
            config.prepared_statements.len,
        });

        return .{
            .conns = conns,
            .num_conns = config.num_conns,
            .allocator = allocator,
            .config = config,
        };
    }

    pub fn deinit(self: *DbConnPool) void {
        for (self.conns) |*conn| conn.deinit();
        self.allocator.free(self.conns);
    }

    /// Submit a DB request to the least-loaded connection.
    /// The query is sent immediately (batched pipelining).
    /// Arms io_uring poll_add if not already armed.
    pub fn submit(self: *DbConnPool, ring: *IoUring, request: DbRequest) bool {
        // Find least-loaded connection
        var best_idx: u8 = 0;
        var best_count: u16 = self.conns[0].count;
        for (self.conns[1..], 1..) |conn, i| {
            if (conn.count < best_count) {
                best_count = conn.count;
                best_idx = @intCast(i);
            }
        }

        return self.submitToConnInternal(ring, best_idx, request);
    }

    /// Submit a request directly to a specific connection index (for chained operations like updates).
    pub fn submitToConn(self: *DbConnPool, ring: *IoUring, conn_idx: u8, request: DbRequest) bool {
        return self.submitToConnInternal(ring, conn_idx, request);
    }

    fn submitToConnInternal(self: *DbConnPool, ring: *IoUring, conn_idx: u8, request: DbRequest) bool {
        const db_conn = &self.conns[conn_idx];

        // Set pipeline_results_remaining based on operation type
        var req = request;
        switch (req.operation) {
            .single_query => {
                req.pipeline_results_remaining = 1;
            },
            .pipeline_query => |pq| {
                req.pipeline_results_remaining = pq.count;
            },
            .fortune_query => {
                req.pipeline_results_remaining = 1;
            },
            .raw_prepared => {
                req.pipeline_results_remaining = 1;
            },
        }
        req.pipeline_results_consumed = 0;

        // Enqueue
        const enqueued = db_conn.enqueue(req) orelse return false;

        // Ensure pipeline mode and send immediately
        if (!db_conn.ensurePipeline()) {
            db_conn.dequeue();
            return false;
        }

        if (!db_conn.sendOperation(&enqueued.operation)) {
            db_conn.dequeue();
            return false;
        }

        // Arm poll if not already armed
        if (!db_conn.poll_armed) {
            const tag = DB_POLL_TAG_BIT | @as(u64, conn_idx);
            _ = ring.poll_add(tag, db_conn.fd, linux.POLL.IN) catch return false;
            db_conn.poll_armed = true;
        }

        return true;
    }

    /// Handle a poll CQE — consume ready results from the specified DbConn.
    /// Returns a list of (conn_ptr, response_data) pairs to write.
    pub fn handlePoll(
        self: *DbConnPool,
        ring: *IoUring,
        db_conn_idx: u8,
        responses: *ResponseList,
    ) void {
        const db_conn = &self.conns[db_conn_idx];
        db_conn.poll_armed = false;

        // Consume input from socket
        if (c.PQconsumeInput(db_conn.conn) != 1) {
            std.log.err("PQconsumeInput failed: {s}", .{std.mem.span(c.PQerrorMessage(db_conn.conn))});
            // Mark head request as error
            if (db_conn.peekHead()) |req| {
                req.result_data.err = true;
                const resp = req.callback(req.http_conn_ptr, &req.result_data, req.phase, self);
                if (resp) |r| {
                    responses.add(req.http_conn_ptr, r);
                }
                self.finishCurrentRequest(db_conn);
            }
            self.rearmPollIfNeeded(ring, db_conn, db_conn_idx);
            return;
        }

        // Drain all ready results
        while (c.PQisBusy(db_conn.conn) == 0) {
            const req = db_conn.peekHead() orelse break;

            const maybe_result = c.PQgetResult(db_conn.conn);
            if (maybe_result == null) {
                // null result = end of results for this query in pipeline
                continue;
            }
            const result = maybe_result.?;

            const status = c.PQresultStatus(result);

            if (status == c.PGRES_PIPELINE_SYNC) {
                c.PQclear(result);
                // This sync marks the end of one request's queries.
                // Invoke callback and dequeue. Stay in pipeline mode.
                const resp = req.callback(req.http_conn_ptr, &req.result_data, req.phase, self);
                if (resp) |r| {
                    responses.add(req.http_conn_ptr, r);
                }
                self.finishCurrentRequest(db_conn);
                continue;
            }

            if (status == c.PGRES_TUPLES_OK) {
                // Extract world/fortune data from result
                self.extractResult(req, result);
                c.PQclear(result);

                // Consume the null terminator for this query result
                const null_result = c.PQgetResult(db_conn.conn);
                if (null_result != null) {
                    c.PQclear(null_result);
                }

                req.pipeline_results_consumed += 1;
            } else if (status == c.PGRES_COMMAND_OK) {
                c.PQclear(result);

                // Consume the null terminator for this query result
                const null_result = c.PQgetResult(db_conn.conn);
                if (null_result != null) {
                    c.PQclear(null_result);
                }

                req.pipeline_results_consumed += 1;
            } else {
                // Error
                std.log.warn("DB result error: {s}", .{std.mem.span(c.PQresultErrorMessage(result))});
                c.PQclear(result);
                req.result_data.err = true;

                // Consume remaining results to clear pipeline
                const null_result = c.PQgetResult(db_conn.conn);
                if (null_result != null) {
                    c.PQclear(null_result);
                }
            }
        }

        // Re-arm poll if there are still pending requests
        self.rearmPollIfNeeded(ring, db_conn, db_conn_idx);
    }

    fn extractResult(self: *DbConnPool, req: *DbRequest, result: *c.PGresult) void {
        _ = self;
        const nrows: usize = @intCast(c.PQntuples(result));
        const ncols: usize = @intCast(c.PQnfields(result));

        switch (req.operation) {
            .single_query, .pipeline_query => {
                // World results — binary format (INT4 big-endian)
                if (nrows > 0 and ncols >= 2) {
                    const id_ptr = c.PQgetvalue(result, 0, 0);
                    const rn_ptr = c.PQgetvalue(result, 0, 1);
                    const id: i32 = @byteSwap(@as(*const i32, @ptrCast(@alignCast(id_ptr))).*);
                    const rn: i32 = @byteSwap(@as(*const i32, @ptrCast(@alignCast(rn_ptr))).*);
                    const idx = req.result_data.world_count;
                    if (idx < 500) {
                        req.result_data.worlds[idx] = .{ .id = id, .randomnumber = rn };
                        req.result_data.world_count += 1;
                    }
                }
            },
            .fortune_query => {
                // Fortune results — text format, multiple rows with id + message
                extractFortuneResult(req, result);
            },
            .raw_prepared => {
                // UPDATE result (COMMAND_OK) — nothing to extract
            },
        }
    }

    /// Extract fortune rows from a result set
    pub fn extractFortuneResult(req: *DbRequest, result: *c.PGresult) void {
        const nrows: usize = @intCast(c.PQntuples(result));
        for (0..nrows) |i| {
            if (req.result_data.fortune_count >= 128) break;
            const id_str = c.PQgetvalue(result, @intCast(i), 0);
            const msg_str = c.PQgetvalue(result, @intCast(i), 1);
            const id = std.fmt.parseInt(i32, std.mem.span(id_str), 10) catch 0;
            const msg = std.mem.span(msg_str);
            const idx = req.result_data.fortune_count;
            const copy_len = @min(msg.len, 256);
            @memcpy(req.result_data.fortunes[idx].message[0..copy_len], msg[0..copy_len]);
            req.result_data.fortunes[idx].message_len = @intCast(copy_len);
            req.result_data.fortunes[idx].id = id;
            req.result_data.fortune_count += 1;
        }
    }

    fn finishCurrentRequest(self: *DbConnPool, db_conn: *DbConn) void {
        _ = self;
        db_conn.dequeue();
    }

    fn rearmPollIfNeeded(self: *DbConnPool, ring: *IoUring, db_conn: *DbConn, conn_idx: u8) void {
        _ = self;
        if (db_conn.count > 0 and !db_conn.poll_armed) {
            const tag = DB_POLL_TAG_BIT | @as(u64, conn_idx);
            _ = ring.poll_add(tag, db_conn.fd, linux.POLL.IN) catch {};
            db_conn.poll_armed = true;
        }
    }

    /// Get the connection index that a request was assigned to (for chained ops)
    pub fn connIdxForLeastLoaded(self: *DbConnPool) u8 {
        var best_idx: u8 = 0;
        var best_count: u16 = self.conns[0].count;
        for (self.conns[1..], 1..) |conn, i| {
            if (conn.count < best_count) {
                best_count = conn.count;
                best_idx = @intCast(i);
            }
        }
        return best_idx;
    }
};

/// List of responses to write back to HTTP connections
pub const ResponseList = struct {
    items: [256]ResponseItem,
    count: usize,

    pub const ResponseItem = struct {
        conn_ptr: usize,
        data: []const u8,
    };

    pub fn init() ResponseList {
        return .{
            .items = undefined,
            .count = 0,
        };
    }

    pub fn add(self: *ResponseList, conn_ptr: usize, data: []const u8) void {
        if (self.count < 256) {
            self.items[self.count] = .{ .conn_ptr = conn_ptr, .data = data };
            self.count += 1;
        }
    }
};
