// TechEmpower Framework Benchmarks — oxelot-http entry
//
// Implements all 7 TFB test types:
//   1. /json           — JSON serialization
//   2. /plaintext      — Plaintext response
//   3. /db             — Single database query
//   4. /queries?q=N   — Multiple database queries (pipelined)
//   5. /fortunes       — Server-side HTML template with DB data
//   6. /updates?q=N   — Multiple updates (batched CASE/WHEN)
//   7. /cached-queries?q=N — In-memory cached world rows
//
// DB handlers use async libpq with io_uring poll for non-blocking DB I/O.
//
// Run with: zig build techempower
// Test with: wrk -t8 -c256 -d15s http://localhost:8080/json

const std = @import("std");
const http = @import("http");
const pg = @import("pg");

// ============================================================================
// Types
// ============================================================================

const World = struct {
    id: i32,
    randomnumber: i32,
};

const Fortune = struct {
    id: i32,
    message: []const u8,
};

// ============================================================================
// Helpers
// ============================================================================

fn writeInt(buf: []u8, val: usize) usize {
    if (val < 10) {
        buf[0] = '0' + @as(u8, @intCast(val));
        return 1;
    } else if (val < 100) {
        buf[0] = '0' + @as(u8, @intCast(val / 10));
        buf[1] = '0' + @as(u8, @intCast(val % 10));
        return 2;
    } else if (val < 1000) {
        buf[0] = '0' + @as(u8, @intCast(val / 100));
        buf[1] = '0' + @as(u8, @intCast(val / 10 % 10));
        buf[2] = '0' + @as(u8, @intCast(val % 10));
        return 3;
    } else if (val < 10000) {
        buf[0] = '0' + @as(u8, @intCast(val / 1000));
        buf[1] = '0' + @as(u8, @intCast(val / 100 % 10));
        buf[2] = '0' + @as(u8, @intCast(val / 10 % 10));
        buf[3] = '0' + @as(u8, @intCast(val % 10));
        return 4;
    } else {
        buf[0] = '0' + @as(u8, @intCast(val / 10000));
        buf[1] = '0' + @as(u8, @intCast(val / 1000 % 10));
        buf[2] = '0' + @as(u8, @intCast(val / 100 % 10));
        buf[3] = '0' + @as(u8, @intCast(val / 10 % 10));
        buf[4] = '0' + @as(u8, @intCast(val % 10));
        return 5;
    }
}

// ============================================================================
// Thread-local state
// ============================================================================

threadlocal var tl_rng: std.Random.Xoshiro256 = undefined;
threadlocal var tl_rng_init: bool = false;

fn getRng() *std.Random.Xoshiro256 {
    if (!tl_rng_init) {
        var seed: u64 = 0;
        if (std.posix.clock_gettime(.REALTIME)) |ts| {
            seed = @bitCast(@as(i64, ts.sec) *% 1_000_000_000 +% @as(i64, ts.nsec));
        } else |_| {}
        tl_rng = std.Random.Xoshiro256.init(seed);
        tl_rng_init = true;
    }
    return &tl_rng;
}

fn randomWorldId() i32 {
    return @as(i32, @intCast(getRng().random().intRangeAtMost(u32, 1, 10000)));
}

/// Get a null-terminated env var value. Returns null if not set.
fn envZ(name: [*:0]const u8) ?[:0]const u8 {
    const val = std.c.getenv(name) orelse return null;
    return std.mem.span(val);
}

fn getDbConnString() [:0]const u8 {
    // Build connection string — stored in global so it persists
    const S = struct {
        var buf: [512]u8 = undefined;
        var str: ?[:0]const u8 = null;
    };
    if (S.str) |s| return s;

    const host = envZ("DBHOST") orelse "tfb-database";
    const port_str = std.posix.getenv("DBPORT") orelse "5432";
    const database = envZ("DBNAME") orelse "hello_world";
    const username = envZ("DBUSER") orelse "benchmarkdbuser";
    const password = envZ("DBPASS") orelse "benchmarkdbpass";

    const result = std.fmt.bufPrintZ(&S.buf, "host={s} port={s} dbname={s} user={s} password={s}", .{
        host, port_str, database, username, password,
    }) catch "host=tfb-database port=5432 dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass";
    S.str = result;
    return result;
}

// Prepared statements shared across all connections
const prepared_stmts = [_]pg.PreparedStmt{
    .{ .name = "world", .sql = "SELECT id, randomnumber FROM world WHERE id=$1", .n_params = 1 },
    .{ .name = "fortune", .sql = "SELECT id, message FROM fortune", .n_params = 0 },
    .{
        .name = "update_worlds",
        .sql = "UPDATE world SET randomnumber = new.rnum FROM " ++
            "(SELECT * FROM UNNEST($1::int[], $2::int[]) AS v(id, rnum) ORDER BY 1) AS new " ++
            "WHERE world.id = new.id",
        .n_params = 2,
    },
};

// ============================================================================
// Async poll callbacks (called from server's io_uring event loop)
// ============================================================================

fn asyncPollInit(ring: *http.IoUring) ?*anyopaque {
    const num_conns: u8 = if (std.posix.getenv("DB_CONNS")) |env|
        std.fmt.parseInt(u8, env, 10) catch 4
    else
        4;
    const pool = pg.DbConnPool.init(std.heap.c_allocator, .{
        .conn_string = getDbConnString(),
        .num_conns = num_conns,
        .prepared_statements = &prepared_stmts,
    }) catch |err| {
        std.log.err("Failed to init async DB pool: {}", .{err});
        return null;
    };
    _ = ring; // ring is used later via getAsyncPollRing()

    // Store on heap so we can return a stable pointer
    const heap_pool = std.heap.c_allocator.create(pg.DbConnPool) catch return null;
    heap_pool.* = pool;
    return @ptrCast(heap_pool);
}

fn asyncPollDeinit(state: *anyopaque) void {
    const pool: *pg.DbConnPool = @ptrCast(@alignCast(state));
    pool.deinit();
    std.heap.c_allocator.destroy(pool);
}

fn asyncPollHandler(state: *anyopaque, ring: *http.IoUring, db_conn_idx: u8, writer: *http.AsyncPollResponseWriter) void {
    const pool: *pg.DbConnPool = @ptrCast(@alignCast(state));
    // Use ResponseList that matches the pg.ResponseList interface
    var responses = pg.ResponseList.init();
    pool.handlePoll(ring, db_conn_idx, &responses);

    // Transfer to the server's writer
    for (responses.items[0..responses.count]) |item| {
        writer.add(item.conn_ptr, item.data);
    }
}

// ============================================================================
// Cached queries — global cache refreshed by background thread
// ============================================================================

const CachedWorld = struct {
    id: i32,
    randomnumber: std.atomic.Value(i32),
};

var cached_worlds: [10001]CachedWorld = undefined;
var cache_ready = std.atomic.Value(bool).init(false);

fn startCacheRefreshThread() !void {
    _ = try std.Thread.spawn(.{}, cacheRefreshLoop, .{});
}

fn cacheRefreshLoop() void {
    while (true) {
        refreshCache() catch |err| {
            std.log.err("Cache refresh failed: {}", .{err});
        };
        std.posix.nanosleep(5, 0);
    }
}

fn refreshCache() !void {
    const db_config: pg.ConnConfig = .{
        .host = envZ("DBHOST") orelse "tfb-database",
        .port = if (std.fmt.parseInt(u16, std.posix.getenv("DBPORT") orelse "5432", 10)) |p| p else |_| 5432,
        .database = envZ("DBNAME") orelse "hello_world",
        .username = envZ("DBUSER") orelse "benchmarkdbuser",
        .password = envZ("DBPASS") orelse "benchmarkdbpass",
    };
    var conn = try pg.Connection.connect(db_config);
    defer conn.deinit();

    var result = try conn.query("SELECT id, randomnumber FROM world");
    defer result.deinit();

    const rows = result.rowCount();
    for (0..rows) |i| {
        const id = result.getInt(i, 0) orelse continue;
        const rn = result.getInt(i, 1) orelse continue;
        if (id >= 1 and id <= 10000) {
            const idx: usize = @intCast(id);
            cached_worlds[idx].id = id;
            cached_worlds[idx].randomnumber.store(rn, .release);
        }
    }

    cache_ready.store(true, .release);
}

// ============================================================================
// Raw response helpers — bypass Response serialization for near-zero allocs
// ============================================================================

threadlocal var tl_response_buf: [65536]u8 = undefined;

fn writeRawResponse(
    content_type: []const u8,
    body: []const u8,
    date: []const u8,
) []const u8 {
    var buf = &tl_response_buf;
    var pos: usize = 0;

    appendSlice(buf, &pos, "HTTP/1.1 200 OK\r\nServer: oxelot-http\r\nContent-Type: ");
    appendSlice(buf, &pos, content_type);
    appendSlice(buf, &pos, "\r\nContent-Length: ");
    pos += writeIntBuf(buf[pos..], body.len);
    appendSlice(buf, &pos, "\r\nDate: ");
    appendSlice(buf, &pos, date);
    appendSlice(buf, &pos, "\r\n\r\n");
    appendSlice(buf, &pos, body);

    return buf[0..pos];
}

/// Build a raw HTTP response and allocate a heap copy (for async callbacks)
fn writeRawResponseAlloc(
    content_type: []const u8,
    body: []const u8,
    date: []const u8,
) ?[]u8 {
    const raw = writeRawResponse(content_type, body, date);
    const buf = std.heap.c_allocator.alloc(u8, raw.len) catch return null;
    @memcpy(buf, raw);
    return buf;
}

fn appendSlice(buf: []u8, pos: *usize, data: []const u8) void {
    @memcpy(buf[pos.*..][0..data.len], data);
    pos.* += data.len;
}

fn writeIntBuf(buf: []u8, val: usize) usize {
    if (val == 0) {
        buf[0] = '0';
        return 1;
    }
    var tmp: [20]u8 = undefined;
    var n: usize = 0;
    var v = val;
    while (v > 0) {
        tmp[n] = '0' + @as(u8, @intCast(v % 10));
        v /= 10;
        n += 1;
    }
    for (0..n) |i| {
        buf[i] = tmp[n - 1 - i];
    }
    return n;
}

fn getDate() []const u8 {
    return http.getHttpDate();
}

/// Get cached HTTP date from server's threadlocal (used in async callbacks
/// where we don't have a Context).
fn getCachedDate() []const u8 {
    // The server caches the date in its own threadlocal; we can use it here
    // since callbacks run on the same io_uring thread.
    // For simplicity, just format a fresh one.
    const S = struct {
        threadlocal var date_buf: [29]u8 = undefined;
        threadlocal var cached_ts: i64 = 0;
    };
    const ts = std.posix.clock_gettime(.REALTIME) catch return "Thu, 01 Jan 1970 00:00:00 GMT";
    if (ts.sec != S.cached_ts) {
        formatHttpDate(&S.date_buf, ts.sec);
        S.cached_ts = ts.sec;
    }
    return &S.date_buf;
}

const day_names = [_][]const u8{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
const month_names = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

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

// ============================================================================
// Handlers — sync (non-DB)
// ============================================================================

fn handleJson(ctx: *http.Context) !void {
    const body =
        \\{"message":"Hello, World!"}
    ;
    ctx.response.setRaw(writeRawResponse("application/json", body, getDate()));
}

fn handlePlaintext(ctx: *http.Context) !void {
    ctx.response.setRaw(writeRawResponse("text/plain; charset=utf-8", "Hello, World!", getDate()));
}

fn handleCachedQueries(ctx: *http.Context) !void {
    if (!cache_ready.load(.acquire)) {
        _ = ctx.response.internalServerError();
        return;
    }

    const count = parseQueryCount(ctx, "q");
    var worlds: [500]World = undefined;

    for (0..count) |i| {
        const id = randomWorldId();
        const idx: usize = @intCast(id);
        worlds[i] = .{
            .id = cached_worlds[idx].id,
            .randomnumber = cached_worlds[idx].randomnumber.load(.acquire),
        };
    }

    sendWorldsRaw(ctx, worlds[0..count]);
}

// ============================================================================
// Handlers — async DB
// ============================================================================

fn handleDb(ctx: *http.Context) !void {
    const pool = getPool() orelse {
        _ = ctx.response.internalServerError();
        return;
    };
    const ring = http.getAsyncPollRing() orelse {
        _ = ctx.response.internalServerError();
        return;
    };

    const id = randomWorldId();

    // Format the id parameter (must be alive until submit returns)
    var id_buf: [16]u8 = undefined;
    const id_str = std.fmt.bufPrintZ(&id_buf, "{d}", .{id}) catch unreachable;

    var params: [8]?[*:0]const u8 = .{null} ** 8;
    params[0] = id_str.ptr;

    const req = pg.DbRequest{
        .http_conn_ptr = http.getCurrentConnPtr(),
        .operation = .{ .single_query = .{
            .stmt_name = "world",
            .params = params,
            .n_params = 1,
        } },
        .callback = dbCallback,
        .result_data = .{
            .worlds = undefined,
            .world_count = 0,
            .fortunes = undefined,
            .fortune_count = 0,
            .err = false,
        },
        .phase = .initial,
        .pipeline_results_remaining = 0,
        .pipeline_results_consumed = 0,
        .active = false,
    };

    if (!pool.submit(ring, req)) {
        _ = ctx.response.internalServerError();
        return;
    }

    ctx.response.async_pending = true;
}

fn handleQueries(ctx: *http.Context) !void {
    const pool = getPool() orelse {
        _ = ctx.response.internalServerError();
        return;
    };
    const ring = http.getAsyncPollRing() orelse {
        _ = ctx.response.internalServerError();
        return;
    };

    const count = parseQueryCount(ctx, "q");

    // Fill param buffers with random ids
    var param_bufs: [500][16]u8 = undefined;
    for (0..count) |i| {
        _ = std.fmt.bufPrintZ(&param_bufs[i], "{d}", .{randomWorldId()}) catch unreachable;
    }

    const req = pg.DbRequest{
        .http_conn_ptr = http.getCurrentConnPtr(),
        .operation = .{ .pipeline_query = .{
            .stmt_name = "world",
            .param_bufs = param_bufs,
            .count = @intCast(count),
        } },
        .callback = queriesCallback,
        .result_data = .{
            .worlds = undefined,
            .world_count = 0,
            .fortunes = undefined,
            .fortune_count = 0,
            .err = false,
        },
        .phase = .initial,
        .pipeline_results_remaining = 0,
        .pipeline_results_consumed = 0,
        .active = false,
    };

    if (!pool.submit(ring, req)) {
        _ = ctx.response.internalServerError();
        return;
    }

    ctx.response.async_pending = true;
}

fn handleFortunes(ctx: *http.Context) !void {
    const pool = getPool() orelse {
        _ = ctx.response.internalServerError();
        return;
    };
    const ring = http.getAsyncPollRing() orelse {
        _ = ctx.response.internalServerError();
        return;
    };

    const req = pg.DbRequest{
        .http_conn_ptr = http.getCurrentConnPtr(),
        .operation = .{ .fortune_query = .{
            .stmt_name = "fortune",
        } },
        .callback = fortuneCallback,
        .result_data = .{
            .worlds = undefined,
            .world_count = 0,
            .fortunes = undefined,
            .fortune_count = 0,
            .err = false,
        },
        .phase = .initial,
        .pipeline_results_remaining = 0,
        .pipeline_results_consumed = 0,
        .active = false,
    };

    if (!pool.submit(ring, req)) {
        _ = ctx.response.internalServerError();
        return;
    }

    ctx.response.async_pending = true;
}

fn handleUpdates(ctx: *http.Context) !void {
    const pool = getPool() orelse {
        _ = ctx.response.internalServerError();
        return;
    };
    const ring = http.getAsyncPollRing() orelse {
        _ = ctx.response.internalServerError();
        return;
    };

    const count = parseQueryCount(ctx, "q");

    // Fill param buffers with random ids
    var param_bufs: [500][16]u8 = undefined;
    for (0..count) |i| {
        _ = std.fmt.bufPrintZ(&param_bufs[i], "{d}", .{randomWorldId()}) catch unreachable;
    }

    const req = pg.DbRequest{
        .http_conn_ptr = http.getCurrentConnPtr(),
        .operation = .{ .pipeline_query = .{
            .stmt_name = "world",
            .param_bufs = param_bufs,
            .count = @intCast(count),
        } },
        .callback = updatesReadCallback,
        .result_data = .{
            .worlds = undefined,
            .world_count = 0,
            .fortunes = undefined,
            .fortune_count = 0,
            .err = false,
        },
        .phase = .initial,
        .pipeline_results_remaining = 0,
        .pipeline_results_consumed = 0,
        .active = false,
    };

    if (!pool.submit(ring, req)) {
        _ = ctx.response.internalServerError();
        return;
    }

    ctx.response.async_pending = true;
}

// ============================================================================
// DB Callbacks (invoked from io_uring poll handler when results are ready)
// ============================================================================

fn dbCallback(conn_ptr: usize, result: *pg.DbResult, phase: pg.async_pool.Phase, pool: *pg.DbConnPool) ?[]const u8 {
    _ = conn_ptr;
    _ = phase;
    _ = pool;

    if (result.err or result.world_count == 0) {
        return writeRawResponseAlloc("application/json", "{\"error\":\"db\"}", getCachedDate());
    }

    const w = result.worlds[0];
    var body_buf: [64]u8 = undefined;
    var writer = http.JsonWriter.init(&body_buf);
    writer.writeRaw("{\"id\":") catch unreachable;
    writer.writeInt(w.id) catch unreachable;
    writer.writeRaw(",\"randomnumber\":") catch unreachable;
    writer.writeInt(w.randomnumber) catch unreachable;
    writer.writeByte('}') catch unreachable;

    return writeRawResponseAlloc("application/json", writer.getWritten(), getCachedDate());
}

fn queriesCallback(conn_ptr: usize, result: *pg.DbResult, phase: pg.async_pool.Phase, pool: *pg.DbConnPool) ?[]const u8 {
    _ = conn_ptr;
    _ = phase;
    _ = pool;

    if (result.err) {
        return writeRawResponseAlloc("application/json", "[]", getCachedDate());
    }

    const worlds = worldsFromResult(result);
    const body = writeWorldsJsonRaw(worlds);
    return writeRawResponseAlloc("application/json", body, getCachedDate());
}

fn fortuneCallback(conn_ptr: usize, result: *pg.DbResult, phase: pg.async_pool.Phase, pool: *pg.DbConnPool) ?[]const u8 {
    _ = conn_ptr;
    _ = phase;
    _ = pool;

    if (result.err) {
        return writeRawResponseAlloc("text/html; charset=utf-8", "<html><body>error</body></html>", getCachedDate());
    }

    // Build Fortune slice from result data
    var fortunes: [128]Fortune = undefined;
    var fortune_count: usize = 0;

    for (0..result.fortune_count) |i| {
        const fr = &result.fortunes[i];
        fortunes[fortune_count] = .{
            .id = fr.id,
            .message = fr.message[0..fr.message_len],
        };
        fortune_count += 1;
    }

    // Add the additional fortune
    fortunes[fortune_count] = .{
        .id = 0,
        .message = "Additional fortune added at request time.",
    };
    fortune_count += 1;

    // Sort by message
    std.mem.sort(Fortune, fortunes[0..fortune_count], {}, struct {
        fn lessThan(_: void, a: Fortune, b: Fortune) bool {
            return std.mem.order(u8, a.message, b.message) == .lt;
        }
    }.lessThan);

    // Render HTML
    var buf: [32768]u8 = undefined;
    var pos: usize = 0;

    const header_html = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
    @memcpy(buf[pos..][0..header_html.len], header_html);
    pos += header_html.len;

    for (fortunes[0..fortune_count]) |f| {
        const row_start = "<tr><td>";
        @memcpy(buf[pos..][0..row_start.len], row_start);
        pos += row_start.len;

        const id_slice = std.fmt.bufPrint(buf[pos..], "{d}", .{f.id}) catch "";
        pos += id_slice.len;

        const mid_html = "</td><td>";
        @memcpy(buf[pos..][0..mid_html.len], mid_html);
        pos += mid_html.len;

        pos += htmlEscape(buf[pos..], f.message);

        const row_end = "</td></tr>";
        @memcpy(buf[pos..][0..row_end.len], row_end);
        pos += row_end.len;
    }

    const footer_html = "</table></body></html>";
    @memcpy(buf[pos..][0..footer_html.len], footer_html);
    pos += footer_html.len;

    return writeRawResponseAlloc("text/html; charset=utf-8", buf[0..pos], getCachedDate());
}

fn updatesReadCallback(conn_ptr: usize, result: *pg.DbResult, phase: pg.async_pool.Phase, pool: *pg.DbConnPool) ?[]const u8 {
    _ = phase;

    if (result.err) {
        return writeRawResponseAlloc("application/json", "[]", getCachedDate());
    }

    // Assign new random numbers
    for (0..result.world_count) |i| {
        result.worlds[i].randomnumber = randomWorldId();
    }

    // Build UNNEST update params
    var ids_buf: [4096]u8 = undefined;
    var rns_buf: [4096]u8 = undefined;
    var ids_pos: usize = 0;
    var rns_pos: usize = 0;

    ids_buf[0] = '{';
    ids_pos = 1;
    rns_buf[0] = '{';
    rns_pos = 1;

    for (0..result.world_count) |i| {
        if (i > 0) {
            ids_buf[ids_pos] = ',';
            ids_pos += 1;
            rns_buf[rns_pos] = ',';
            rns_pos += 1;
        }
        ids_pos += writeInt(ids_buf[ids_pos..], @intCast(result.worlds[i].id));
        rns_pos += writeInt(rns_buf[rns_pos..], @intCast(result.worlds[i].randomnumber));
    }

    ids_buf[ids_pos] = '}';
    ids_pos += 1;
    ids_buf[ids_pos] = 0;

    rns_buf[rns_pos] = '}';
    rns_pos += 1;
    rns_buf[rns_pos] = 0;

    // Submit the UPDATE as a follow-up request on the same connection
    const ring = http.getAsyncPollRing() orelse {
        return writeRawResponseAlloc("application/json", "[]", getCachedDate());
    };

    const update_req = pg.DbRequest{
        .http_conn_ptr = conn_ptr,
        .operation = .{ .raw_prepared = .{
            .stmt_name = "update_worlds",
            .params = .{ @ptrCast(&ids_buf), @ptrCast(&rns_buf) },
            .n_params = 2,
        } },
        .callback = updatesWriteCallback,
        .result_data = result.*, // Copy the worlds data for the response
        .phase = .updates_write,
        .pipeline_results_remaining = 0,
        .pipeline_results_consumed = 0,
        .active = false,
    };

    // Submit to the pool
    if (!pool.submit(ring, update_req)) {
        // Fallback: return the worlds without updating
        const worlds = worldsFromResult(result);
        const body = writeWorldsJsonRaw(worlds);
        return writeRawResponseAlloc("application/json", body, getCachedDate());
    }

    // Don't return a response yet — wait for the UPDATE to complete
    return null;
}

fn updatesWriteCallback(conn_ptr: usize, result: *pg.DbResult, phase: pg.async_pool.Phase, pool: *pg.DbConnPool) ?[]const u8 {
    _ = conn_ptr;
    _ = phase;
    _ = pool;

    // UPDATE complete — send the worlds as JSON response
    const worlds = worldsFromResult(result);
    const body = writeWorldsJsonRaw(worlds);
    return writeRawResponseAlloc("application/json", body, getCachedDate());
}

// ============================================================================
// Helpers
// ============================================================================

fn getPool() ?*pg.DbConnPool {
    const state = http.getAsyncPollState() orelse return null;
    return @ptrCast(@alignCast(state));
}

fn parseQueryCount(ctx: *http.Context, param_name: []const u8) usize {
    const raw = ctx.query(param_name) orelse return 1;
    const n = std.fmt.parseInt(usize, raw, 10) catch return 1;
    if (n < 1) return 1;
    if (n > 500) return 500;
    return n;
}

fn worldsFromResult(result: *pg.DbResult) []const World {
    // Cast pg.async_pool.World to our World (same layout)
    const pg_worlds: [*]const pg.async_pool.World = &result.worlds;
    const our_worlds: [*]const World = @ptrCast(pg_worlds);
    return our_worlds[0..result.world_count];
}

threadlocal var tl_body_buf: [32768]u8 = undefined;

fn writeWorldsJsonRaw(worlds: []const World) []const u8 {
    var writer = http.JsonWriter.init(&tl_body_buf);

    writer.writeByte('[') catch unreachable;
    for (worlds, 0..) |w, i| {
        if (i > 0) writer.writeByte(',') catch unreachable;
        writer.writeRaw("{\"id\":") catch unreachable;
        writer.writeInt(w.id) catch unreachable;
        writer.writeRaw(",\"randomnumber\":") catch unreachable;
        writer.writeInt(w.randomnumber) catch unreachable;
        writer.writeByte('}') catch unreachable;
    }
    writer.writeByte(']') catch unreachable;

    return writer.getWritten();
}

fn sendWorldsRaw(ctx: *http.Context, worlds: []const World) void {
    const body = writeWorldsJsonRaw(worlds);
    ctx.response.setRaw(writeRawResponse("application/json", body, getDate()));
}

fn htmlEscape(out: []u8, input: []const u8) usize {
    var pos: usize = 0;
    for (input) |ch| {
        switch (ch) {
            '<' => {
                @memcpy(out[pos..][0..4], "&lt;");
                pos += 4;
            },
            '>' => {
                @memcpy(out[pos..][0..4], "&gt;");
                pos += 4;
            },
            '&' => {
                @memcpy(out[pos..][0..5], "&amp;");
                pos += 5;
            },
            '"' => {
                @memcpy(out[pos..][0..6], "&quot;");
                pos += 6;
            },
            '\'' => {
                @memcpy(out[pos..][0..5], "&#39;");
                pos += 5;
            },
            else => {
                out[pos] = ch;
                pos += 1;
            },
        }
    }
    return pos;
}

// ============================================================================
// Main
// ============================================================================

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    // Start cache refresh background thread
    startCacheRefreshThread() catch |err| {
        std.log.warn("Failed to start cache thread: {} (cached-queries will be unavailable)", .{err});
    };

    // Create router
    var router = http.router(allocator);
    defer router.deinit();

    _ = router
        .get("/json", handleJson)
        .get("/plaintext", handlePlaintext)
        .get("/db", handleDb)
        .get("/queries", handleQueries)
        .get("/fortunes", handleFortunes)
        .get("/updates", handleUpdates)
        .get("/cached-queries", handleCachedQueries);

    var server = http.Server.init(allocator, &router, .{
        .threads = null, // Use CPU count
        .ring_size = 4096,
        .cqe_batch_size = 512,
        .cpu_affinity = true,
        .tcp_nodelay = true,
        .max_connections = 8192,
        .read_buffer_size = 4096,
        .async_poll_init = asyncPollInit,
        .async_poll_deinit = asyncPollDeinit,
        .async_poll_handler = asyncPollHandler,
    });
    defer server.deinit();

    const port: u16 = if (std.fmt.parseInt(u16, std.posix.getenv("PORT") orelse "8080", 10)) |p| p else |_| 8080;

    std.log.info("TechEmpower benchmark server starting on port {d} (async DB mode)", .{port});
    std.log.info("Endpoints: /json /plaintext /db /queries /fortunes /updates /cached-queries", .{});
    try server.run("0.0.0.0", port);
}
