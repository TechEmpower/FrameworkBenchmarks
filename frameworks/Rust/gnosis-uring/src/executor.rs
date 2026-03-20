//! Topology executor — walks the DAG and dispatches operations.
//!
//! The executor is the heart of gnosis-uring. It replaces:
//!   - Event loops (epoll/kqueue reactor)
//!   - Callback chains
//!   - State machines
//!
//! With a single function: walk edges, submit ops, collect results.
//!
//! On Linux, operations are io_uring submissions.
//! On macOS, operations are kqueue + direct syscalls (for development).
//!
//! The topology is a compile-time constant (from Betty).
//! Branch prediction loves static DAGs — no vtable dispatch,
//! no dynamic routing, no hash lookups.

use std::os::raw::c_int;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::http;
use crate::laminar;
use crate::cache::FileCache;
use crate::db;

/// Cached HTTP Date header — updated every second.
static DATE_EPOCH: AtomicU64 = AtomicU64::new(0);
static mut DATE_BUF: [u8; 64] = [0u8; 64];
static mut DATE_LEN: usize = 0;

/// Get current HTTP Date string, cached per-second.
pub fn http_date() -> &'static str {
    use std::time::{SystemTime, UNIX_EPOCH};
    let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    let cached = DATE_EPOCH.load(Ordering::Relaxed);
    if now != cached {
        DATE_EPOCH.store(now, Ordering::Relaxed);
        // Format: "Mon, 17 Mar 2026 14:30:00 GMT"
        let secs = now;
        let days = secs / 86400;
        let time_of_day = secs % 86400;
        let hours = time_of_day / 3600;
        let minutes = (time_of_day % 3600) / 60;
        let seconds = time_of_day % 60;

        // Day of week (Jan 1 1970 was Thursday = 4)
        let dow = ((days + 4) % 7) as usize;
        let dow_names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

        // Date from days since epoch
        let (year, month, day) = days_to_ymd(days as i64);
        let month_names = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];

        let s = format!("{}, {:02} {} {:04} {:02}:{:02}:{:02} GMT",
            dow_names[dow], day, month_names[(month - 1) as usize], year, hours, minutes, seconds);

        unsafe {
            let len = s.len().min(64);
            DATE_BUF[..len].copy_from_slice(&s.as_bytes()[..len]);
            DATE_LEN = len;
        }
    }
    unsafe { std::str::from_utf8_unchecked(&DATE_BUF[..DATE_LEN]) }
}

/// Convert days since Unix epoch to (year, month, day).
fn days_to_ymd(days: i64) -> (i64, i64, i64) {
    // Algorithm from Howard Hinnant
    let z = days + 719468;
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u64;
    let yoe = (doe - doe/1461 + doe/36524 - doe/146096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365*yoe + yoe/4 - yoe/100);
    let mp = (5*doy + 2) / 153;
    let d = doy - (153*mp + 2)/5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m as i64, d as i64)
}

/// Per-connection execution context.
///
/// Allocated once per accept(), reused across keep-alive requests.
/// Zero allocation in the request hot path.
pub struct ConnContext {
    pub socket_fd: c_int,
    pub read_buf: Vec<u8>,
    pub read_len: usize,
}

impl ConnContext {
    pub fn new(socket_fd: c_int) -> Self {
        Self {
            socket_fd,
            read_buf: vec![0u8; 65536],
            read_len: 0,
        }
    }
}

/// The executor drives connections through the topology.
pub struct Executor {
    pub root: String,
    pub cache: FileCache,
    pub port: u16,
    db: Option<db::DbConn>,
    db_cache: Option<db::WorldCache>,
    json_buf: Vec<u8>, // reusable JSON serialization buffer
}

impl Executor {
    pub fn new(root: String, port: u16) -> Self {
        Self {
            root,
            cache: FileCache::new(64 * 1024 * 1024), // 64MB cache
            port,
            db: None,
            db_cache: None,
            json_buf: Vec::with_capacity(256),
        }
    }

    /// Lazily initialize the DB connection.
    fn ensure_db(&mut self) -> &mut db::DbConn {
        if self.db.is_none() {
            self.db = Some(db::DbConn::new());
        }
        self.db.as_mut().unwrap()
    }

    /// Lazily initialize the world cache.
    fn ensure_cache(&mut self) -> &db::WorldCache {
        if self.db_cache.is_none() {
            let conn = self.ensure_db();
            self.db_cache = Some(db::WorldCache::new(conn));
        }
        self.db_cache.as_ref().unwrap()
    }

    /// Handle all pipelined HTTP requests from a single read.
    ///
    /// LAMINAR HTTP pipelining:
    ///   FORK: parse N requests from buffer
    ///   PROCESS: build response for each
    ///   FOLD: concatenate responses, send as one write
    ///
    /// Returns true to keep the connection alive, false to close.
    pub fn handle_request(&mut self, ctx: &mut ConnContext) -> bool {
        let buf = &ctx.read_buf[..ctx.read_len];

        // FORK: parse all pipelined requests
        let (requests, _consumed) = http::parse_pipelined(buf);

        if requests.is_empty() {
            Self::send_response(ctx.socket_fd, http::NOT_FOUND_RESPONSE);
            return false;
        }

        let keep_alive = requests.last().map(|(_, ka)| *ka).unwrap_or(false);

        if requests.len() == 1 {
            // Single request — fast path
            let path = std::str::from_utf8(&requests[0].0).unwrap_or("/");
            return self.handle_single_request(ctx, path, keep_alive);
        }

        // Multiple pipelined requests — TOPOLOGY ROTATION
        //
        // Instead of N sequential PG round-trips, we:
        //   1. FORK: classify all requests, collect /db IDs
        //   2. CANNON: pipeline all /db queries to PG in one write
        //   3. FOLD: build HTTP responses in original order, inserting PG results
        //
        // This rotates the blockage point: instead of blocking N times on PG,
        // we block ONCE for all N queries.

        let date = http_date();
        let paths: Vec<&str> = requests.iter()
            .map(|(p, _)| std::str::from_utf8(p).unwrap_or("/"))
            .collect();

        // Collect all /db request indices and generate their IDs
        let mut db_indices = Vec::new();
        let mut db_ids = Vec::new();
        for (i, &path) in paths.iter().enumerate() {
            if path == "/db" {
                db_indices.push(i);
            }
        }

        // CANNON: pipeline all /db queries in one PG round-trip
        let db_results = if !db_indices.is_empty() {
            let conn = self.ensure_db();
            for _ in 0..db_indices.len() {
                db_ids.push(conn.rand_id());
            }
            conn.pg.query_worlds_pipelined(&db_ids)
        } else {
            Vec::new()
        };

        // FOLD: build all HTTP responses in order
        let mut batch = Vec::with_capacity(requests.len() * 192);
        let mut db_result_idx = 0;

        for (i, &path) in paths.iter().enumerate() {
            match path {
                "/plaintext" => batch.extend_from_slice(&http::build_plaintext_response(date)),
                "/json" => batch.extend_from_slice(&http::build_json_response(date)),
                "/db" => {
                    let (id, rn) = db_results[db_result_idx];
                    db_result_idx += 1;
                    let w = db::WorldRow { id, random_number: rn };
                    let mut body = Vec::with_capacity(40);
                    db::world_to_json(&w, &mut body);
                    batch.extend_from_slice(&http::build_db_response(date, &body));
                }
                _ => {
                    // Fallback for other routes (queries, fortunes, etc.)
                    let resp = self.build_single_response(path);
                    batch.extend_from_slice(&resp);
                }
            }
        }

        Self::send_response(ctx.socket_fd, &batch);
        keep_alive
    }

    /// Handle a single request (fast path, no batch alloc).
    fn handle_single_request(&mut self, ctx: &mut ConnContext, path: &str, keep_alive: bool) -> bool {
        let date = http_date();

        // ── TechEmpower routes ───────────────────────────────────
        match path {
            "/plaintext" => {
                let resp = http::build_plaintext_response(date);
                Self::send_response(ctx.socket_fd, &resp);
                return keep_alive;
            }
            "/json" => {
                let resp = http::build_json_response(date);
                Self::send_response(ctx.socket_fd, &resp);
                return keep_alive;
            }
            "/db" => {
                let conn = self.ensure_db();
                let id = conn.rand_id();
                let world = conn.get_world(id);
                self.json_buf.clear();
                db::world_to_json(&world, &mut self.json_buf);
                let resp = http::build_db_response(date, &self.json_buf);
                Self::send_response(ctx.socket_fd, &resp);
                return keep_alive;
            }
            "/fortunes" => {
                let conn = self.ensure_db();
                let fortunes = conn.get_fortunes();
                let body = db::fortunes_to_html(&fortunes);
                let resp = http::build_html_response(date, &body);
                Self::send_response(ctx.socket_fd, &resp);
                return keep_alive;
            }
            _ => {}
        }

        // Routes with query params
        if path.starts_with("/queries") {
            let count = db::parse_count_param(path, "queries");
            let conn = self.ensure_db();
            let worlds = conn.get_worlds(count);
            let body = db::worlds_to_json(&worlds);
            let resp = http::build_db_response(date, &body);
            Self::send_response(ctx.socket_fd, &resp);
            return keep_alive;
        }

        if path.starts_with("/updates") {
            let count = db::parse_count_param(path, "queries");
            let conn = self.ensure_db();
            let worlds = conn.update_worlds(count);
            let body = db::worlds_to_json(&worlds);
            let resp = http::build_db_response(date, &body);
            Self::send_response(ctx.socket_fd, &resp);
            return keep_alive;
        }

        if path.starts_with("/cached-queries") {
            let count = db::parse_count_param(path, "count");
            self.ensure_cache();
            let cache = self.db_cache.as_ref().unwrap();
            let db = self.db.as_mut().unwrap();
            let mut worlds_json = Vec::with_capacity(count * 40);
            worlds_json.push(b'[');
            for i in 0..count {
                if i > 0 { worlds_json.push(b','); }
                let id = db.rand_id();
                let w = cache.get(id);
                db::world_to_json(w, &mut worlds_json);
            }
            worlds_json.push(b']');
            let resp = http::build_db_response(date, &worlds_json);
            Self::send_response(ctx.socket_fd, &resp);
            return keep_alive;
        }

        // ── FORK: race(cache, mmap, disk) ────────────────────────
        let file_path = if path == "/" { "/index.html" } else { path };
        let full_path = format!("{}{}", self.root, file_path);

        // Arm 1: Cache lookup (microseconds)
        if let Some(entry) = self.cache.get(file_path) {
            return self.serve_cached(ctx, &entry, file_path, keep_alive);
        }

        // Arm 2/3: Disk read
        match std::fs::read(&full_path) {
            Ok(data) => {
                let content_type = mime_type(file_path);

                // Populate cache for next hit
                self.cache.put(file_path.to_string(), data.clone(), content_type.to_string());

                // ── Laminar pipeline: chunk + race codecs ────────
                self.serve_laminar(ctx, &data, content_type, keep_alive)
            }
            Err(_) => {
                Self::send_response(ctx.socket_fd, http::NOT_FOUND_RESPONSE);
                keep_alive
            }
        }
    }

    /// Build a single response as bytes (for pipelined batching).
    fn build_single_response(&mut self, path: &str) -> Vec<u8> {
        let date = http_date();
        match path {
            "/plaintext" => http::build_plaintext_response(date),
            "/json" => http::build_json_response(date),
            "/db" => {
                let conn = self.ensure_db();
                let id = conn.rand_id();
                let world = conn.get_world(id);
                let mut body = Vec::with_capacity(40);
                db::world_to_json(&world, &mut body);
                http::build_db_response(date, &body)
            }
            "/fortunes" => {
                let conn = self.ensure_db();
                let fortunes = conn.get_fortunes();
                let body = db::fortunes_to_html(&fortunes);
                http::build_html_response(date, &body)
            }
            p if p.starts_with("/queries") => {
                let count = db::parse_count_param(p, "queries");
                let conn = self.ensure_db();
                let worlds = conn.get_worlds(count);
                let body = db::worlds_to_json(&worlds);
                http::build_db_response(date, &body)
            }
            p if p.starts_with("/updates") => {
                let count = db::parse_count_param(p, "queries");
                let conn = self.ensure_db();
                let worlds = conn.update_worlds(count);
                let body = db::worlds_to_json(&worlds);
                http::build_db_response(date, &body)
            }
            p if p.starts_with("/cached-queries") => {
                let count = db::parse_count_param(p, "count");
                self.ensure_cache();
                let cache = self.db_cache.as_ref().unwrap();
                let db = self.db.as_mut().unwrap();
                let mut body = Vec::with_capacity(count * 40);
                body.push(b'[');
                for i in 0..count {
                    if i > 0 { body.push(b','); }
                    let id = db.rand_id();
                    let w = cache.get(id);
                    db::world_to_json(w, &mut body);
                }
                body.push(b']');
                http::build_db_response(date, &body)
            }
            _ => {
                let file_path = if path == "/" { "/index.html" } else { path };
                let full_path = format!("{}{}", self.root, file_path);

                if let Some(entry) = self.cache.get(file_path) {
                    return self.build_laminar_response(&entry.data, &entry.content_type);
                }

                match std::fs::read(&full_path) {
                    Ok(data) => {
                        let ct = mime_type(file_path);
                        self.cache.put(file_path.to_string(), data.clone(), ct.to_string());
                        self.build_laminar_response(&data, ct)
                    }
                    Err(_) => http::NOT_FOUND_RESPONSE.to_vec(),
                }
            }
        }
    }

    /// Build a Laminar-compressed response as bytes.
    fn build_laminar_response(&self, data: &[u8], content_type: &str) -> Vec<u8> {
        let result = laminar::race_chunk(data);
        match result.codec {
            laminar::CodecId::Identity => http::build_response(&result.data, content_type),
            laminar::CodecId::Gzip => http::build_compressed_response(&result.data, content_type, "gzip"),
            laminar::CodecId::Brotli => http::build_compressed_response(&result.data, content_type, "br"),
            laminar::CodecId::Deflate => http::build_compressed_response(&result.data, content_type, "deflate"),
        }
    }

    /// Serve from cache via Laminar pipeline.
    fn serve_cached(&self, ctx: &mut ConnContext, entry: &crate::cache::CacheEntry, _path: &str, keep_alive: bool) -> bool {
        self.serve_laminar(ctx, &entry.data, &entry.content_type, keep_alive)
    }

    /// Run data through Laminar codec racing and send as HTTP response.
    fn serve_laminar(&self, ctx: &mut ConnContext, data: &[u8], content_type: &str, keep_alive: bool) -> bool {
        // Race codecs on the best single-chunk result
        let result = laminar::race_chunk(data);

        let encoding = match result.codec {
            laminar::CodecId::Identity => "",
            laminar::CodecId::Gzip => "gzip",
            laminar::CodecId::Brotli => "br",
            laminar::CodecId::Deflate => "deflate",
        };

        let response = if encoding.is_empty() {
            http::build_response(&result.data, content_type)
        } else {
            http::build_compressed_response(&result.data, content_type, encoding)
        };

        Self::send_response(ctx.socket_fd, &response);
        keep_alive
    }

    /// Send bytes to socket.
    #[inline]
    fn send_response(fd: c_int, data: &[u8]) {
        let mut sent = 0;
        while sent < data.len() {
            let n = unsafe {
                libc::write(
                    fd,
                    data[sent..].as_ptr() as *const libc::c_void,
                    data.len() - sent,
                )
            };
            if n <= 0 {
                break;
            }
            sent += n as usize;
        }
    }
}

/// Simple MIME type detection from file extension.
fn mime_type(path: &str) -> &'static str {
    match path.rsplit('.').next() {
        Some("html") | Some("htm") => "text/html",
        Some("css") => "text/css",
        Some("js") | Some("mjs") => "application/javascript",
        Some("json") => "application/json",
        Some("svg") => "image/svg+xml",
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("webp") => "image/webp",
        Some("gif") => "image/gif",
        Some("ico") => "image/x-icon",
        Some("woff2") => "font/woff2",
        Some("woff") => "font/woff",
        Some("txt") => "text/plain",
        Some("xml") => "application/xml",
        Some("webmanifest") => "application/manifest+json",
        _ => "application/octet-stream",
    }
}
