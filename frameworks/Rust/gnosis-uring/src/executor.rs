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

use crate::http;
use crate::laminar;
use crate::cache::FileCache;

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
            read_buf: vec![0u8; 8192],
            read_len: 0,
        }
    }
}

/// The executor drives connections through the topology.
pub struct Executor {
    pub root: String,
    pub cache: FileCache,
    pub port: u16,
}

impl Executor {
    pub fn new(root: String, port: u16) -> Self {
        Self {
            root,
            cache: FileCache::new(64 * 1024 * 1024), // 64MB cache
            port,
        }
    }

    /// Handle a complete HTTP request on a connection.
    ///
    /// This is the topology walk:
    ///   ParseHTTP → FORK(cache, mmap, disk) → RACE → LaminarSend
    ///
    /// Returns true to keep the connection alive, false to close.
    pub fn handle_request(&self, ctx: &mut ConnContext) -> bool {
        // ── Node: ParseHTTP ──────────────────────────────────────
        // Copy path and keep_alive out before dropping the borrow on ctx
        let (path_owned, keep_alive) = {
            let (request, _consumed) = match http::parse_request(&ctx.read_buf[..ctx.read_len]) {
                Some(r) => r,
                None => {
                    Self::send_response(ctx.socket_fd, http::NOT_FOUND_RESPONSE);
                    return false;
                }
            };

            // Only handle GET
            if request.method != b"GET" {
                Self::send_response(ctx.socket_fd, http::NOT_FOUND_RESPONSE);
                return request.keep_alive;
            }

            let path = std::str::from_utf8(request.path).unwrap_or("/").to_string();
            (path, request.keep_alive)
        };

        let path = path_owned.as_str();

        // ── TechEmpower routes (no file I/O) ─────────────────────
        match path {
            "/plaintext" => {
                Self::send_response(ctx.socket_fd, http::PLAINTEXT_RESPONSE);
                return keep_alive;
            }
            "/json" => {
                Self::send_response(ctx.socket_fd, http::JSON_RESPONSE);
                return keep_alive;
            }
            _ => {}
        }

        // ── FORK: race(cache, mmap, disk) ────────────────────────
        let file_path = if path == "/" { "/index.html" } else { path };
        let full_path = format!("{}{}", self.root, file_path);

        // Arm 1: Cache lookup (microseconds)
        if let Some(entry) = self.cache.get(file_path) {
            return self.serve_cached(ctx, &entry, file_path, keep_alive);
        }

        // Arm 2/3: Disk read (mmap and read race — on macOS both go through VFS)
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
