//! io_uring executor — maps FORK/RACE/FOLD/VENT to SQ/CQ operations.
//!
//! Steals from the best:
//!   may-minihttp: coroutine-per-connection, buffer pooling
//!   h2o:          zero-copy sendfile, pipelined parsing
//!   JVM C2:       static DAG = all inlined at compile time (LTO)
//!   Vert.x:       multi-reactor (one ring per thread, no shared state)
//!
//! Our addition:
//!   Laminar codec racing via FORK/RACE on compression
//!   UDP Aeon Flow: 10-byte frames as datagrams, zero TCP overhead
//!
//! Topology primitives → io_uring:
//!   FORK  → batch SQE submissions
//!   RACE  → first CQE wins, IORING_ASYNC_CANCEL on losers
//!   FOLD  → gather CQEs
//!   VENT  → close fd, cancel ops

#[cfg(target_os = "linux")]
pub mod linux {
    use io_uring::{IoUring, opcode, types, cqueue};
    use std::os::fd::RawFd;
    use std::collections::HashMap;
    use std::pin::Pin;

    use crate::laminar;
    use crate::http;

    /// Ring size — power of 2. 4096 handles ~16K concurrent ops.
    const RING_SIZE: u32 = 4096;

    /// Read buffer per connection.
    /// 64KB read buffer — fits ~256 pipelined HTTP requests.
    const READ_BUF_SIZE: usize = 65536;

    // ── CQE user_data encoding ───────────────────────────────────
    //
    // We pack (event_type, conn_id) into a u64:
    //   bits 60-63: event type (4 bits = 16 types)
    //   bits  0-59: conn_id or other payload
    //
    // This avoids the >= comparison bug and is O(1) dispatch.

    const EVT_ACCEPT: u64  = 0;
    const EVT_READ: u64    = 1;
    const EVT_WRITE: u64   = 2;
    const EVT_CLOSE: u64   = 3;
    const EVT_UDP_RECV: u64 = 4;
    const EVT_UDP_SEND: u64 = 5;
    const EVT_SHIFT: u32   = 60;

    #[inline]
    fn pack_tag(evt: u64, id: u32) -> u64 {
        (evt << EVT_SHIFT) | (id as u64)
    }

    #[inline]
    fn unpack_tag(tag: u64) -> (u64, u32) {
        let evt = tag >> EVT_SHIFT;
        let id = (tag & ((1u64 << EVT_SHIFT) - 1)) as u32;
        (evt, id)
    }

    /// Per-connection state.
    ///
    /// Buffers are Box<[u8]> (heap-allocated, stable address).
    /// HashMap<u32, Conn> can resize without invalidating buffer pointers
    /// because Box doesn't move its heap allocation.
    struct Conn {
        fd: RawFd,
        /// Pinned read buffer — stable pointer for io_uring.
        read_buf: Pin<Box<[u8]>>,
        /// Write buffer lives here until the kernel finishes sending.
        write_buf: Option<Pin<Box<[u8]>>>,
        /// Track keep-alive state.
        keep_alive: bool,
    }

    impl Conn {
        fn new(fd: RawFd) -> Self {
            Self {
                fd,
                read_buf: Pin::new(vec![0u8; READ_BUF_SIZE].into_boxed_slice()),
                write_buf: None,
                keep_alive: true,
            }
        }

        /// Stable pointer to read buffer (safe across HashMap resizes).
        fn read_ptr(&self) -> *mut u8 {
            self.read_buf.as_ptr() as *mut u8
        }

        fn read_len(&self) -> u32 {
            self.read_buf.len() as u32
        }
    }

    /// UDP recv buffer (static, reused).
    struct UdpRecvBuf {
        buf: Pin<Box<[u8]>>,
    }

    impl UdpRecvBuf {
        fn new() -> Self {
            Self {
                buf: Pin::new(vec![0u8; 65536].into_boxed_slice()),
            }
        }
    }

    /// io_uring topology executor.
    pub struct UringExecutor {
        ring: IoUring,
        root: String,
        tcp_port: u16,
        udp_port: u16,
        conns: HashMap<u32, Conn>,
        next_id: u32,
        cache: crate::cache::FileCache,
        udp_buf: UdpRecvBuf,
    }

    impl UringExecutor {
        pub fn new(root: String, tcp_port: u16, udp_port: u16) -> std::io::Result<Self> {
            // Try SQPOLL (zero-syscall), fall back to normal ring
            let ring = IoUring::builder()
                .setup_sqpoll(2000)
                .build(RING_SIZE)
                .or_else(|_| IoUring::new(RING_SIZE))?;

            Ok(Self {
                ring,
                root,
                tcp_port,
                udp_port,
                conns: HashMap::with_capacity(1024),
                next_id: 1,
                cache: crate::cache::FileCache::new(64 * 1024 * 1024),
                udp_buf: UdpRecvBuf::new(),
            })
        }

        /// Main event loop — the entire server.
        pub fn run(&mut self) -> std::io::Result<()> {
            let tcp_fd = self.setup_tcp()?;
            let udp_fd = self.setup_udp()?;

            // FORK: seed the ring with accept + UDP recv
            self.push_accept(tcp_fd)?;
            self.push_udp_recv(udp_fd)?;
            self.ring.submit()?;

            eprintln!("gnosis-uring [io_uring] listening:");
            eprintln!("  TCP :{} (HTTP/1.1 for browsers)", self.tcp_port);
            eprintln!("  UDP :{} (Aeon Flow for topology clients)", self.udp_port);

            loop {
                self.ring.submit_and_wait(1)?;

                // Drain all CQEs — FOLD: gather completions
                let cqes: Vec<(u64, i32)> = self.ring.completion()
                    .map(|cqe| (cqe.user_data(), cqe.result()))
                    .collect();

                for (tag, result) in cqes {
                    let (evt, id) = unpack_tag(tag);

                    match evt {
                        EVT_ACCEPT => {
                            if result >= 0 {
                                self.on_accept(result as RawFd)?;
                            }
                            // Always re-arm accept
                            self.push_accept(tcp_fd)?;
                        }

                        EVT_READ => {
                            if result > 0 {
                                self.on_read(id, result as usize)?;
                            } else {
                                // EOF or error — VENT this connection
                                self.close_conn(id)?;
                            }
                        }

                        EVT_WRITE => {
                            self.on_write_done(id)?;
                        }

                        EVT_CLOSE => {
                            // fd closed, remove state
                            self.conns.remove(&id);
                        }

                        EVT_UDP_RECV => {
                            if result > 0 {
                                self.on_udp_recv(udp_fd, result as usize)?;
                            }
                            self.push_udp_recv(udp_fd)?;
                        }

                        EVT_UDP_SEND => {
                            // sent, nothing to do
                        }

                        _ => {}
                    }
                }
            }
        }

        // ═════════════════════════════════════════════════════════
        // Socket setup
        // ═════════════════════════════════════════════════════════

        fn setup_tcp(&self) -> std::io::Result<RawFd> {
            unsafe {
                let fd = libc::socket(libc::AF_INET, libc::SOCK_STREAM | libc::SOCK_NONBLOCK | libc::SOCK_CLOEXEC, 0);
                if fd < 0 { return Err(std::io::Error::last_os_error()); }

                let one: libc::c_int = 1;
                libc::setsockopt(fd, libc::SOL_SOCKET, libc::SO_REUSEPORT, &one as *const _ as *const _, 4);
                libc::setsockopt(fd, libc::SOL_SOCKET, libc::SO_REUSEADDR, &one as *const _ as *const _, 4);

                let mut addr: libc::sockaddr_in = std::mem::zeroed();
                addr.sin_family = libc::AF_INET as u16;
                addr.sin_port = self.tcp_port.to_be();
                addr.sin_addr.s_addr = 0; // INADDR_ANY

                if libc::bind(fd, &addr as *const _ as *const _, std::mem::size_of_val(&addr) as u32) < 0 {
                    libc::close(fd);
                    return Err(std::io::Error::last_os_error());
                }
                if libc::listen(fd, 4096) < 0 {
                    libc::close(fd);
                    return Err(std::io::Error::last_os_error());
                }
                Ok(fd)
            }
        }

        fn setup_udp(&self) -> std::io::Result<RawFd> {
            unsafe {
                let fd = libc::socket(libc::AF_INET, libc::SOCK_DGRAM | libc::SOCK_NONBLOCK | libc::SOCK_CLOEXEC, 0);
                if fd < 0 { return Err(std::io::Error::last_os_error()); }

                let one: libc::c_int = 1;
                libc::setsockopt(fd, libc::SOL_SOCKET, libc::SO_REUSEPORT, &one as *const _ as *const _, 4);

                let mut addr: libc::sockaddr_in = std::mem::zeroed();
                addr.sin_family = libc::AF_INET as u16;
                addr.sin_port = self.udp_port.to_be();
                addr.sin_addr.s_addr = 0;

                if libc::bind(fd, &addr as *const _ as *const _, std::mem::size_of_val(&addr) as u32) < 0 {
                    libc::close(fd);
                    return Err(std::io::Error::last_os_error());
                }
                Ok(fd)
            }
        }

        // ═════════════════════════════════════════════════════════
        // SQ submissions — FORK
        // ═════════════════════════════════════════════════════════

        fn push_accept(&mut self, listener: RawFd) -> std::io::Result<()> {
            let sqe = opcode::Accept::new(types::Fd(listener), std::ptr::null_mut(), std::ptr::null_mut())
                .build()
                .user_data(pack_tag(EVT_ACCEPT, 0));
            unsafe { self.ring.submission().push(&sqe).map_err(|_| sq_full()) }
        }

        fn push_read(&mut self, id: u32) -> std::io::Result<()> {
            let conn = self.conns.get(&id).ok_or_else(|| no_conn(id))?;
            let sqe = opcode::Recv::new(types::Fd(conn.fd), conn.read_ptr(), conn.read_len())
                .build()
                .user_data(pack_tag(EVT_READ, id));
            unsafe { self.ring.submission().push(&sqe).map_err(|_| sq_full()) }
        }

        fn push_write(&mut self, id: u32) -> std::io::Result<()> {
            let conn = self.conns.get(&id).ok_or_else(|| no_conn(id))?;
            let buf = conn.write_buf.as_ref().ok_or_else(|| no_conn(id))?;
            let sqe = opcode::Send::new(types::Fd(conn.fd), buf.as_ptr(), buf.len() as u32)
                .build()
                .user_data(pack_tag(EVT_WRITE, id));
            unsafe { self.ring.submission().push(&sqe).map_err(|_| sq_full()) }
        }

        fn push_close(&mut self, id: u32) -> std::io::Result<()> {
            let conn = self.conns.get(&id).ok_or_else(|| no_conn(id))?;
            let sqe = opcode::Close::new(types::Fd(conn.fd))
                .build()
                .user_data(pack_tag(EVT_CLOSE, id));
            unsafe { self.ring.submission().push(&sqe).map_err(|_| sq_full()) }
        }

        fn push_udp_recv(&mut self, udp_fd: RawFd) -> std::io::Result<()> {
            let sqe = opcode::Recv::new(
                types::Fd(udp_fd),
                self.udp_buf.buf.as_ptr() as *mut u8,
                self.udp_buf.buf.len() as u32,
            )
            .build()
            .user_data(pack_tag(EVT_UDP_RECV, 0));
            unsafe { self.ring.submission().push(&sqe).map_err(|_| sq_full()) }
        }

        // ═════════════════════════════════════════════════════════
        // CQ handlers — RACE / FOLD / VENT
        // ═════════════════════════════════════════════════════════

        /// New connection accepted — set TCP_NODELAY, submit first read.
        fn on_accept(&mut self, client_fd: RawFd) -> std::io::Result<()> {
            // TCP_NODELAY — steal from may-minihttp
            unsafe {
                let one: libc::c_int = 1;
                libc::setsockopt(client_fd, libc::IPPROTO_TCP, libc::TCP_NODELAY, &one as *const _ as *const _, 4);
            }

            let id = self.next_id;
            self.next_id = self.next_id.wrapping_add(1);
            if self.next_id == 0 { self.next_id = 1; } // skip 0

            self.conns.insert(id, Conn::new(client_fd));
            self.push_read(id)
        }

        /// Read completed — parse ALL pipelined HTTP requests, batch responses.
        ///
        /// LAMINAR HTTP pipelining:
        ///   1. FORK: parse N requests from one read buffer
        ///   2. PROCESS: build response for each request
        ///   3. FOLD: concatenate all responses into one write buffer
        ///   4. Submit one io_uring Send for the entire batch
        ///
        /// TechEmpower's wrk sends 16-256 pipelined requests per connection.
        /// Without pipelining: 1 req/read → 1 write → 1 CQE cycle per request.
        /// With pipelining: N reqs/read → 1 write → 1 CQE cycle for N requests.
        /// Throughput multiplied by pipeline depth.
        fn on_read(&mut self, id: u32, nbytes: usize) -> std::io::Result<()> {
            let (response, keep_alive) = {
                let conn = match self.conns.get(&id) {
                    Some(c) => c,
                    None => return Ok(()),
                };

                let buf = &conn.read_buf[..nbytes];

                // Parse ALL pipelined requests (FORK)
                let (requests, _consumed) = http::parse_pipelined(buf);

                if requests.is_empty() {
                    // No complete request found
                    (http::NOT_FOUND_RESPONSE.to_vec(), false)
                } else {
                    // Track keep-alive from last request in pipeline
                    let ka = requests.last().map(|(_, ka)| *ka).unwrap_or(false);

                    if requests.len() == 1 {
                        // Single request — fast path (no concat needed)
                        let path = std::str::from_utf8(&requests[0].0).unwrap_or("/");
                        (self.build_response(path), ka)
                    } else {
                        // Multiple pipelined requests — FOLD responses into one buffer
                        let mut batch = Vec::with_capacity(requests.len() * 128);
                        for (path_bytes, _) in &requests {
                            let path = std::str::from_utf8(path_bytes).unwrap_or("/");
                            let resp = self.build_response(path);
                            batch.extend_from_slice(&resp);
                        }
                        (batch, ka)
                    }
                }
            };

            // Store batched response and submit single write
            if let Some(conn) = self.conns.get_mut(&id) {
                conn.keep_alive = keep_alive;
                conn.write_buf = Some(Pin::new(response.into_boxed_slice()));
            }
            self.push_write(id)
        }

        /// Write completed — either re-read (keep-alive) or close (VENT).
        ///
        /// BUG FIX: previous version always closed after write.
        /// Now we check keep_alive and re-arm the read if the client
        /// wants to send more requests on this connection.
        fn on_write_done(&mut self, id: u32) -> std::io::Result<()> {
            let keep_alive = match self.conns.get_mut(&id) {
                Some(conn) => {
                    // Release write buffer — kernel is done with it
                    conn.write_buf = None;
                    conn.keep_alive
                }
                None => return Ok(()),
            };

            if keep_alive {
                // Keep-alive: submit another read on the same connection
                // This is the may-minihttp trick: reuse the connection
                self.push_read(id)
            } else {
                // Connection: close — VENT
                self.close_conn(id)
            }
        }

        /// Close a connection — VENT.
        fn close_conn(&mut self, id: u32) -> std::io::Result<()> {
            if self.conns.contains_key(&id) {
                self.push_close(id)?;
            }
            Ok(())
        }

        /// Handle UDP recv (Aeon Flow frame).
        fn on_udp_recv(&mut self, _udp_fd: RawFd, nbytes: usize) -> std::io::Result<()> {
            let buf = &self.udp_buf.buf[..nbytes];
            if let Some((_frame, _payload)) = super::parse_flow_request(buf) {
                // TODO: handle flow request
                // Parse path from payload, resolve file, compress, send back
            }
            Ok(())
        }

        // ═════════════════════════════════════════════════════════
        // Request handling — topology execution
        // ═════════════════════════════════════════════════════════

        /// Build HTTP response for a path.
        ///
        /// Topology: ParseHTTP → FORK(cache, disk) → RACE → Laminar → response
        fn build_response(&self, path: &str) -> Vec<u8> {
            match path {
                // TechEmpower hot paths — pre-built responses, zero alloc
                "/plaintext" => http::build_plaintext_response(crate::executor::http_date()),
                "/json" => http::build_json_response(crate::executor::http_date()),

                _ => {
                    let file_path = if path == "/" { "/index.html" } else { path };

                    // ── FORK: race(cache, disk) ──────────────────
                    // Arm 1: cache (microseconds)
                    if let Some(entry) = self.cache.get(file_path) {
                        return self.compress_and_respond(&entry.data, &entry.content_type);
                    }

                    // Arm 2: disk (milliseconds)
                    let full_path = format!("{}{}", self.root, file_path);
                    match std::fs::read(&full_path) {
                        Ok(data) => {
                            let ct = mime_type(file_path);
                            self.cache.put(file_path.to_string(), data.clone(), ct.to_string());
                            self.compress_and_respond(&data, ct)
                        }
                        Err(_) => http::NOT_FOUND_RESPONSE.to_vec(),
                    }
                }
            }
        }

        /// Laminar codec race + HTTP response construction.
        ///
        /// FORK: race(identity, gzip, brotli, deflate)
        /// RACE: pick smallest
        /// FOLD: header + body
        #[inline]
        fn compress_and_respond(&self, data: &[u8], content_type: &str) -> Vec<u8> {
            let result = laminar::race_chunk(data);
            match result.codec {
                laminar::CodecId::Identity => http::build_response(&result.data, content_type),
                laminar::CodecId::Gzip => http::build_compressed_response(&result.data, content_type, "gzip"),
                laminar::CodecId::Brotli => http::build_compressed_response(&result.data, content_type, "br"),
                laminar::CodecId::Deflate => http::build_compressed_response(&result.data, content_type, "deflate"),
            }
        }
    }

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
            Some("woff2") => "font/woff2",
            Some("txt") => "text/plain",
            _ => "application/octet-stream",
        }
    }

    fn sq_full() -> std::io::Error {
        std::io::Error::new(std::io::ErrorKind::Other, "io_uring SQ full")
    }

    fn no_conn(id: u32) -> std::io::Error {
        std::io::Error::new(std::io::ErrorKind::NotFound, format!("no conn {}", id))
    }
}

// ═══════════════════════════════════════════════════════════════════
// UDP Aeon Flow transport
// ═══════════════════════════════════════════════════════════════════

/// Aeon Flow frame — 10 bytes, self-describing, no connection state.
///
/// ```text
/// ┌──────────────┬──────────────┬───────┬──────────────┐
/// │ stream_id(2) │ sequence(4)  │ flags │ payload_len  │
/// │   u16 BE     │   u32 BE     │  u8   │   u24 BE     │
/// └──────────────┴──────────────┴───────┴──────────────┘
/// ```
///
/// Flags:
///   0x00 = DATA
///   0x10 = FIN (last frame)
///   0x20 = FORK (server-push)
///   0x40 = VENT (cancel/error)
///   0x80 = RACE (first-wins)
#[derive(Debug, Clone, Copy)]
pub struct FlowFrame {
    pub stream_id: u16,
    pub sequence: u32,
    pub flags: u8,
    pub payload_len: u32,
}

pub const FLOW_DATA: u8 = 0x00;
pub const FLOW_FIN: u8 = 0x10;
pub const FLOW_FORK: u8 = 0x20;
pub const FLOW_VENT: u8 = 0x40;
pub const FLOW_RACE: u8 = 0x80;

impl FlowFrame {
    #[inline]
    pub fn parse(buf: &[u8; 10]) -> Self {
        Self {
            stream_id: u16::from_be_bytes([buf[0], buf[1]]),
            sequence: u32::from_be_bytes([buf[2], buf[3], buf[4], buf[5]]),
            flags: buf[6],
            payload_len: ((buf[7] as u32) << 16) | ((buf[8] as u32) << 8) | (buf[9] as u32),
        }
    }

    #[inline]
    pub fn encode(&self) -> [u8; 10] {
        crate::laminar::flow_header(self.stream_id, self.sequence, self.flags, self.payload_len)
    }

    #[inline] pub fn is_fin(&self) -> bool { self.flags & FLOW_FIN != 0 }
    #[inline] pub fn is_fork(&self) -> bool { self.flags & FLOW_FORK != 0 }
    #[inline] pub fn is_vent(&self) -> bool { self.flags & FLOW_VENT != 0 }
    #[inline] pub fn is_race(&self) -> bool { self.flags & FLOW_RACE != 0 }
}

/// Parse a UDP datagram as an Aeon Flow request.
pub fn parse_flow_request(datagram: &[u8]) -> Option<(FlowFrame, &[u8])> {
    if datagram.len() < 10 { return None; }
    let header: [u8; 10] = datagram[..10].try_into().ok()?;
    let frame = FlowFrame::parse(&header);
    let end = std::cmp::min(10 + frame.payload_len as usize, datagram.len());
    Some((frame, &datagram[10..end]))
}

/// Build a flow response datagram.
pub fn build_flow_response(stream_id: u16, sequence: u32, flags: u8, payload: &[u8]) -> Vec<u8> {
    let header = crate::laminar::flow_header(stream_id, sequence, flags, payload.len() as u32);
    let mut out = Vec::with_capacity(10 + payload.len());
    out.extend_from_slice(&header);
    out.extend_from_slice(payload);
    out
}
