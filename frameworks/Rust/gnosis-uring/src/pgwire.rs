//! Raw PostgreSQL v3 wire protocol — just enough for TechEmpower.
//!
//! Cannon-style query pipelining with binary result format:
//!   - Binary i32 results: 4 bytes big-endian, no text parsing
//!   - Reusable read buffer: zero alloc in hot path
//!   - BufReader/BufWriter: minimal syscalls
//!   - One write for N queries, one read for N results

use std::io::{BufReader, BufWriter, Read, Write};
use std::net::TcpStream;
#[cfg(unix)]
use std::os::unix::net::UnixStream;

const READ_BUF_SIZE: usize = 65536;

/// Stream enum — zero-cost dispatch, no Box<dyn>.
enum PgStream {
    Tcp(TcpStream),
    #[cfg(unix)]
    Unix(UnixStream),
}

impl Read for PgStream {
    #[inline(always)]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            PgStream::Tcp(s) => s.read(buf),
            #[cfg(unix)]
            PgStream::Unix(s) => s.read(buf),
        }
    }
}

impl Write for PgStream {
    #[inline(always)]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            PgStream::Tcp(s) => s.write(buf),
            #[cfg(unix)]
            PgStream::Unix(s) => s.write(buf),
        }
    }
    #[inline(always)]
    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            PgStream::Tcp(s) => s.flush(),
            #[cfg(unix)]
            PgStream::Unix(s) => s.flush(),
        }
    }
}

impl PgStream {
    fn try_clone(&self) -> std::io::Result<Self> {
        match self {
            PgStream::Tcp(s) => Ok(PgStream::Tcp(s.try_clone()?)),
            #[cfg(unix)]
            PgStream::Unix(s) => Ok(PgStream::Unix(s.try_clone()?)),
        }
    }
}

/// A raw PG wire protocol connection.
pub struct PgWire {
    reader: BufReader<PgStream>,
    writer: BufWriter<PgStream>,
    wbuf: Vec<u8>,
    rbuf: Vec<u8>,
}

impl PgWire {
    pub fn connect(host: &str, port: u16, user: &str, password: &str, dbname: &str) -> Self {
        // Try Unix domain socket first (faster — no TCP overhead)
        #[cfg(unix)]
        {
            let socket_path = format!("/var/run/postgresql/.s.PGSQL.{}", port);
            let socket_path_tmp = format!("/tmp/.s.PGSQL.{}", port);
            let uds_path = if std::path::Path::new(&socket_path).exists() {
                Some(socket_path)
            } else if std::path::Path::new(&socket_path_tmp).exists() {
                Some(socket_path_tmp)
            } else {
                None
            };

            if let Some(path) = uds_path {
                if let Ok(stream) = UnixStream::connect(&path) {
                    eprintln!("  PG: connected via Unix socket {}", path);
                    let s = PgStream::Unix(stream);
                    let reader = BufReader::with_capacity(READ_BUF_SIZE, s.try_clone().unwrap());
                    let writer = BufWriter::with_capacity(READ_BUF_SIZE, s);
                    return Self::init(reader, writer, user, password, dbname);
                }
            }
        }

        // TCP fallback
        let stream = TcpStream::connect((host, port)).expect("PG connect failed");
        stream.set_nodelay(true).ok();
        let s = PgStream::Tcp(stream);
        let reader = BufReader::with_capacity(READ_BUF_SIZE, s.try_clone().unwrap());
        let writer = BufWriter::with_capacity(READ_BUF_SIZE, s);
        Self::init(reader, writer, user, password, dbname)
    }

    fn init(
        reader: BufReader<PgStream>,
        mut writer: BufWriter<PgStream>,
        user: &str,
        password: &str,
        dbname: &str,
    ) -> Self {

        // Startup message
        let mut startup = Vec::with_capacity(128);
        startup.extend_from_slice(&[0u8; 4]);
        startup.extend_from_slice(&196608i32.to_be_bytes());
        write_cstr(&mut startup, b"user");
        write_cstr(&mut startup, user.as_bytes());
        write_cstr(&mut startup, b"database");
        write_cstr(&mut startup, dbname.as_bytes());
        startup.push(0);
        let len = startup.len() as i32;
        startup[0..4].copy_from_slice(&len.to_be_bytes());
        writer.write_all(&startup).unwrap();
        writer.flush().unwrap();

        let mut conn = Self {
            reader,
            writer,
            wbuf: Vec::with_capacity(8192),
            rbuf: vec![0u8; 4096],
        };

        // Auth flow
        loop {
            let (ty, plen) = conn.read_msg_header();
            match ty {
                b'R' => {
                    conn.read_payload(plen);
                    let auth = i32::from_be_bytes([conn.rbuf[0], conn.rbuf[1], conn.rbuf[2], conn.rbuf[3]]);
                    match auth {
                        0 => {}
                        3 => conn.send_password(password),
                        5 => {
                            let salt = [conn.rbuf[4], conn.rbuf[5], conn.rbuf[6], conn.rbuf[7]];
                            let md5 = md5_password(user, password, &salt);
                            conn.send_password(&md5);
                        }
                        _ => panic!("Unsupported auth: {}", auth),
                    }
                }
                b'Z' => { conn.skip_payload(plen); break; }
                _ => { conn.skip_payload(plen); }
            }
        }

        // Prepare statements: "w" = world (binary results), "f" = fortune (text results)
        conn.wbuf.clear();
        // Parse "w" with param type hint (INT4 = OID 23)
        append_parse_typed(&mut conn.wbuf, "w", "SELECT id, randomnumber FROM world WHERE id=$1", 23);
        // Parse "f" (no params)
        append_parse(&mut conn.wbuf, "f", "SELECT id, message FROM fortune");
        conn.wbuf.push(b'S');
        conn.wbuf.extend_from_slice(&4i32.to_be_bytes());
        conn.writer.write_all(&conn.wbuf).unwrap();
        conn.writer.flush().unwrap();

        loop {
            let (ty, plen) = conn.read_msg_header();
            match ty {
                b'Z' => { conn.skip_payload(plen); break; }
                _ => { conn.skip_payload(plen); }
            }
        }

        conn
    }

    fn send_password(&mut self, password: &str) {
        let len = (4 + password.len() + 1) as i32;
        self.writer.write_all(&[b'p']).unwrap();
        self.writer.write_all(&len.to_be_bytes()).unwrap();
        self.writer.write_all(password.as_bytes()).unwrap();
        self.writer.write_all(&[0]).unwrap();
        self.writer.flush().unwrap();
    }

    /// Single world query with binary results. Returns (id, randomnumber).
    #[inline]
    pub fn query_world(&mut self, id: i32) -> (i32, i32) {
        self.wbuf.clear();
        append_bind_execute_binary(&mut self.wbuf, "w", id);
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut result = (0i32, 0i32);
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'D' => {
                    self.read_payload(plen);
                    result = parse_world_row_binary(&self.rbuf);
                }
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }
        result
    }

    /// CANNON: Pipeline N world queries with binary results.
    pub fn query_worlds_pipelined(&mut self, ids: &[i32]) -> Vec<(i32, i32)> {
        self.wbuf.clear();
        for &id in ids {
            append_bind_execute_binary(&mut self.wbuf, "w", id);
        }
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut results = Vec::with_capacity(ids.len());
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'D' => {
                    self.read_payload(plen);
                    results.push(parse_world_row_binary(&self.rbuf));
                }
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }
        results
    }

    /// OSCILLATING UPDATE: SELECTs + UPDATE in one write, one read.
    ///
    /// Since we know the ids and new randomNumbers before querying,
    /// we can send all 20 Bind+Execute (SELECTs) + the UPDATE query
    /// in a single write. PG processes them in order, and we read
    /// all results in one pass. Two round-trips become one.
    pub fn query_and_update_pipelined(
        &mut self,
        ids: &[i32],
        new_randoms: &[i32],
    ) -> Vec<(i32, i32)> {
        self.wbuf.clear();

        // Phase 1: All SELECT Bind+Executes
        for &id in ids {
            append_bind_execute_binary(&mut self.wbuf, "w", id);
        }

        // Phase 2: Build and append UPDATE as Simple Query
        // (Simple Query has its own implicit sync, so we don't need an explicit Sync between phases)
        let mut sql = String::with_capacity(64 + ids.len() * 16);
        sql.push_str("UPDATE world SET randomnumber = v.r FROM (VALUES ");

        // Build sorted (id, new_random) pairs for the UPDATE
        let mut pairs: Vec<(i32, i32)> = ids.iter().zip(new_randoms.iter())
            .map(|(&id, &rn)| (id, rn))
            .collect();
        pairs.sort_by_key(|&(id, _)| id);

        for (i, &(id, rn)) in pairs.iter().enumerate() {
            if i > 0 { sql.push(','); }
            sql.push('(');
            sql.push_str(itoa::Buffer::new().format(id));
            sql.push(',');
            sql.push_str(itoa::Buffer::new().format(rn));
            sql.push(')');
        }
        sql.push_str(") AS v(i, r) WHERE world.id = v.i");

        // Append as Simple Query (Q message) — has its own implicit transaction
        self.wbuf.push(b'Q');
        let qlen = (4 + sql.len() + 1) as i32;
        self.wbuf.extend_from_slice(&qlen.to_be_bytes());
        self.wbuf.extend_from_slice(sql.as_bytes());
        self.wbuf.push(0);

        // Single Sync for the extended query (SELECT) portion
        // Note: Simple Query (UPDATE) doesn't need Sync, it self-syncs
        // But we need Sync BEFORE the Simple Query to flush extended query results
        // Actually, let's restructure: SELECTs with Sync, then UPDATE as Simple Query
        // Rewrite: clear and redo properly

        self.wbuf.clear();

        // SELECTs via extended query
        for &id in ids {
            append_bind_execute_binary(&mut self.wbuf, "w", id);
        }
        // Sync to end the extended query batch
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());

        // UPDATE via Simple Query (will execute after SELECTs complete)
        self.wbuf.push(b'Q');
        let qlen = (4 + sql.len() + 1) as i32;
        self.wbuf.extend_from_slice(&qlen.to_be_bytes());
        self.wbuf.extend_from_slice(sql.as_bytes());
        self.wbuf.push(0);

        // ONE write for everything
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        // Read SELECT results
        let mut results = Vec::with_capacity(ids.len());
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'D' => {
                    self.read_payload(plen);
                    results.push(parse_world_row_binary(&self.rbuf));
                }
                b'Z' => { self.skip_payload(plen); break; } // End of extended query batch
                _ => { self.skip_payload(plen); }
            }
        }

        // Read UPDATE result (Simple Query: CommandComplete + ReadyForQuery)
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }

        results
    }

    /// Fetch fortunes using prepared statement (text results for strings).
    pub fn query_fortunes(&mut self) -> Vec<(i32, String)> {
        self.wbuf.clear();
        append_bind_execute_no_params(&mut self.wbuf, "f");
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut rows = Vec::with_capacity(16);
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'D' => {
                    self.read_payload(plen);
                    rows.push(parse_fortune_row(&self.rbuf));
                }
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }
        rows
    }

    /// Simple query for cache init.
    pub fn simple_query(&mut self, query: &str) -> Vec<Vec<String>> {
        self.wbuf.clear();
        self.wbuf.push(b'Q');
        let len = (4 + query.len() + 1) as i32;
        self.wbuf.extend_from_slice(&len.to_be_bytes());
        self.wbuf.extend_from_slice(query.as_bytes());
        self.wbuf.push(0);
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut rows = Vec::new();
        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'D' => {
                    self.read_payload(plen);
                    rows.push(parse_string_row(&self.rbuf));
                }
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }
        rows
    }

    /// Execute a command (UPDATE, etc.).
    pub fn execute_command(&mut self, query: &str) {
        self.wbuf.clear();
        self.wbuf.push(b'Q');
        let len = (4 + query.len() + 1) as i32;
        self.wbuf.extend_from_slice(&len.to_be_bytes());
        self.wbuf.extend_from_slice(query.as_bytes());
        self.wbuf.push(0);
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        loop {
            let (ty, plen) = self.read_msg_header();
            match ty {
                b'Z' => { self.skip_payload(plen); break; }
                _ => { self.skip_payload(plen); }
            }
        }
    }

    /// Read message header: (type, payload_length). Does NOT read payload.
    #[inline(always)]
    fn read_msg_header(&mut self) -> (u8, usize) {
        let mut h = [0u8; 5];
        self.reader.read_exact(&mut h).unwrap();
        let plen = i32::from_be_bytes([h[1], h[2], h[3], h[4]]) as usize - 4;
        (h[0], plen)
    }

    /// Read payload into reusable buffer.
    #[inline(always)]
    fn read_payload(&mut self, len: usize) {
        if self.rbuf.len() < len {
            self.rbuf.resize(len, 0);
        }
        self.reader.read_exact(&mut self.rbuf[..len]).unwrap();
    }

    /// Skip payload we don't need.
    #[inline(always)]
    fn skip_payload(&mut self, len: usize) {
        if len == 0 { return; }
        if self.rbuf.len() < len {
            self.rbuf.resize(len, 0);
        }
        self.reader.read_exact(&mut self.rbuf[..len]).unwrap();
    }
}

// ── Message builders ──────────────────────────────────────────

fn append_parse(buf: &mut Vec<u8>, name: &str, query: &str) {
    buf.push(b'P');
    let len = (4 + name.len() + 1 + query.len() + 1 + 2) as i32;
    buf.extend_from_slice(&len.to_be_bytes());
    buf.extend_from_slice(name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(query.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 param type hints
}

fn append_parse_typed(buf: &mut Vec<u8>, name: &str, query: &str, param_oid: i32) {
    buf.push(b'P');
    let len = (4 + name.len() + 1 + query.len() + 1 + 2 + 4) as i32;
    buf.extend_from_slice(&len.to_be_bytes());
    buf.extend_from_slice(name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(query.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 param type hint
    buf.extend_from_slice(&param_oid.to_be_bytes()); // OID
}

/// Bind + Execute with one i32 param, requesting BINARY results.
fn append_bind_execute_binary(buf: &mut Vec<u8>, stmt: &str, param: i32) {
    // Bind: portal="" + stmt + format_codes=[1(binary)] + params=[binary i32] + result_formats=[1(binary)]
    buf.push(b'B');
    let bind_len = 4
        + 1                     // portal \0
        + stmt.len() + 1       // stmt \0
        + 2 + 2                // num_param_format_codes(1) + format(binary=1)
        + 2                     // num_params(1)
        + 4 + 4                // param_len(4) + param_data(i32)
        + 2 + 2;               // num_result_format_codes(1) + format(binary=1)
    buf.extend_from_slice(&(bind_len as i32).to_be_bytes());
    buf.push(0); // unnamed portal
    buf.extend_from_slice(stmt.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 param format code
    buf.extend_from_slice(&1i16.to_be_bytes()); // binary
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 param
    buf.extend_from_slice(&4i32.to_be_bytes()); // param length = 4
    buf.extend_from_slice(&param.to_be_bytes()); // param value (binary i32)
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 result format code
    buf.extend_from_slice(&1i16.to_be_bytes()); // binary

    // Execute
    buf.push(b'E');
    buf.extend_from_slice(&9i32.to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i32.to_be_bytes());
}

/// Bind + Execute with no params, text results.
fn append_bind_execute_no_params(buf: &mut Vec<u8>, stmt: &str) {
    buf.push(b'B');
    let bind_len = 4 + 1 + stmt.len() + 1 + 2 + 2 + 2;
    buf.extend_from_slice(&(bind_len as i32).to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(stmt.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // no format codes
    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 params
    buf.extend_from_slice(&0i16.to_be_bytes()); // no result format codes (text)

    buf.push(b'E');
    buf.extend_from_slice(&9i32.to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i32.to_be_bytes());
}

// ── Row parsers ───────────────────────────────────────────────

/// Parse a binary DataRow: 2 fields, each 4-byte big-endian i32.
#[inline(always)]
fn parse_world_row_binary(buf: &[u8]) -> (i32, i32) {
    // DataRow: num_fields(2) + [field_len(4) + field_data(4)] * 2
    // With binary format, fields are raw i32 big-endian
    let id = i32::from_be_bytes([buf[6], buf[7], buf[8], buf[9]]);
    let rn = i32::from_be_bytes([buf[14], buf[15], buf[16], buf[17]]);
    (id, rn)
}

/// Parse a text DataRow into (id, message) for fortunes.
#[inline]
fn parse_fortune_row(buf: &[u8]) -> (i32, String) {
    let mut pos = 2;

    let id_len = i32::from_be_bytes([buf[pos], buf[pos+1], buf[pos+2], buf[pos+3]]) as usize;
    pos += 4;
    let id = parse_int(&buf[pos..pos+id_len]);
    pos += id_len;

    let msg_len = i32::from_be_bytes([buf[pos], buf[pos+1], buf[pos+2], buf[pos+3]]) as usize;
    pos += 4;
    let msg = String::from_utf8_lossy(&buf[pos..pos+msg_len]).to_string();

    (id, msg)
}

/// Parse a text DataRow into Vec<String>.
fn parse_string_row(buf: &[u8]) -> Vec<String> {
    let num = i16::from_be_bytes([buf[0], buf[1]]) as usize;
    let mut pos = 2;
    let mut fields = Vec::with_capacity(num);
    for _ in 0..num {
        let fl = i32::from_be_bytes([buf[pos], buf[pos+1], buf[pos+2], buf[pos+3]]);
        pos += 4;
        if fl < 0 {
            fields.push(String::new());
        } else {
            let len = fl as usize;
            fields.push(String::from_utf8_lossy(&buf[pos..pos+len]).to_string());
            pos += len;
        }
    }
    fields
}

#[inline(always)]
fn parse_int(bytes: &[u8]) -> i32 {
    let mut n: i32 = 0;
    let mut neg = false;
    for &b in bytes {
        if b == b'-' { neg = true; continue; }
        n = n * 10 + (b - b'0') as i32;
    }
    if neg { -n } else { n }
}

fn write_cstr(buf: &mut Vec<u8>, s: &[u8]) {
    buf.extend_from_slice(s);
    buf.push(0);
}

fn md5_password(user: &str, password: &str, salt: &[u8]) -> String {
    let inner = format!("{}{}", password, user);
    let inner_hash = md5_hex(inner.as_bytes());
    let mut outer_input = inner_hash.into_bytes();
    outer_input.extend_from_slice(salt);
    format!("md5{}", md5_hex(&outer_input))
}

fn md5_hex(data: &[u8]) -> String {
    let mut a0: u32 = 0x67452301;
    let mut b0: u32 = 0xefcdab89;
    let mut c0: u32 = 0x98badcfe;
    let mut d0: u32 = 0x10325476;
    static S: [u32; 64] = [
        7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
        5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
        4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
        6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21,
    ];
    static K: [u32; 64] = [
        0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
        0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
        0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
        0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
        0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
        0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
        0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
        0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391,
    ];
    let orig_len = data.len();
    let mut padded = data.to_vec();
    padded.push(0x80);
    while padded.len() % 64 != 56 { padded.push(0); }
    padded.extend_from_slice(&((orig_len * 8) as u64).to_le_bytes());
    for chunk in padded.chunks(64) {
        let mut m = [0u32; 16];
        for (i, c) in chunk.chunks(4).enumerate() {
            m[i] = u32::from_le_bytes([c[0], c[1], c[2], c[3]]);
        }
        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);
        for i in 0..64 {
            let (f, g) = match i {
                0..=15 => ((b & c) | ((!b) & d), i),
                16..=31 => ((d & b) | ((!d) & c), (5*i + 1) % 16),
                32..=47 => (b ^ c ^ d, (3*i + 5) % 16),
                _ => (c ^ (b | (!d)), (7*i) % 16),
            };
            let f = f.wrapping_add(a).wrapping_add(K[i]).wrapping_add(m[g]);
            a = d; d = c; c = b;
            b = b.wrapping_add(f.rotate_left(S[i]));
        }
        a0 = a0.wrapping_add(a); b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c); d0 = d0.wrapping_add(d);
    }
    format!("{:08x}{:08x}{:08x}{:08x}", a0.to_le(), b0.to_le(), c0.to_le(), d0.to_le())
}
