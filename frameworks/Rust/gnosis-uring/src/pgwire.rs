//! Raw PostgreSQL v3 wire protocol — just enough for TechEmpower.
//!
//! Cannon-style query pipelining: preload all IDs, write all queries
//! in one syscall, read all results in one syscall. No async runtime.
//!
//! Optimizations:
//!   - BufWriter batches all writes into one syscall
//!   - BufReader reduces read syscalls
//!   - Prepared statements for both world and fortune queries
//!   - Reusable write buffer for pipeline construction

use std::io::{BufReader, BufWriter, Read, Write};
use std::net::TcpStream;

/// A raw PG wire protocol connection with prepared statement support.
pub struct PgWire {
    reader: BufReader<TcpStream>,
    writer: BufWriter<TcpStream>,
    wbuf: Vec<u8>, // reusable write buffer for pipeline construction
}

impl PgWire {
    /// Connect, authenticate, and prepare statements.
    pub fn connect(host: &str, port: u16, user: &str, password: &str, dbname: &str) -> Self {
        let stream = TcpStream::connect((host, port)).expect("PG connect failed");
        stream.set_nodelay(true).ok();

        let reader = BufReader::with_capacity(65536, stream.try_clone().unwrap());
        let mut writer = BufWriter::with_capacity(65536, stream);

        // === Startup message (no type byte) ===
        let mut startup = Vec::with_capacity(128);
        startup.extend_from_slice(&[0u8; 4]); // length placeholder
        startup.extend_from_slice(&196608i32.to_be_bytes()); // protocol 3.0
        write_cstr(&mut startup, b"user");
        write_cstr(&mut startup, user.as_bytes());
        write_cstr(&mut startup, b"database");
        write_cstr(&mut startup, dbname.as_bytes());
        startup.push(0); // terminator
        let len = startup.len() as i32;
        startup[0..4].copy_from_slice(&len.to_be_bytes());
        writer.write_all(&startup).unwrap();
        writer.flush().unwrap();

        let mut conn = Self {
            reader,
            writer,
            wbuf: Vec::with_capacity(4096),
        };

        // Read auth flow
        loop {
            let (ty, payload) = conn.read_message();
            match ty {
                b'R' => {
                    let auth_type = i32::from_be_bytes([payload[0], payload[1], payload[2], payload[3]]);
                    match auth_type {
                        0 => {} // AuthenticationOk
                        3 => conn.send_password(password),
                        5 => {
                            let salt = &payload[4..8];
                            let md5_pass = md5_password(user, password, salt);
                            conn.send_password(&md5_pass);
                        }
                        _ => panic!("Unsupported auth type: {}", auth_type),
                    }
                }
                b'K' | b'S' => {} // BackendKeyData, ParameterStatus
                b'Z' => break,
                b'E' => panic!("PG auth error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }

        // Prepare both statements in one batch
        conn.wbuf.clear();

        // Parse "w" = world query
        append_parse(&mut conn.wbuf, "w", "SELECT id, randomnumber FROM world WHERE id=$1");
        // Parse "f" = fortune query
        append_parse(&mut conn.wbuf, "f", "SELECT id, message FROM fortune");
        // Single Sync
        conn.wbuf.push(b'S');
        conn.wbuf.extend_from_slice(&4i32.to_be_bytes());

        conn.writer.write_all(&conn.wbuf).unwrap();
        conn.writer.flush().unwrap();

        // Read until ReadyForQuery
        loop {
            let (ty, payload) = conn.read_message();
            match ty {
                b'1' => {} // ParseComplete
                b'Z' => break,
                b'E' => panic!("PG prepare error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
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

    /// Execute a single world query. Returns (id, randomnumber).
    pub fn query_world(&mut self, id: i32) -> (i32, i32) {
        // Bind + Execute + Sync in one write
        self.wbuf.clear();
        append_bind_execute(&mut self.wbuf, "w", id);
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut result = (0i32, 0i32);
        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'2' => {} // BindComplete
                b'D' => result = parse_world_row(&payload),
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
        result
    }

    /// CANNON: Pipeline N world queries in one write, read all results.
    pub fn query_worlds_pipelined(&mut self, ids: &[i32]) -> Vec<(i32, i32)> {
        // === LAUNCH: batch all queries into one write ===
        self.wbuf.clear();
        for &id in ids {
            append_bind_execute(&mut self.wbuf, "w", id);
        }
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        // === GATHER: read all results ===
        let mut results = Vec::with_capacity(ids.len());
        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'2' => {} // BindComplete
                b'D' => results.push(parse_world_row(&payload)),
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG pipeline error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
        results
    }

    /// Fetch all fortunes using prepared statement.
    pub fn query_fortunes(&mut self) -> Vec<(i32, String)> {
        // Bind + Execute + Sync for "f" (fortune stmt, no params)
        self.wbuf.clear();
        append_bind_execute_no_params(&mut self.wbuf, "f");
        self.wbuf.push(b'S');
        self.wbuf.extend_from_slice(&4i32.to_be_bytes());
        self.writer.write_all(&self.wbuf).unwrap();
        self.writer.flush().unwrap();

        let mut rows = Vec::with_capacity(16);
        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'2' => {} // BindComplete
                b'D' => {
                    let (id, msg) = parse_fortune_row(&payload);
                    rows.push((id, msg));
                }
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG fortune error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
        rows
    }

    /// Execute a simple query (for cache init, updates).
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
            let (ty, payload) = self.read_message();
            match ty {
                b'T' => {} // RowDescription
                b'D' => rows.push(parse_string_row(&payload)),
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
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
            let (ty, payload) = self.read_message();
            match ty {
                b'C' | b'T' => {}
                b'Z' => break,
                b'E' => panic!("PG error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
    }

    /// Read one message from the backend.
    #[inline]
    fn read_message(&mut self) -> (u8, Vec<u8>) {
        let mut header = [0u8; 5];
        self.reader.read_exact(&mut header).unwrap();
        let ty = header[0];
        let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
        let payload_len = len - 4;

        if payload_len == 0 {
            return (ty, Vec::new());
        }

        let mut payload = vec![0u8; payload_len];
        self.reader.read_exact(&mut payload).unwrap();
        (ty, payload)
    }
}

/// Append a Parse message to the buffer.
fn append_parse(buf: &mut Vec<u8>, name: &str, query: &str) {
    buf.push(b'P');
    let len = (4 + name.len() + 1 + query.len() + 1 + 2) as i32;
    buf.extend_from_slice(&len.to_be_bytes());
    buf.extend_from_slice(name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(query.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // no param type hints
}

/// Append Bind + Execute for a prepared statement with one i32 param.
fn append_bind_execute(buf: &mut Vec<u8>, stmt_name: &str, param: i32) {
    let param_str = itoa::Buffer::new().format(param).to_string();

    // Bind: 'B' + len + portal\0 + stmt\0 + num_format_codes(i16=0)
    //   + num_params(i16=1) + param_len(i32) + param_data + num_result_formats(i16=0)
    buf.push(b'B');
    let bind_len = 4 + 1 + stmt_name.len() + 1 + 2 + 2 + 4 + param_str.len() + 2;
    buf.extend_from_slice(&(bind_len as i32).to_be_bytes());
    buf.push(0); // unnamed portal
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // no format codes
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 param
    buf.extend_from_slice(&(param_str.len() as i32).to_be_bytes());
    buf.extend_from_slice(param_str.as_bytes());
    buf.extend_from_slice(&0i16.to_be_bytes()); // no result format codes

    // Execute: 'E' + len + portal\0 + max_rows(i32=0)
    buf.push(b'E');
    buf.extend_from_slice(&9i32.to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i32.to_be_bytes());
}

/// Append Bind + Execute for a prepared statement with no params.
fn append_bind_execute_no_params(buf: &mut Vec<u8>, stmt_name: &str) {
    // Bind with 0 params
    buf.push(b'B');
    let bind_len = 4 + 1 + stmt_name.len() + 1 + 2 + 2 + 2;
    buf.extend_from_slice(&(bind_len as i32).to_be_bytes());
    buf.push(0); // unnamed portal
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // no format codes
    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 params
    buf.extend_from_slice(&0i16.to_be_bytes()); // no result format codes

    // Execute
    buf.push(b'E');
    buf.extend_from_slice(&9i32.to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i32.to_be_bytes());
}

/// Parse a DataRow into (id, randomnumber).
#[inline]
fn parse_world_row(payload: &[u8]) -> (i32, i32) {
    let mut pos = 2; // skip num_fields

    let id_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let id: i32 = parse_int(&payload[pos..pos+id_len]);
    pos += id_len;

    let rn_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let rn: i32 = parse_int(&payload[pos..pos+rn_len]);

    (id, rn)
}

/// Parse a DataRow into (id, message) for fortunes.
#[inline]
fn parse_fortune_row(payload: &[u8]) -> (i32, String) {
    let mut pos = 2; // skip num_fields

    let id_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let id: i32 = parse_int(&payload[pos..pos+id_len]);
    pos += id_len;

    let msg_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let msg = String::from_utf8_lossy(&payload[pos..pos+msg_len]).to_string();

    (id, msg)
}

/// Parse a DataRow into Vec<String>.
fn parse_string_row(payload: &[u8]) -> Vec<String> {
    let num_fields = i16::from_be_bytes([payload[0], payload[1]]) as usize;
    let mut pos = 2;
    let mut fields = Vec::with_capacity(num_fields);
    for _ in 0..num_fields {
        let field_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]);
        pos += 4;
        if field_len < 0 {
            fields.push(String::new());
        } else {
            let len = field_len as usize;
            fields.push(String::from_utf8_lossy(&payload[pos..pos+len]).to_string());
            pos += len;
        }
    }
    fields
}

#[inline]
fn parse_int(bytes: &[u8]) -> i32 {
    // Fast integer parsing without going through str
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

    format!("{:08x}{:08x}{:08x}{:08x}",
        a0.to_le(), b0.to_le(), c0.to_le(), d0.to_le())
}
