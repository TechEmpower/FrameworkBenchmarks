//! Raw PostgreSQL v3 wire protocol — just enough for TechEmpower.
//!
//! Cannon-style query pipelining: preload all IDs, write all queries
//! in one syscall, read all results in one syscall. No async runtime.
//!
//! PG v3 message format:
//!   Frontend → Backend: type(1) + len(4) + payload
//!   Backend → Frontend: type(1) + len(4) + payload
//!
//! We only implement:
//!   Startup, PasswordMessage, Parse, Bind, Describe, Execute, Sync
//!   And parse: AuthenticationOk, ReadyForQuery, ParseComplete,
//!   BindComplete, DataRow, CommandComplete, RowDescription

use std::io::{Read, Write};
use std::net::TcpStream;

/// A raw PG wire protocol connection with prepared statement support.
pub struct PgWire {
    stream: TcpStream,
    buf: Vec<u8>,       // reusable read buffer
    world_stmt: String, // prepared statement name
}

impl PgWire {
    /// Connect, authenticate, and prepare the world query.
    pub fn connect(host: &str, port: u16, user: &str, password: &str, dbname: &str) -> Self {
        let mut stream = TcpStream::connect((host, port)).expect("PG connect failed");
        stream.set_nodelay(true).ok();

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
        stream.write_all(&startup).unwrap();

        let buf = vec![0u8; 65536];
        let mut conn = Self {
            stream,
            buf,
            world_stmt: "w".to_string(),
        };

        // Read auth request
        loop {
            let (ty, payload) = conn.read_message();
            match ty {
                b'R' => {
                    // Authentication
                    let auth_type = i32::from_be_bytes([payload[0], payload[1], payload[2], payload[3]]);
                    match auth_type {
                        0 => {} // AuthenticationOk
                        3 => {
                            // CleartextPassword
                            conn.send_password(password);
                        }
                        5 => {
                            // MD5Password — salt is payload[4..8]
                            let salt = &payload[4..8];
                            let md5_pass = md5_password(user, password, salt);
                            conn.send_password(&md5_pass);
                        }
                        _ => panic!("Unsupported auth type: {}", auth_type),
                    }
                }
                b'K' => {} // BackendKeyData — ignore
                b'S' => {} // ParameterStatus — ignore
                b'Z' => break, // ReadyForQuery — done with auth
                b'E' => panic!("PG error during auth: {:?}", String::from_utf8_lossy(&payload)),
                _ => {} // ignore others
            }
        }

        // Prepare the world query
        conn.prepare(&conn.world_stmt.clone(), "SELECT id, randomnumber FROM world WHERE id=$1");

        conn
    }

    /// Send a password message.
    fn send_password(&mut self, password: &str) {
        let mut msg = Vec::with_capacity(8 + password.len());
        msg.push(b'p');
        let len = (4 + password.len() + 1) as i32;
        msg.extend_from_slice(&len.to_be_bytes());
        msg.extend_from_slice(password.as_bytes());
        msg.push(0);
        self.stream.write_all(&msg).unwrap();
    }

    /// Prepare a named statement.
    fn prepare(&mut self, name: &str, query: &str) {
        let mut msg = Vec::with_capacity(64 + query.len());

        // Parse message: 'P' + len + name\0 + query\0 + num_params(i16)
        msg.push(b'P');
        let len = (4 + name.len() + 1 + query.len() + 1 + 2) as i32;
        msg.extend_from_slice(&len.to_be_bytes());
        msg.extend_from_slice(name.as_bytes());
        msg.push(0);
        msg.extend_from_slice(query.as_bytes());
        msg.push(0);
        msg.extend_from_slice(&0i16.to_be_bytes()); // no param type hints

        // Sync
        msg.push(b'S');
        msg.extend_from_slice(&4i32.to_be_bytes());

        self.stream.write_all(&msg).unwrap();

        // Read until ReadyForQuery
        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'1' => {} // ParseComplete
                b'Z' => break,
                b'E' => panic!("PG error during prepare: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
    }

    /// Execute a single query by statement name. Returns (id, randomnumber).
    pub fn query_world(&mut self, id: i32) -> (i32, i32) {
        self.send_bind_execute(&self.world_stmt.clone(), id);
        self.send_sync();
        self.stream.flush().unwrap();

        let mut result = (0i32, 0i32);
        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'2' => {} // BindComplete
                b'D' => result = parse_world_row(&payload), // DataRow
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
        result
    }

    /// CANNON: Pipeline N world queries in one write, read all results.
    ///
    /// Preloads all IDs (kinetic energy), sends all Bind+Execute messages
    /// in one write syscall (launch velocity), reads all DataRows in
    /// one or few read syscalls (gather).
    pub fn query_worlds_pipelined(&mut self, ids: &[i32]) -> Vec<(i32, i32)> {
        // === LAUNCH: batch all queries into one write ===
        let mut msg = Vec::with_capacity(ids.len() * 32 + 8);
        let stmt = self.world_stmt.clone();
        for &id in ids {
            append_bind_execute(&mut msg, &stmt, id);
        }
        // Single Sync at the end
        msg.push(b'S');
        msg.extend_from_slice(&4i32.to_be_bytes());

        self.stream.write_all(&msg).unwrap();
        self.stream.flush().unwrap();

        // === GATHER: read all results ===
        let mut results = Vec::with_capacity(ids.len());
        let mut rows_collected = 0;

        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'2' => {} // BindComplete
                b'D' => {
                    results.push(parse_world_row(&payload));
                    rows_collected += 1;
                }
                b'C' => {} // CommandComplete
                b'Z' => break, // ReadyForQuery — all done
                b'E' => panic!("PG pipeline error at row {}: {:?}",
                    rows_collected, String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
        results
    }

    /// Execute a simple query (no params). Returns raw rows as strings.
    pub fn simple_query(&mut self, query: &str) -> Vec<Vec<String>> {
        let mut msg = Vec::with_capacity(8 + query.len());
        msg.push(b'Q');
        let len = (4 + query.len() + 1) as i32;
        msg.extend_from_slice(&len.to_be_bytes());
        msg.extend_from_slice(query.as_bytes());
        msg.push(0);
        self.stream.write_all(&msg).unwrap();
        self.stream.flush().unwrap();

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

    /// Execute a command (UPDATE, etc.) — no results expected.
    pub fn execute_command(&mut self, query: &str) {
        let mut msg = Vec::with_capacity(8 + query.len());
        msg.push(b'Q');
        let len = (4 + query.len() + 1) as i32;
        msg.extend_from_slice(&len.to_be_bytes());
        msg.extend_from_slice(query.as_bytes());
        msg.push(0);
        self.stream.write_all(&msg).unwrap();
        self.stream.flush().unwrap();

        loop {
            let (ty, payload) = self.read_message();
            match ty {
                b'C' => {} // CommandComplete
                b'Z' => break,
                b'E' => panic!("PG error: {:?}", String::from_utf8_lossy(&payload)),
                _ => {}
            }
        }
    }

    /// Send Bind + Execute for a prepared statement with one i32 param.
    fn send_bind_execute(&mut self, stmt_name: &str, param: i32) {
        let mut msg = Vec::with_capacity(64);
        append_bind_execute(&mut msg, stmt_name, param);
        self.stream.write_all(&msg).unwrap();
    }

    /// Send Sync message.
    fn send_sync(&mut self) {
        let msg = [b'S', 0, 0, 0, 4];
        self.stream.write_all(&msg).unwrap();
    }

    /// Read one message from the backend.
    fn read_message(&mut self) -> (u8, Vec<u8>) {
        // Read type (1 byte) + length (4 bytes)
        let mut header = [0u8; 5];
        self.stream.read_exact(&mut header).unwrap();
        let ty = header[0];
        let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
        let payload_len = len - 4;

        if payload_len == 0 {
            return (ty, Vec::new());
        }

        let mut payload = vec![0u8; payload_len];
        self.stream.read_exact(&mut payload).unwrap();
        (ty, payload)
    }
}

/// Append Bind + Execute messages to a buffer (for batching).
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
    buf.extend_from_slice(&0i16.to_be_bytes()); // no format codes (text)
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 param
    buf.extend_from_slice(&(param_str.len() as i32).to_be_bytes());
    buf.extend_from_slice(param_str.as_bytes());
    buf.extend_from_slice(&0i16.to_be_bytes()); // no result format codes (text)

    // Execute: 'E' + len + portal\0 + max_rows(i32=0 = unlimited)
    buf.push(b'E');
    buf.extend_from_slice(&9i32.to_be_bytes()); // 4 + 1 + 4
    buf.push(0); // unnamed portal
    buf.extend_from_slice(&0i32.to_be_bytes()); // unlimited rows
}

/// Parse a DataRow message into (id, randomnumber).
/// DataRow format: num_fields(i16) + [field_len(i32) + field_data]*
fn parse_world_row(payload: &[u8]) -> (i32, i32) {
    let mut pos = 2; // skip num_fields (we know it's 2)

    // Field 1: id
    let id_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let id: i32 = parse_int(&payload[pos..pos+id_len]);
    pos += id_len;

    // Field 2: randomnumber
    let rn_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]) as usize;
    pos += 4;
    let rn: i32 = parse_int(&payload[pos..pos+rn_len]);

    (id, rn)
}

/// Parse a DataRow into Vec<String> (for fortunes).
fn parse_string_row(payload: &[u8]) -> Vec<String> {
    let num_fields = i16::from_be_bytes([payload[0], payload[1]]) as usize;
    let mut pos = 2;
    let mut fields = Vec::with_capacity(num_fields);

    for _ in 0..num_fields {
        let field_len = i32::from_be_bytes([payload[pos], payload[pos+1], payload[pos+2], payload[pos+3]]);
        pos += 4;
        if field_len < 0 {
            fields.push(String::new()); // NULL
        } else {
            let len = field_len as usize;
            fields.push(String::from_utf8_lossy(&payload[pos..pos+len]).to_string());
            pos += len;
        }
    }
    fields
}

/// Parse an integer from text bytes.
#[inline]
fn parse_int(bytes: &[u8]) -> i32 {
    let s = unsafe { std::str::from_utf8_unchecked(bytes) };
    s.parse().unwrap()
}

/// Write a C-style null-terminated string.
fn write_cstr(buf: &mut Vec<u8>, s: &[u8]) {
    buf.extend_from_slice(s);
    buf.push(0);
}

/// MD5 password hash: md5(md5(password + user) + salt)
fn md5_password(user: &str, password: &str, salt: &[u8]) -> String {
    let inner = format!("{}{}", password, user);
    let inner_hash = md5_hex(inner.as_bytes());
    let mut outer_input = inner_hash.into_bytes();
    outer_input.extend_from_slice(salt);
    format!("md5{}", md5_hex(&outer_input))
}

/// Simple MD5 (implemented inline — no dependency needed for 64 bytes of code).
fn md5_hex(data: &[u8]) -> String {
    // Minimal MD5 implementation
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

    // Pad message
    let orig_len = data.len();
    let mut padded = data.to_vec();
    padded.push(0x80);
    while padded.len() % 64 != 56 {
        padded.push(0);
    }
    padded.extend_from_slice(&((orig_len * 8) as u64).to_le_bytes());

    // Process blocks
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
            a = d;
            d = c;
            c = b;
            b = b.wrapping_add(f.rotate_left(S[i]));
        }

        a0 = a0.wrapping_add(a);
        b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c);
        d0 = d0.wrapping_add(d);
    }

    format!("{:08x}{:08x}{:08x}{:08x}",
        a0.to_le(), b0.to_le(), c0.to_le(), d0.to_le())
}
