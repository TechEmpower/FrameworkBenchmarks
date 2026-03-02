//! PostgreSQL v3 wire protocol message encoding/decoding.
//!
//! Handles binary format for maximum efficiency — 30% less data
//! than text protocol for integer columns.

use std::io::{self, Read, Write};

// ── Frontend (client → server) messages ──────────────────────────────

/// Write a StartupMessage (no type byte).
/// Protocol version 3.0 = 196608.
pub fn write_startup(writer: &mut impl Write, user: &str, database: &str) -> io::Result<()> {
    let mut buf = Vec::with_capacity(64);
    buf.extend_from_slice(&[0u8; 4]);
    buf.extend_from_slice(&196608i32.to_be_bytes());
    buf.extend_from_slice(b"user\0");
    buf.extend_from_slice(user.as_bytes());
    buf.push(0);
    buf.extend_from_slice(b"database\0");
    buf.extend_from_slice(database.as_bytes());
    buf.push(0);
    buf.push(0);
    let len = buf.len() as i32;
    buf[0..4].copy_from_slice(&len.to_be_bytes());
    writer.write_all(&buf)
}

/// Write a Parse message (prepare a statement).
pub fn write_parse(
    writer: &mut impl Write,
    stmt_name: &str,
    query: &str,
    param_oids: &[u32],
) -> io::Result<()> {
    let body_len = 4 + stmt_name.len() + 1 + query.len() + 1 + 2 + param_oids.len() * 4;
    let mut buf = Vec::with_capacity(1 + body_len);
    buf.push(b'P');
    buf.extend_from_slice(&(body_len as i32).to_be_bytes());
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(query.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&(param_oids.len() as i16).to_be_bytes());
    for &oid in param_oids {
        buf.extend_from_slice(&oid.to_be_bytes());
    }
    writer.write_all(&buf)
}

/// Write a Sync message.
pub fn write_sync(writer: &mut impl Write) -> io::Result<()> {
    let buf: [u8; 5] = [b'S', 0, 0, 0, 4];
    writer.write_all(&buf)
}

/// Write an MD5 password message.
pub fn write_md5_password(
    writer: &mut impl Write,
    user: &str,
    password: &str,
    salt: &[u8; 4],
) -> io::Result<()> {
    let mut inner = md5_hash(password.as_bytes(), user.as_bytes());
    let inner_hex = md5_hex(&inner);
    let outer = md5_hash(inner_hex.as_bytes(), salt);
    let outer_hex = md5_hex(&outer);
    let password_msg = format!("md5{}", outer_hex);
    let body_len = 4 + password_msg.len() + 1;
    let mut buf = Vec::with_capacity(1 + body_len);
    buf.push(b'p');
    buf.extend_from_slice(&(body_len as i32).to_be_bytes());
    buf.extend_from_slice(password_msg.as_bytes());
    buf.push(0);
    inner.fill(0);
    writer.write_all(&buf)
}

// ── Buffered write functions (hot path — append to Vec, flush once) ──

/// Append a Bind message with i32 binary params to a buffer.
#[inline]
pub fn buf_bind_i32(buf: &mut Vec<u8>, stmt_name: &str, params: &[i32]) {
    let num_params = params.len();
    let body_len = (4
        + 1 + stmt_name.len() + 1
        + 2 + 2
        + 2
        + num_params * 8
        + 2 + 2) as i32;

    buf.push(b'B');
    buf.extend_from_slice(&body_len.to_be_bytes());
    buf.push(0); // unnamed portal
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 format code
    buf.extend_from_slice(&1i16.to_be_bytes()); // binary
    buf.extend_from_slice(&(num_params as i16).to_be_bytes());
    for &val in params {
        buf.extend_from_slice(&4i32.to_be_bytes());
        buf.extend_from_slice(&val.to_be_bytes());
    }
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 result format
    buf.extend_from_slice(&1i16.to_be_bytes()); // binary
}

/// Append a Bind message with no params to a buffer.
#[inline]
pub fn buf_bind_no_params(buf: &mut Vec<u8>, stmt_name: &str, result_formats: &[i16]) {
    let body_len = (4
        + 1 + stmt_name.len() + 1
        + 2 + 2
        + 2 + result_formats.len() * 2) as i32;

    buf.push(b'B');
    buf.extend_from_slice(&body_len.to_be_bytes());
    buf.push(0);
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 param format codes
    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 params
    buf.extend_from_slice(&(result_formats.len() as i16).to_be_bytes());
    for &fmt in result_formats {
        buf.extend_from_slice(&fmt.to_be_bytes());
    }
}

/// Append a Bind message with two int4[] binary array params to a buffer.
/// Used for batch UPDATE with unnest($1::int[], $2::int[]).
#[inline]
pub fn buf_bind_i32_arrays(buf: &mut Vec<u8>, stmt_name: &str, arr1: &[i32], arr2: &[i32]) {
    // Each int4 array in binary: 20 header bytes + N * 8 (len + value per element)
    let arr1_data_len = 20 + arr1.len() * 8;
    let arr2_data_len = 20 + arr2.len() * 8;

    let body_len = (4
        + 1 + stmt_name.len() + 1  // portal(\0) + stmt_name + \0
        + 2 + 2                      // 1 format code = binary
        + 2                          // num params = 2
        + 4 + arr1_data_len          // param1 length prefix + array data
        + 4 + arr2_data_len          // param2 length prefix + array data
        + 2) as i32;                 // 0 result format codes (UPDATE returns no rows)

    buf.push(b'B');
    buf.extend_from_slice(&body_len.to_be_bytes());
    buf.push(0); // unnamed portal
    buf.extend_from_slice(stmt_name.as_bytes());
    buf.push(0);
    buf.extend_from_slice(&1i16.to_be_bytes()); // 1 format code
    buf.extend_from_slice(&1i16.to_be_bytes()); // binary
    buf.extend_from_slice(&2i16.to_be_bytes()); // 2 params

    // Param 1: int4[] array
    write_i32_array(buf, arr1);
    // Param 2: int4[] array
    write_i32_array(buf, arr2);

    buf.extend_from_slice(&0i16.to_be_bytes()); // 0 result format codes
}

/// Write a single int4[] in PostgreSQL binary array format.
#[inline]
fn write_i32_array(buf: &mut Vec<u8>, values: &[i32]) {
    let data_len = (20 + values.len() * 8) as i32;
    buf.extend_from_slice(&data_len.to_be_bytes());  // param length
    buf.extend_from_slice(&1i32.to_be_bytes());       // ndim = 1
    buf.extend_from_slice(&0i32.to_be_bytes());       // has_nulls = 0
    buf.extend_from_slice(&23i32.to_be_bytes());      // element_oid = OID_INT4
    buf.extend_from_slice(&(values.len() as i32).to_be_bytes()); // dim size
    buf.extend_from_slice(&1i32.to_be_bytes());       // dim lower bound = 1
    for &val in values {
        buf.extend_from_slice(&4i32.to_be_bytes());   // element length = 4
        buf.extend_from_slice(&val.to_be_bytes());     // element value
    }
}

/// Append an Execute message to a buffer.
#[inline]
pub fn buf_execute(buf: &mut Vec<u8>) {
    buf.extend_from_slice(&[b'E', 0, 0, 0, 9, 0, 0, 0, 0, 0]);
}

/// Append a Sync message to a buffer.
#[inline]
pub fn buf_sync(buf: &mut Vec<u8>) {
    buf.extend_from_slice(&[b'S', 0, 0, 0, 4]);
}

// ── Backend (server → client) message reading ────────────────────────

/// Backend message types we care about.
#[derive(Debug)]
pub enum BackendMessage {
    AuthenticationOk,
    AuthenticationMd5Password([u8; 4]),
    AuthenticationCleartextPassword,
    AuthenticationSASL(Vec<u8>),
    ReadyForQuery,
    ParseComplete,
    BindComplete,
    DataRow(Vec<Option<Vec<u8>>>),
    CommandComplete,
    ErrorResponse(String),
    RowDescription,
    ParameterStatus,
    BackendKeyData,
    NoData,
    CloseComplete,
    Other(u8),
}

/// Read a single backend message (allocating — used for startup only).
pub fn read_message(reader: &mut impl Read) -> io::Result<BackendMessage> {
    let mut header = [0u8; 5];
    reader.read_exact(&mut header)?;

    let msg_type = header[0];
    let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
    let body_len = len.saturating_sub(4);
    let mut body = vec![0u8; body_len];
    if body_len > 0 {
        reader.read_exact(&mut body)?;
    }

    Ok(match msg_type {
        b'R' => {
            if body.len() >= 4 {
                let auth_type = i32::from_be_bytes([body[0], body[1], body[2], body[3]]);
                match auth_type {
                    0 => BackendMessage::AuthenticationOk,
                    3 => BackendMessage::AuthenticationCleartextPassword,
                    5 => {
                        let mut salt = [0u8; 4];
                        salt.copy_from_slice(&body[4..8]);
                        BackendMessage::AuthenticationMd5Password(salt)
                    }
                    10 => BackendMessage::AuthenticationSASL(body[4..].to_vec()),
                    _ => BackendMessage::Other(msg_type),
                }
            } else {
                BackendMessage::Other(msg_type)
            }
        }
        b'Z' => BackendMessage::ReadyForQuery,
        b'1' => BackendMessage::ParseComplete,
        b'2' => BackendMessage::BindComplete,
        b'D' => {
            let num_cols = i16::from_be_bytes([body[0], body[1]]) as usize;
            let mut cols = Vec::with_capacity(num_cols);
            let mut offset = 2;
            for _ in 0..num_cols {
                let col_len = i32::from_be_bytes([
                    body[offset], body[offset + 1], body[offset + 2], body[offset + 3],
                ]);
                offset += 4;
                if col_len < 0 {
                    cols.push(None);
                } else {
                    let end = offset + col_len as usize;
                    cols.push(Some(body[offset..end].to_vec()));
                    offset = end;
                }
            }
            BackendMessage::DataRow(cols)
        }
        b'C' => BackendMessage::CommandComplete,
        b'E' => {
            let mut msg = String::new();
            let mut i = 0;
            while i < body.len() && body[i] != 0 {
                let field_type = body[i];
                i += 1;
                let start = i;
                while i < body.len() && body[i] != 0 {
                    i += 1;
                }
                if field_type == b'M' {
                    msg = String::from_utf8_lossy(&body[start..i]).to_string();
                }
                i += 1;
            }
            BackendMessage::ErrorResponse(msg)
        }
        b'T' => BackendMessage::RowDescription,
        b'S' => BackendMessage::ParameterStatus,
        b'K' => BackendMessage::BackendKeyData,
        b'n' => BackendMessage::NoData,
        b'3' => BackendMessage::CloseComplete,
        _ => BackendMessage::Other(msg_type),
    })
}

/// Read messages until ReadyForQuery, collecting DataRows (allocating — startup only).
pub fn drain_until_ready(reader: &mut impl Read) -> io::Result<()> {
    loop {
        match read_message(reader)? {
            BackendMessage::ReadyForQuery => return Ok(()),
            BackendMessage::ErrorResponse(msg) => {
                return Err(io::Error::new(io::ErrorKind::Other, msg));
            }
            _ => {}
        }
    }
}

// ── Zero-alloc read functions (hot path) ─────────────────────────────

/// Read a message header. Returns (msg_type, body_len).
#[inline]
fn read_header(reader: &mut impl Read) -> io::Result<(u8, usize)> {
    let mut hdr = [0u8; 5];
    reader.read_exact(&mut hdr)?;
    let len = i32::from_be_bytes([hdr[1], hdr[2], hdr[3], hdr[4]]) as usize;
    Ok((hdr[0], len.saturating_sub(4)))
}

/// Read message body into a reusable buffer.
#[inline]
fn read_body(reader: &mut impl Read, buf: &mut Vec<u8>, body_len: usize) -> io::Result<()> {
    if buf.len() < body_len {
        buf.resize(body_len, 0);
    }
    if body_len > 0 {
        reader.read_exact(&mut buf[..body_len])?;
    }
    Ok(())
}

/// Read a single world row (2 binary int4 columns) without allocation.
pub fn read_single_world(reader: &mut impl Read, rbuf: &mut Vec<u8>) -> io::Result<(i32, i32)> {
    let mut result = None;
    loop {
        let (msg_type, body_len) = read_header(reader)?;
        read_body(reader, rbuf, body_len)?;
        match msg_type {
            b'D' => {
                // DataRow: [num_cols(2)][col1_len(4)][col1(4)][col2_len(4)][col2(4)]
                let id = i32::from_be_bytes([rbuf[6], rbuf[7], rbuf[8], rbuf[9]]);
                let rn = i32::from_be_bytes([rbuf[14], rbuf[15], rbuf[16], rbuf[17]]);
                result = Some((id, rn));
            }
            b'Z' => {
                return result.ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "no row"));
            }
            b'E' => {
                return Err(io::Error::new(io::ErrorKind::Other, "query error"));
            }
            _ => {}
        }
    }
}

/// Read multiple world rows (pipelined). Appends (id, randomNumber) to `out`.
pub fn read_world_rows(
    reader: &mut impl Read,
    rbuf: &mut Vec<u8>,
    out: &mut Vec<(i32, i32)>,
) -> io::Result<()> {
    loop {
        let (msg_type, body_len) = read_header(reader)?;
        read_body(reader, rbuf, body_len)?;
        match msg_type {
            b'D' => {
                let id = i32::from_be_bytes([rbuf[6], rbuf[7], rbuf[8], rbuf[9]]);
                let rn = i32::from_be_bytes([rbuf[14], rbuf[15], rbuf[16], rbuf[17]]);
                out.push((id, rn));
            }
            b'Z' => return Ok(()),
            b'E' => return Err(io::Error::new(io::ErrorKind::Other, "query error")),
            _ => {}
        }
    }
}

/// Read fortune rows (binary id + text message). Appends to `out`.
pub fn read_fortune_rows(
    reader: &mut impl Read,
    rbuf: &mut Vec<u8>,
    out: &mut Vec<(i32, String)>,
) -> io::Result<()> {
    loop {
        let (msg_type, body_len) = read_header(reader)?;
        read_body(reader, rbuf, body_len)?;
        match msg_type {
            b'D' => {
                // [num_cols(2)][col1_len(4)][col1(4)][col2_len(4)][col2(variable)]
                let id = i32::from_be_bytes([rbuf[6], rbuf[7], rbuf[8], rbuf[9]]);
                let text_len = i32::from_be_bytes([rbuf[10], rbuf[11], rbuf[12], rbuf[13]]) as usize;
                let message = String::from_utf8_lossy(&rbuf[14..14 + text_len]).to_string();
                out.push((id, message));
            }
            b'Z' => return Ok(()),
            b'E' => return Err(io::Error::new(io::ErrorKind::Other, "query error")),
            _ => {}
        }
    }
}

/// Drain messages until ReadyForQuery using a reusable buffer.
pub fn drain_until_ready_buf(reader: &mut impl Read, rbuf: &mut Vec<u8>) -> io::Result<()> {
    loop {
        let (msg_type, body_len) = read_header(reader)?;
        read_body(reader, rbuf, body_len)?;
        match msg_type {
            b'Z' => return Ok(()),
            b'E' => return Err(io::Error::new(io::ErrorKind::Other, "error")),
            _ => {}
        }
    }
}

// ── Buffer-based parsing (async DB — works on byte slices) ──────────

/// Scan a PG response buffer for the ReadyForQuery ('Z') message.
/// Returns `Some(bytes_consumed)` when found, `None` if incomplete.
#[inline]
pub fn try_find_ready(buf: &[u8]) -> Option<usize> {
    let mut pos = 0;
    while pos + 5 <= buf.len() {
        let msg_type = buf[pos];
        let len = i32::from_be_bytes([buf[pos + 1], buf[pos + 2], buf[pos + 3], buf[pos + 4]]) as usize;
        let msg_end = pos + 1 + len;
        if msg_end > buf.len() {
            return None; // incomplete message
        }
        if msg_type == b'Z' {
            return Some(msg_end);
        }
        pos = msg_end;
    }
    None
}

/// Parse a single world row from a complete PG response buffer.
#[inline]
pub fn parse_single_world_buf(buf: &[u8]) -> Option<(i32, i32)> {
    let mut pos = 0;
    while pos + 5 <= buf.len() {
        let msg_type = buf[pos];
        let len = i32::from_be_bytes([buf[pos + 1], buf[pos + 2], buf[pos + 3], buf[pos + 4]]) as usize;
        let msg_end = pos + 1 + len;
        if msg_type == b'D' {
            // Body: [num_cols(2)][col1_len(4)][col1(4)][col2_len(4)][col2(4)]
            let b = pos + 5;
            let id = i32::from_be_bytes([buf[b + 6], buf[b + 7], buf[b + 8], buf[b + 9]]);
            let rn = i32::from_be_bytes([buf[b + 14], buf[b + 15], buf[b + 16], buf[b + 17]]);
            return Some((id, rn));
        }
        if msg_type == b'Z' {
            return None;
        }
        pos = msg_end;
    }
    None
}

/// Parse world rows from a complete PG response buffer. Appends to `out`.
#[inline]
pub fn parse_world_rows_buf(buf: &[u8], out: &mut Vec<(i32, i32)>) {
    let mut pos = 0;
    while pos + 5 <= buf.len() {
        let msg_type = buf[pos];
        let len = i32::from_be_bytes([buf[pos + 1], buf[pos + 2], buf[pos + 3], buf[pos + 4]]) as usize;
        let msg_end = pos + 1 + len;
        match msg_type {
            b'D' => {
                let b = pos + 5;
                let id = i32::from_be_bytes([buf[b + 6], buf[b + 7], buf[b + 8], buf[b + 9]]);
                let rn = i32::from_be_bytes([buf[b + 14], buf[b + 15], buf[b + 16], buf[b + 17]]);
                out.push((id, rn));
            }
            b'Z' => return,
            _ => {}
        }
        pos = msg_end;
    }
}

/// Parse fortune rows from a complete PG response buffer. Appends to `out`.
#[inline]
pub fn parse_fortune_rows_buf(buf: &[u8], out: &mut Vec<(i32, String)>) {
    let mut pos = 0;
    while pos + 5 <= buf.len() {
        let msg_type = buf[pos];
        let len = i32::from_be_bytes([buf[pos + 1], buf[pos + 2], buf[pos + 3], buf[pos + 4]]) as usize;
        let msg_end = pos + 1 + len;
        match msg_type {
            b'D' => {
                let b = pos + 5;
                let id = i32::from_be_bytes([buf[b + 6], buf[b + 7], buf[b + 8], buf[b + 9]]);
                let text_len = i32::from_be_bytes([buf[b + 10], buf[b + 11], buf[b + 12], buf[b + 13]]) as usize;
                let message = String::from_utf8_lossy(&buf[b + 14..b + 14 + text_len]).to_string();
                out.push((id, message));
            }
            b'Z' => return,
            _ => {}
        }
        pos = msg_end;
    }
}

// ── Minimal MD5 implementation ───────────────────────────────────────

fn md5_hash(a: &[u8], b: &[u8]) -> [u8; 16] {
    let mut msg = Vec::with_capacity(a.len() + b.len());
    msg.extend_from_slice(a);
    msg.extend_from_slice(b);
    md5_compute(&msg)
}

fn md5_hex(hash: &[u8; 16]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut s = String::with_capacity(32);
    for &byte in hash {
        s.push(HEX[(byte >> 4) as usize] as char);
        s.push(HEX[(byte & 0xf) as usize] as char);
    }
    s
}

fn md5_compute(msg: &[u8]) -> [u8; 16] {
    let mut a0: u32 = 0x67452301;
    let mut b0: u32 = 0xefcdab89;
    let mut c0: u32 = 0x98badcfe;
    let mut d0: u32 = 0x10325476;

    let orig_len_bits = (msg.len() as u64) * 8;
    let mut padded = msg.to_vec();
    padded.push(0x80);
    while padded.len() % 64 != 56 {
        padded.push(0);
    }
    padded.extend_from_slice(&orig_len_bits.to_le_bytes());

    const S: [u32; 64] = [
        7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
        5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
        4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
        6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
    ];
    const K: [u32; 64] = [
        0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
        0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
        0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
        0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
        0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
        0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
        0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
        0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
        0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
        0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
        0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
        0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
        0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
        0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
        0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
        0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
    ];

    for chunk in padded.chunks_exact(64) {
        let mut m = [0u32; 16];
        for (i, c) in chunk.chunks_exact(4).enumerate() {
            m[i] = u32::from_le_bytes([c[0], c[1], c[2], c[3]]);
        }

        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);

        for i in 0..64 {
            let (f, g) = match i {
                0..=15 => ((b & c) | ((!b) & d), i),
                16..=31 => ((d & b) | ((!d) & c), (5 * i + 1) % 16),
                32..=47 => (b ^ c ^ d, (3 * i + 5) % 16),
                _ => (c ^ (b | (!d)), (7 * i) % 16),
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

    let mut result = [0u8; 16];
    result[0..4].copy_from_slice(&a0.to_le_bytes());
    result[4..8].copy_from_slice(&b0.to_le_bytes());
    result[8..12].copy_from_slice(&c0.to_le_bytes());
    result[12..16].copy_from_slice(&d0.to_le_bytes());
    result
}
