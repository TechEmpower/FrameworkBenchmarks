//! SCRAM-SHA-256 authentication for PostgreSQL.
//!
//! Implements RFC 5802 / RFC 7677 with zero external dependencies.
//! Used for PostgreSQL 14+ which defaults to scram-sha-256.

use std::io::{self, BufReader, Read, Write};
use std::net::TcpStream;

// ── SHA-256 ──────────────────────────────────────────────────────────

const SHA256_K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
];

fn sha256(data: &[u8]) -> [u8; 32] {
    let mut h: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
    ];

    let bit_len = (data.len() as u64) * 8;
    let mut padded = data.to_vec();
    padded.push(0x80);
    while padded.len() % 64 != 56 {
        padded.push(0);
    }
    padded.extend_from_slice(&bit_len.to_be_bytes());

    for chunk in padded.chunks_exact(64) {
        let mut w = [0u32; 64];
        for i in 0..16 {
            w[i] = u32::from_be_bytes([
                chunk[i * 4], chunk[i * 4 + 1],
                chunk[i * 4 + 2], chunk[i * 4 + 3],
            ]);
        }
        for i in 16..64 {
            let s0 = w[i-15].rotate_right(7) ^ w[i-15].rotate_right(18) ^ (w[i-15] >> 3);
            let s1 = w[i-2].rotate_right(17) ^ w[i-2].rotate_right(19) ^ (w[i-2] >> 10);
            w[i] = w[i-16].wrapping_add(s0).wrapping_add(w[i-7]).wrapping_add(s1);
        }

        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] = h;

        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ ((!e) & g);
            let t1 = hh.wrapping_add(s1).wrapping_add(ch).wrapping_add(SHA256_K[i]).wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let t2 = s0.wrapping_add(maj);

            hh = g; g = f; f = e;
            e = d.wrapping_add(t1);
            d = c; c = b; b = a;
            a = t1.wrapping_add(t2);
        }

        h[0] = h[0].wrapping_add(a); h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c); h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e); h[5] = h[5].wrapping_add(f);
        h[6] = h[6].wrapping_add(g); h[7] = h[7].wrapping_add(hh);
    }

    let mut result = [0u8; 32];
    for (i, &v) in h.iter().enumerate() {
        result[i*4..i*4+4].copy_from_slice(&v.to_be_bytes());
    }
    result
}

// ── HMAC-SHA-256 ─────────────────────────────────────────────────────

fn hmac_sha256(key: &[u8], message: &[u8]) -> [u8; 32] {
    let mut k = [0u8; 64];
    if key.len() > 64 {
        let h = sha256(key);
        k[..32].copy_from_slice(&h);
    } else {
        k[..key.len()].copy_from_slice(key);
    }

    let mut ipad = [0x36u8; 64];
    let mut opad = [0x5cu8; 64];
    for i in 0..64 {
        ipad[i] ^= k[i];
        opad[i] ^= k[i];
    }

    let mut inner = Vec::with_capacity(64 + message.len());
    inner.extend_from_slice(&ipad);
    inner.extend_from_slice(message);
    let inner_hash = sha256(&inner);

    let mut outer = [0u8; 96];
    outer[..64].copy_from_slice(&opad);
    outer[64..96].copy_from_slice(&inner_hash);
    sha256(&outer)
}

// ── PBKDF2-HMAC-SHA-256 ─────────────────────────────────────────────

fn pbkdf2_sha256(password: &[u8], salt: &[u8], iterations: u32) -> [u8; 32] {
    // Single block (i=1) since we need exactly 32 bytes
    let mut msg = Vec::with_capacity(salt.len() + 4);
    msg.extend_from_slice(salt);
    msg.extend_from_slice(&1u32.to_be_bytes()); // block index = 1

    let mut u = hmac_sha256(password, &msg);
    let mut result = u;

    for _ in 1..iterations {
        u = hmac_sha256(password, &u);
        for j in 0..32 {
            result[j] ^= u[j];
        }
    }
    result
}

// ── Base64 ───────────────────────────────────────────────────────────

const B64_CHARS: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn base64_encode(data: &[u8]) -> String {
    let mut out = String::with_capacity((data.len() + 2) / 3 * 4);
    for chunk in data.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = if chunk.len() > 1 { chunk[1] as u32 } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] as u32 } else { 0 };
        let n = (b0 << 16) | (b1 << 8) | b2;

        out.push(B64_CHARS[((n >> 18) & 63) as usize] as char);
        out.push(B64_CHARS[((n >> 12) & 63) as usize] as char);
        if chunk.len() > 1 {
            out.push(B64_CHARS[((n >> 6) & 63) as usize] as char);
        } else {
            out.push('=');
        }
        if chunk.len() > 2 {
            out.push(B64_CHARS[(n & 63) as usize] as char);
        } else {
            out.push('=');
        }
    }
    out
}

fn base64_decode(s: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(s.len() * 3 / 4);
    let bytes: Vec<u8> = s.bytes()
        .filter(|&b| b != b'=')
        .map(|b| match b {
            b'A'..=b'Z' => b - b'A',
            b'a'..=b'z' => b - b'a' + 26,
            b'0'..=b'9' => b - b'0' + 52,
            b'+' => 62,
            b'/' => 63,
            _ => 0,
        })
        .collect();

    for chunk in bytes.chunks(4) {
        if chunk.len() >= 2 {
            out.push((chunk[0] << 2) | (chunk[1] >> 4));
        }
        if chunk.len() >= 3 {
            out.push((chunk[1] << 4) | (chunk[2] >> 2));
        }
        if chunk.len() >= 4 {
            out.push((chunk[2] << 6) | chunk[3]);
        }
    }
    out
}

// ── SCRAM-SHA-256 Protocol ───────────────────────────────────────────

/// Perform SCRAM-SHA-256 over a BufReader<TcpStream>.
pub fn authenticate_bufreader(
    stream: &mut BufReader<TcpStream>,
    _user: &str,
    password: &str,
    sasl_body: &[u8],
) -> io::Result<()> {
    // Verify server supports SCRAM-SHA-256
    let mechanisms = String::from_utf8_lossy(sasl_body);
    if !mechanisms.contains("SCRAM-SHA-256") {
        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Server does not support SCRAM-SHA-256",
        ));
    }

    // Generate client nonce
    let nonce = generate_nonce();

    // Client-first-message-bare: n=<user>,r=<nonce>
    let client_first_bare = format!("n=,r={}", nonce);
    // Full client-first-message: gs2-header + bare
    let client_first = format!("n,,{}", client_first_bare);

    // Send SASLInitialResponse
    send_sasl_initial_response(stream.get_mut(), &client_first)?;

    // Read AuthenticationSASLContinue (auth_type=11)
    let server_first = read_sasl_continue(stream)?;

    // Parse server-first-message
    let (server_nonce, salt_b64, iterations) = parse_server_first(&server_first)?;

    // Verify server nonce starts with our client nonce
    if !server_nonce.starts_with(&nonce) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Server nonce doesn't match",
        ));
    }

    // Derive keys
    let salt = base64_decode(salt_b64);
    let salted_password = pbkdf2_sha256(password.as_bytes(), &salt, iterations);
    let client_key = hmac_sha256(&salted_password, b"Client Key");
    let stored_key = sha256(&client_key);
    let server_key = hmac_sha256(&salted_password, b"Server Key");

    // Client-final-message-without-proof
    let channel_binding = base64_encode(b"n,,");
    let client_final_without_proof = format!("c={},r={}", channel_binding, server_nonce);

    // AuthMessage = client-first-bare + "," + server-first + "," + client-final-without-proof
    let auth_message = format!("{},{},{}", client_first_bare, server_first, client_final_without_proof);

    // ClientSignature = HMAC(StoredKey, AuthMessage)
    let client_signature = hmac_sha256(&stored_key, auth_message.as_bytes());

    // ClientProof = ClientKey XOR ClientSignature
    let mut client_proof = [0u8; 32];
    for i in 0..32 {
        client_proof[i] = client_key[i] ^ client_signature[i];
    }

    // ServerSignature = HMAC(ServerKey, AuthMessage)
    let expected_server_sig = hmac_sha256(&server_key, auth_message.as_bytes());

    // Send client-final-message
    let client_final = format!("{},p={}", client_final_without_proof, base64_encode(&client_proof));
    send_sasl_response(stream.get_mut(), &client_final)?;

    // Read AuthenticationSASLFinal (auth_type=12)
    let server_final = read_sasl_final(stream)?;

    // Verify server signature
    if let Some(sig_b64) = server_final.strip_prefix("v=") {
        let server_sig = base64_decode(sig_b64);
        if server_sig != expected_server_sig {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Server signature mismatch",
            ));
        }
    }

    // Read AuthenticationOk (auth_type=0)
    let mut header = [0u8; 5];
    stream.read_exact(&mut header)?;
    let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
    let body_len = len.saturating_sub(4);
    let mut body = vec![0u8; body_len];
    if body_len > 0 {
        stream.read_exact(&mut body)?;
    }

    Ok(())
}

fn generate_nonce() -> String {
    // Use thread ID + address of stack variable for uniqueness
    let mut seed: u64 = 0;
    let ptr = &seed as *const u64 as u64;
    seed = ptr ^ 0x517cc1b727220a95;
    // Simple xorshift for randomness
    seed ^= seed << 13;
    seed ^= seed >> 7;
    seed ^= seed << 17;
    base64_encode(&seed.to_le_bytes())
        .replace('+', "A")
        .replace('/', "B")
        .replace('=', "")
}

fn send_sasl_initial_response(writer: &mut impl Write, client_first: &str) -> io::Result<()> {
    let mechanism = b"SCRAM-SHA-256\0";
    let response_bytes = client_first.as_bytes();
    let body_len = 4 + mechanism.len() + 4 + response_bytes.len();
    let mut buf = Vec::with_capacity(1 + body_len);
    buf.push(b'p'); // SASLInitialResponse
    buf.extend_from_slice(&(body_len as i32).to_be_bytes());
    buf.extend_from_slice(mechanism);
    buf.extend_from_slice(&(response_bytes.len() as i32).to_be_bytes());
    buf.extend_from_slice(response_bytes);
    writer.write_all(&buf)
}

fn send_sasl_response(writer: &mut impl Write, client_final: &str) -> io::Result<()> {
    let response_bytes = client_final.as_bytes();
    let body_len = 4 + response_bytes.len();
    let mut buf = Vec::with_capacity(1 + body_len);
    buf.push(b'p'); // SASLResponse
    buf.extend_from_slice(&(body_len as i32).to_be_bytes());
    buf.extend_from_slice(response_bytes);
    writer.write_all(&buf)
}

fn read_sasl_continue(reader: &mut impl Read) -> io::Result<String> {
    let mut header = [0u8; 5];
    reader.read_exact(&mut header)?;
    if header[0] != b'R' {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "Expected Authentication message"));
    }
    let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
    let body_len = len.saturating_sub(4);
    let mut body = vec![0u8; body_len];
    if body_len > 0 {
        reader.read_exact(&mut body)?;
    }
    // auth_type should be 11 (SASLContinue)
    let auth_type = i32::from_be_bytes([body[0], body[1], body[2], body[3]]);
    if auth_type != 11 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Expected AuthenticationSASLContinue (11), got {}", auth_type),
        ));
    }
    Ok(String::from_utf8_lossy(&body[4..]).to_string())
}

fn read_sasl_final(reader: &mut impl Read) -> io::Result<String> {
    let mut header = [0u8; 5];
    reader.read_exact(&mut header)?;
    if header[0] != b'R' {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "Expected Authentication message"));
    }
    let len = i32::from_be_bytes([header[1], header[2], header[3], header[4]]) as usize;
    let body_len = len.saturating_sub(4);
    let mut body = vec![0u8; body_len];
    if body_len > 0 {
        reader.read_exact(&mut body)?;
    }
    let auth_type = i32::from_be_bytes([body[0], body[1], body[2], body[3]]);
    if auth_type != 12 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Expected AuthenticationSASLFinal (12), got {}", auth_type),
        ));
    }
    Ok(String::from_utf8_lossy(&body[4..]).to_string())
}

fn parse_server_first(msg: &str) -> io::Result<(&str, &str, u32)> {
    let mut nonce = "";
    let mut salt = "";
    let mut iterations = 0u32;

    for part in msg.split(',') {
        if let Some(val) = part.strip_prefix("r=") {
            nonce = val;
        } else if let Some(val) = part.strip_prefix("s=") {
            salt = val;
        } else if let Some(val) = part.strip_prefix("i=") {
            iterations = val.parse().map_err(|_| {
                io::Error::new(io::ErrorKind::InvalidData, "Invalid iteration count")
            })?;
        }
    }

    if nonce.is_empty() || salt.is_empty() || iterations == 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Incomplete server-first-message",
        ));
    }

    Ok((nonce, salt, iterations))
}
