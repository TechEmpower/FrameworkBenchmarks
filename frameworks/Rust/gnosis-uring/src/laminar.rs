//! Laminar Pipeline — per-chunk codec racing in Rust.
//!
//! The Hella-Whipped Laminar Pipeline:
//!   1. Read file in chunks (64KB default)
//!   2. FORK: race all codecs on each chunk
//!   3. RACE: pick smallest (raceMin)
//!   4. Build 10-byte Flow frame header
//!   5. writev/io_uring: header + compressed chunk
//!
//! Double-Wallington rotation:
//!   - Compress chunk N+1 while kernel sends chunk N
//!   - io_uring makes this free: submit write for N, compress N+1,
//!     then submit write for N+1. Kernel handles both concurrently.
//!
//! THM-TOPO-RACE-SUBSUMPTION: laminar wire <= sendfile wire (always).
//! THM-TOPO-RACE-IDENTITY-BASELINE: identity always participates.

use std::io::Write;
use flate2::Compression;
use flate2::write::{GzEncoder, DeflateEncoder};
use brotli::enc::BrotliEncoderParams;

/// Default chunk size: 64KB.
pub const CHUNK_SIZE: usize = 65_536;

/// Minimum size to attempt compression (below this, identity wins).
pub const MIN_COMPRESS_LEN: usize = 256;

/// Codec identifier for frame metadata.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum CodecId {
    Identity = 0,
    Gzip = 1,
    Brotli = 2,
    Deflate = 3,
}

/// Result of racing codecs on a single chunk.
pub struct ChunkResult {
    pub data: Vec<u8>,
    pub codec: CodecId,
    pub original_size: usize,
}

/// Race all four codecs on a chunk. Returns the smallest.
///
/// This is the FORK/RACE at the chunk level:
///   FORK -> [identity, gzip, brotli, deflate]
///   RACE -> smallest wins
///   VENT -> losers discarded
///
/// Identity is always a candidate (safety floor).
#[inline]
pub fn race_chunk(chunk: &[u8]) -> ChunkResult {
    let original_size = chunk.len();

    // Short-circuit tiny chunks
    if chunk.len() < MIN_COMPRESS_LEN {
        return ChunkResult {
            data: chunk.to_vec(),
            codec: CodecId::Identity,
            original_size,
        };
    }

    // Identity baseline
    let mut best = chunk.to_vec();
    let mut best_len = chunk.len();
    let mut best_codec = CodecId::Identity;

    // Gzip level 6
    {
        let mut enc = GzEncoder::new(Vec::with_capacity(chunk.len()), Compression::new(6));
        if enc.write_all(chunk).is_ok() {
            if let Ok(compressed) = enc.finish() {
                if compressed.len() < best_len {
                    best_len = compressed.len();
                    best = compressed;
                    best_codec = CodecId::Gzip;
                }
            }
        }
    }

    // Brotli quality 4
    {
        let mut compressed = Vec::with_capacity(chunk.len());
        let mut params = BrotliEncoderParams::default();
        params.quality = 4;
        if brotli::enc::BrotliCompress(
            &mut std::io::Cursor::new(chunk),
            &mut compressed,
            &params,
        ).is_ok() && compressed.len() < best_len
        {
            best_len = compressed.len();
            best = compressed;
            best_codec = CodecId::Brotli;
        }
    }

    // Deflate level 6
    {
        let mut enc = DeflateEncoder::new(Vec::with_capacity(chunk.len()), Compression::new(6));
        if enc.write_all(chunk).is_ok() {
            if let Ok(compressed) = enc.finish() {
                if compressed.len() < best_len {
                    best = compressed;
                    best_codec = CodecId::Deflate;
                }
            }
        }
    }

    ChunkResult {
        data: best,
        codec: best_codec,
        original_size,
    }
}

/// Build a 10-byte Aeon Flow frame header.
#[inline]
pub fn flow_header(stream_id: u16, sequence: u32, flags: u8, payload_len: u32) -> [u8; 10] {
    [
        (stream_id >> 8) as u8,
        (stream_id & 0xFF) as u8,
        (sequence >> 24) as u8,
        (sequence >> 16) as u8,
        (sequence >> 8) as u8,
        (sequence & 0xFF) as u8,
        flags,
        ((payload_len >> 16) & 0xFF) as u8,
        ((payload_len >> 8) & 0xFF) as u8,
        (payload_len & 0xFF) as u8,
    ]
}

/// Process an entire file through the Laminar pipeline.
///
/// Returns a Vec of (header, compressed_chunk) pairs ready for writev.
/// The caller decides how to send them (writev, io_uring, etc).
///
/// This separates compression from I/O so the executor can overlap:
///   - Compress chunk N+1 while io_uring sends chunk N
pub fn laminar_pipeline(data: &[u8], stream_id: u16) -> Vec<([u8; 10], Vec<u8>)> {
    let mut frames = Vec::new();
    let mut offset = 0usize;
    let mut sequence = 0u32;

    while offset < data.len() {
        let end = std::cmp::min(offset + CHUNK_SIZE, data.len());
        let chunk = &data[offset..end];
        let is_last = end >= data.len();

        let result = race_chunk(chunk);
        let flags = if is_last { 0x10 } else { 0x00 }; // FIN on last
        let header = flow_header(stream_id, sequence, flags, result.data.len() as u32);

        frames.push((header, result.data));
        offset = end;
        sequence += 1;
    }

    frames
}

/// Laminar pipeline stats for benchmarking.
pub struct LaminarStats {
    pub total_raw: usize,
    pub total_compressed: usize,
    pub total_framing: usize,
    pub total_wire: usize,
    pub chunk_count: usize,
    pub codec_wins: [usize; 4], // identity, gzip, brotli, deflate
}

/// Process data through Laminar and collect stats (no I/O).
pub fn laminar_stats(data: &[u8]) -> LaminarStats {
    let mut stats = LaminarStats {
        total_raw: data.len(),
        total_compressed: 0,
        total_framing: 0,
        total_wire: 0,
        chunk_count: 0,
        codec_wins: [0; 4],
    };

    let mut offset = 0usize;
    while offset < data.len() {
        let end = std::cmp::min(offset + CHUNK_SIZE, data.len());
        let chunk = &data[offset..end];

        let result = race_chunk(chunk);
        stats.codec_wins[result.codec as usize] += 1;
        stats.total_compressed += result.data.len();
        stats.chunk_count += 1;

        offset = end;
    }

    stats.total_framing = stats.chunk_count * 10;
    stats.total_wire = stats.total_compressed + stats.total_framing;
    stats
}
