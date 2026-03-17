//! LAMINAR Multiplexing — interleaved codec-raced frames across streams.
//!
//! Traditional multiplexing (HTTP/2, HTTP/3): send frames from different
//! streams in round-robin or priority order. Each frame is compressed
//! with a single fixed codec per stream.
//!
//! LAMINAR multiplexing: every chunk of every stream is independently
//! codec-raced. Frames from different streams are interleaved on the
//! same io_uring ring, with the codec race happening per-chunk.
//!
//! The pipeline:
//!   1. N streams active concurrently
//!   2. Each stream chunked at 64KB boundaries
//!   3. Each chunk: FORK(identity, gzip, brotli, deflate) → RACE → smallest
//!   4. Winning chunk wrapped in 10-byte Flow frame (stream_id identifies source)
//!   5. All frames from all streams submitted to io_uring as a batch
//!   6. Kernel sends them interleaved — Wallington Rotation across streams
//!
//! Why this wins:
//!   - Per-chunk optimal codec (not per-stream)
//!   - Batch SQE submission = one io_uring_enter for N frames
//!   - Stream interleaving prevents head-of-line blocking
//!   - Void walker learns across streams (JS always brotli, images always identity)

use crate::laminar::{self, CodecId, ChunkResult, CHUNK_SIZE};
use std::collections::HashMap;

/// A stream being multiplexed through the Laminar pipeline.
pub struct MuxStream {
    pub stream_id: u16,
    pub data: Vec<u8>,
    pub content_type: String,
    pub offset: usize,
    pub sequence: u32,
    pub done: bool,
}

impl MuxStream {
    pub fn new(stream_id: u16, data: Vec<u8>, content_type: String) -> Self {
        Self {
            stream_id,
            data,
            content_type,
            offset: 0,
            sequence: 0,
            done: false,
        }
    }

    /// Get next chunk, or None if stream is finished.
    fn next_chunk(&self) -> Option<&[u8]> {
        if self.offset >= self.data.len() {
            return None;
        }
        let end = std::cmp::min(self.offset + CHUNK_SIZE, self.data.len());
        Some(&self.data[self.offset..end])
    }

    /// Is this the last chunk?
    fn is_last_chunk(&self) -> bool {
        self.offset + CHUNK_SIZE >= self.data.len()
    }

    /// Advance to next chunk.
    fn advance(&mut self) {
        self.offset += CHUNK_SIZE;
        self.sequence += 1;
        if self.offset >= self.data.len() {
            self.done = true;
        }
    }
}

/// A prepared frame ready for io_uring submission.
pub struct MuxFrame {
    pub stream_id: u16,
    pub header: [u8; 10],
    pub payload: Vec<u8>,
    pub codec: CodecId,
}

/// Void walker state shared across streams.
///
/// Tracks which codecs win for each content type.
/// After warmup, prunes consistently-losing codecs.
/// This is the cross-stream learning that no other server does.
pub struct MuxVoidWalker {
    /// Codec win counts per content type prefix (e.g., "text/", "image/", "application/")
    wins: HashMap<String, [u32; 4]>,
    rounds: u32,
    warmup: u32,
}

impl MuxVoidWalker {
    pub fn new(warmup: u32) -> Self {
        Self {
            wins: HashMap::new(),
            rounds: 0,
            warmup,
        }
    }

    /// Record a codec win for a content type.
    pub fn record(&mut self, content_type: &str, codec: CodecId) {
        let prefix = content_prefix(content_type);
        let counts = self.wins.entry(prefix).or_insert([0; 4]);
        counts[codec as usize] += 1;
        self.rounds += 1;
    }

    /// Get the set of codecs to race for a given content type.
    /// After warmup, prunes codecs that never win for this type.
    pub fn codecs_for(&self, content_type: &str) -> Vec<CodecId> {
        if self.rounds < self.warmup {
            // During warmup, race all codecs
            return vec![CodecId::Identity, CodecId::Gzip, CodecId::Brotli, CodecId::Deflate];
        }

        let prefix = content_prefix(content_type);
        if let Some(counts) = self.wins.get(&prefix) {
            let total: u32 = counts.iter().sum();
            if total == 0 {
                return vec![CodecId::Identity, CodecId::Gzip, CodecId::Brotli, CodecId::Deflate];
            }

            let mut codecs = Vec::with_capacity(4);
            // Always include identity (safety floor)
            codecs.push(CodecId::Identity);

            // Include codecs that have won at least 5% of the time
            let threshold = total / 20; // 5%
            if counts[CodecId::Gzip as usize] > threshold { codecs.push(CodecId::Gzip); }
            if counts[CodecId::Brotli as usize] > threshold { codecs.push(CodecId::Brotli); }
            if counts[CodecId::Deflate as usize] > threshold { codecs.push(CodecId::Deflate); }

            codecs
        } else {
            vec![CodecId::Identity, CodecId::Gzip, CodecId::Brotli, CodecId::Deflate]
        }
    }
}

fn content_prefix(ct: &str) -> String {
    // Group by first part: "text/", "image/", "application/", "font/"
    ct.split('/').next().unwrap_or("other").to_string()
}

/// Multiplexed Laminar pipeline result.
pub struct MuxBatch {
    pub frames: Vec<MuxFrame>,
    pub streams_done: Vec<u16>,
    pub total_raw: usize,
    pub total_compressed: usize,
}

/// Run one round of LAMINAR multiplexing across active streams.
///
/// Takes one chunk from each active stream, races codecs on each,
/// and returns the batch of frames ready for io_uring submission.
///
/// This is the Wallington Rotation across streams:
///   Stream A chunk 0 | Stream B chunk 0 | Stream C chunk 0
///   Stream A chunk 1 | Stream B chunk 1 | Stream C chunk 1
///   ...
///
/// Each chunk independently codec-raced (Worthington Whip).
/// All frames batched for one io_uring_enter call.
pub fn mux_round(
    streams: &mut Vec<MuxStream>,
    walker: &mut MuxVoidWalker,
) -> MuxBatch {
    let mut frames = Vec::with_capacity(streams.len());
    let mut streams_done = Vec::new();
    let mut total_raw = 0usize;
    let mut total_compressed = 0usize;

    for stream in streams.iter_mut() {
        if stream.done {
            continue;
        }

        let chunk = match stream.next_chunk() {
            Some(c) => c,
            None => {
                stream.done = true;
                streams_done.push(stream.stream_id);
                continue;
            }
        };

        let is_last = stream.is_last_chunk();
        total_raw += chunk.len();

        // FORK: race codecs for this chunk
        // Void walker may prune codecs based on content type history
        let _active_codecs = walker.codecs_for(&stream.content_type);
        // For now, race all (void walker pruning is a CPU optimization, not correctness)
        let result = laminar::race_chunk(chunk);

        // Record win for void walker learning
        walker.record(&stream.content_type, result.codec);

        total_compressed += result.data.len();

        // Build flow frame
        let flags = if is_last { 0x10 } else { 0x00 }; // FIN on last
        let header = laminar::flow_header(
            stream.stream_id,
            stream.sequence,
            flags,
            result.data.len() as u32,
        );

        frames.push(MuxFrame {
            stream_id: stream.stream_id,
            header,
            payload: result.data,
            codec: result.codec,
        });

        stream.advance();
        if stream.done {
            streams_done.push(stream.stream_id);
        }
    }

    // Remove done streams
    streams.retain(|s| !s.done);

    MuxBatch {
        frames,
        streams_done,
        total_raw,
        total_compressed,
    }
}

/// Run the full LAMINAR multiplexing pipeline until all streams are done.
///
/// Returns all frames in interleaved order.
pub fn mux_all(
    mut streams: Vec<MuxStream>,
    walker: &mut MuxVoidWalker,
) -> Vec<MuxFrame> {
    let mut all_frames = Vec::new();

    while !streams.is_empty() {
        let batch = mux_round(&mut streams, walker);
        all_frames.extend(batch.frames);
    }

    all_frames
}

/// Stats from a mux run.
pub struct MuxStats {
    pub stream_count: usize,
    pub total_frames: usize,
    pub total_raw: usize,
    pub total_compressed: usize,
    pub total_framing: usize,
    pub total_wire: usize,
    pub codec_wins: [usize; 4],
    pub interleave_depth: usize, // max concurrent streams in any round
}

/// Run multiplexing and collect stats.
pub fn mux_stats(streams: Vec<MuxStream>) -> MuxStats {
    let stream_count = streams.len();
    let mut walker = MuxVoidWalker::new(8);
    let mut all_streams = streams;
    let mut total_raw = 0;
    let mut total_compressed = 0;
    let mut total_frames = 0;
    let mut codec_wins = [0usize; 4];
    let mut max_depth = 0;

    while !all_streams.is_empty() {
        let active = all_streams.len();
        if active > max_depth { max_depth = active; }

        let batch = mux_round(&mut all_streams, &mut walker);
        total_raw += batch.total_raw;
        total_compressed += batch.total_compressed;
        total_frames += batch.frames.len();

        for frame in &batch.frames {
            codec_wins[frame.codec as usize] += 1;
        }
    }

    let total_framing = total_frames * 10;
    MuxStats {
        stream_count,
        total_frames,
        total_raw,
        total_compressed,
        total_framing,
        total_wire: total_compressed + total_framing,
        codec_wins,
        interleave_depth: max_depth,
    }
}
