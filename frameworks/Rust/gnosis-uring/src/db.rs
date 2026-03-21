//! PostgreSQL database operations for TechEmpower benchmarks.
//!
//! Uses raw PG wire protocol (pgwire.rs) for cannon-style pipelining:
//! - Preload all IDs (kinetic energy)
//! - Write all queries in one syscall (launch velocity)
//! - Read all results in one syscall (gather)
//!
//! No async runtime. No external PG crate. Zero overhead.

use nanorand::{Rng, WyRand};
use crate::pgwire::PgWire;

/// World table row.
pub struct WorldRow {
    pub id: i32,
    pub random_number: i32,
}

/// Fortune table row.
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

/// Number of PG connections per thread for query fan-out.
/// Keep low to avoid exceeding PG max_connections (default 100).
/// With 32 threads: 32 × (1 primary + 2 pool) = 96 connections.
const PG_POOL_SIZE: usize = 2;

/// Database connection pool with cannon-style pipelined queries.
/// Multiple connections enable PG-side parallelism for multi-query endpoints.
pub struct DbConn {
    pub pg: PgWire,          // primary connection (single queries, fortunes, cache init)
    pool: Vec<PgWire>,       // fan-out pool for multi-query endpoints
    pub rng: WyRand,
}

impl DbConn {
    pub fn new() -> Self {
        let host = std::env::var("DBHOST").unwrap_or_else(|_| "tfb-database".to_string());
        let port: u16 = std::env::var("PGPORT").unwrap_or_else(|_| "5432".to_string()).parse().unwrap();

        let pg = PgWire::connect(&host, port, "benchmarkdbuser", "benchmarkdbpass", "hello_world");

        // Fan-out pool for multi-query endpoints
        let pool: Vec<PgWire> = (0..PG_POOL_SIZE)
            .map(|_| PgWire::connect(&host, port, "benchmarkdbuser", "benchmarkdbpass", "hello_world"))
            .collect();

        Self {
            pg,
            pool,
            rng: WyRand::new(),
        }
    }

    #[inline]
    pub fn rand_id(&mut self) -> i32 {
        (self.rng.generate_range(0_u32..10000) as i32) + 1
    }

    /// Single database query — /db
    pub fn get_world(&mut self, id: i32) -> WorldRow {
        let (rid, rn) = self.pg.query_world(id);
        WorldRow { id: rid, random_number: rn }
    }

    /// Multiple queries — /queries (FAN-OUT CANNON)
    ///
    /// FORK queries across PG_POOL_SIZE connections.
    /// Write to all, then read from all sequentially.
    /// PG processes connections concurrently.
    pub fn get_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        if count <= 1 {
            let id = self.rand_id();
            let w = self.get_world(id);
            return vec![w];
        }

        let ids: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();
        let results = self.fan_out_queries(&ids);

        results.into_iter().map(|(id, rn)| {
            WorldRow { id, random_number: rn }
        }).collect()
    }

    /// Fan-out queries across pool connections.
    /// Write all, then read all — PG server-side parallelism does the work.
    fn fan_out_queries(&mut self, ids: &[i32]) -> Vec<(i32, i32)> {
        let n = self.pool.len();

        // FORK: distribute IDs across connections
        let mut chunks: Vec<Vec<i32>> = (0..n).map(|_| Vec::new()).collect();
        let mut result_map: Vec<(usize, usize)> = Vec::with_capacity(ids.len());
        for (i, &id) in ids.iter().enumerate() {
            let conn_idx = i % n;
            let local_idx = chunks[conn_idx].len();
            chunks[conn_idx].push(id);
            result_map.push((conn_idx, local_idx));
        }

        // LAUNCH: write to all connections (fills BufWriter, one flush each)
        for (conn_idx, chunk) in chunks.iter().enumerate() {
            if !chunk.is_empty() {
                self.pool[conn_idx].write_pipelined_queries(chunk);
            }
        }

        // GATHER: read from all connections sequentially
        // By the time we read conn[1], PG has already processed it while we read conn[0]
        let mut conn_results: Vec<Vec<(i32, i32)>> = (0..n).map(|_| Vec::new()).collect();
        for (conn_idx, chunk) in chunks.iter().enumerate() {
            if !chunk.is_empty() {
                conn_results[conn_idx] = self.pool[conn_idx].read_pipelined_results(chunk.len());
            }
        }

        // FOLD: reassemble in original order
        let mut results = Vec::with_capacity(ids.len());
        for &(conn_idx, local_idx) in &result_map {
            results.push(conn_results[conn_idx][local_idx]);
        }
        results
    }

    /// Fetch + randomize + bulk update — /updates (FAN-OUT CANNON)
    pub fn update_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        let ids: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();
        let new_randoms: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();

        let results = self.fan_out_queries(&ids);

        let mut worlds: Vec<WorldRow> = results.into_iter().enumerate().map(|(i, (id, _))| {
            WorldRow { id, random_number: new_randoms[i] }
        }).collect();

        worlds.sort_by_key(|w| w.id);

        // Bulk UPDATE via primary connection
        let mut sql = String::with_capacity(64 + count * 16);
        sql.push_str("UPDATE world SET randomnumber = v.r FROM (VALUES ");
        for (i, w) in worlds.iter().enumerate() {
            if i > 0 { sql.push(','); }
            sql.push('(');
            sql.push_str(itoa::Buffer::new().format(w.id));
            sql.push(',');
            sql.push_str(itoa::Buffer::new().format(w.random_number));
            sql.push(')');
        }
        sql.push_str(") AS v(i, r) WHERE world.id = v.i");
        self.pg.execute_command(&sql);

        worlds
    }

    /// Fetch all fortunes — /fortunes (prepared statement)
    pub fn get_fortunes(&mut self) -> Vec<Fortune> {
        let rows = self.pg.query_fortunes();
        let mut fortunes: Vec<Fortune> = rows.into_iter().map(|(id, message)| {
            Fortune { id, message }
        }).collect();

        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        fortunes.sort_by(|a, b| a.message.cmp(&b.message));
        fortunes
    }
}

/// In-memory cache for all 10K world rows — /cached-queries
pub struct WorldCache {
    worlds: Vec<WorldRow>,
}

impl WorldCache {
    pub fn new(conn: &mut DbConn) -> Self {
        let rows = conn.pg.simple_query("SELECT id, randomnumber FROM world ORDER BY id");
        let worlds: Vec<WorldRow> = rows.into_iter().map(|row| {
            WorldRow {
                id: row[0].parse().unwrap(),
                random_number: row[1].parse().unwrap(),
            }
        }).collect();
        Self { worlds }
    }

    #[inline]
    pub fn get(&self, id: i32) -> &WorldRow {
        &self.worlds[(id - 1) as usize]
    }
}

/// Serialize a WorldRow to JSON bytes: {"id":N,"randomNumber":N}
pub fn world_to_json(w: &WorldRow, buf: &mut Vec<u8>) {
    let mut itoa_buf = itoa::Buffer::new();
    buf.extend_from_slice(b"{\"id\":");
    buf.extend_from_slice(itoa_buf.format(w.id).as_bytes());
    buf.extend_from_slice(b",\"randomNumber\":");
    buf.extend_from_slice(itoa_buf.format(w.random_number).as_bytes());
    buf.push(b'}');
}

/// Serialize a Vec<WorldRow> to JSON array.
pub fn worlds_to_json(worlds: &[WorldRow]) -> Vec<u8> {
    let mut buf = Vec::with_capacity(worlds.len() * 40);
    buf.push(b'[');
    for (i, w) in worlds.iter().enumerate() {
        if i > 0 { buf.push(b','); }
        world_to_json(w, &mut buf);
    }
    buf.push(b']');
    buf
}

/// Render fortunes as HTML with XSS escaping.
pub fn fortunes_to_html(fortunes: &[Fortune]) -> Vec<u8> {
    let mut html = Vec::with_capacity(2048);
    html.extend_from_slice(
        b"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
    );
    let mut itoa_buf = itoa::Buffer::new();
    for f in fortunes {
        html.extend_from_slice(b"<tr><td>");
        html.extend_from_slice(itoa_buf.format(f.id).as_bytes());
        html.extend_from_slice(b"</td><td>");
        html_escape(&f.message, &mut html);
        html.extend_from_slice(b"</td></tr>");
    }
    html.extend_from_slice(b"</table></body></html>");
    html
}

/// Zero-alloc fortune HTML into reusable buffer.
pub fn fortunes_to_html_into(fortunes: &[Fortune], buf: &mut Vec<u8>) {
    buf.extend_from_slice(
        b"<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
    );
    let mut itoa_buf = itoa::Buffer::new();
    for f in fortunes {
        buf.extend_from_slice(b"<tr><td>");
        buf.extend_from_slice(itoa_buf.format(f.id).as_bytes());
        buf.extend_from_slice(b"</td><td>");
        html_escape(&f.message, buf);
        buf.extend_from_slice(b"</td></tr>");
    }
    buf.extend_from_slice(b"</table></body></html>");
}

fn html_escape(s: &str, buf: &mut Vec<u8>) {
    for b in s.bytes() {
        match b {
            b'&' => buf.extend_from_slice(b"&amp;"),
            b'<' => buf.extend_from_slice(b"&lt;"),
            b'>' => buf.extend_from_slice(b"&gt;"),
            b'"' => buf.extend_from_slice(b"&quot;"),
            b'\'' => buf.extend_from_slice(b"&#x27;"),
            _ => buf.push(b),
        }
    }
}

/// Parse the "queries" or "count" query parameter, clamped to [1, 500].
pub fn parse_count_param(path: &str, param_name: &str) -> usize {
    if let Some(qmark) = path.find('?') {
        let qs = &path[qmark + 1..];
        for part in qs.split('&') {
            if let Some(eq) = part.find('=') {
                if &part[..eq] == param_name {
                    let val: i32 = part[eq + 1..].parse().unwrap_or(1);
                    return val.max(1).min(500) as usize;
                }
            }
        }
    }
    1 // default
}
