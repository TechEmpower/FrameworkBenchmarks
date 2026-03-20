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

/// Database connection with cannon-style pipelined queries.
pub struct DbConn {
    pub pg: PgWire,
    pub rng: WyRand,
}

impl DbConn {
    pub fn new() -> Self {
        let host = std::env::var("DBHOST").unwrap_or_else(|_| "tfb-database".to_string());
        let port: u16 = std::env::var("PGPORT").unwrap_or_else(|_| "5432".to_string()).parse().unwrap();

        let pg = PgWire::connect(&host, port, "benchmarkdbuser", "benchmarkdbpass", "hello_world");

        Self {
            pg,
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

    /// Multiple queries — /queries (CANNON PIPELINED)
    ///
    /// Preloads all IDs, writes all queries in one syscall,
    /// reads all results in one syscall.
    pub fn get_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        // Kinetic energy: preload all IDs
        let ids: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();

        // Launch + gather
        let results = self.pg.query_worlds_pipelined(&ids);

        results.into_iter().map(|(id, rn)| {
            WorldRow { id, random_number: rn }
        }).collect()
    }

    /// Fetch + randomize + bulk update — /updates (CANNON PIPELINED)
    pub fn update_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        // Kinetic energy
        let ids: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();
        let new_randoms: Vec<i32> = (0..count).map(|_| self.rand_id()).collect();

        // Launch: pipeline all SELECTs
        let results = self.pg.query_worlds_pipelined(&ids);

        let mut worlds: Vec<WorldRow> = results.into_iter().enumerate().map(|(i, (id, _))| {
            WorldRow { id, random_number: new_randoms[i] }
        }).collect();

        // Sort by id for consistent UPDATE ordering
        worlds.sort_by_key(|w| w.id);

        // Bulk UPDATE
        let mut sql = String::from("UPDATE world SET randomnumber = v.r FROM (VALUES ");
        for (i, w) in worlds.iter().enumerate() {
            if i > 0 { sql.push(','); }
            sql.push('(');
            sql.push_str(&w.id.to_string());
            sql.push(',');
            sql.push_str(&w.random_number.to_string());
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
