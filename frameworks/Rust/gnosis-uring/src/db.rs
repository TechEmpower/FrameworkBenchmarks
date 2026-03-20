//! PostgreSQL database operations for TechEmpower benchmarks.
//!
//! Lazy connection — only initialized when a DB endpoint is first hit.
//! Each thread gets its own connection (no shared state, no pooling).
//! Uses sync postgres crate — lower overhead than async for blocking I/O model.

use postgres::{Client, NoTls, Statement};
use nanorand::{Rng, WyRand};

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

/// Database connection with prepared statements.
pub struct DbConn {
    client: Client,
    world_stmt: Statement,
    fortune_stmt: Statement,
    pub rng: WyRand,
}

impl DbConn {
    /// Connect to the TechEmpower database.
    pub fn new() -> Self {
        let host = std::env::var("DBHOST").unwrap_or_else(|_| "tfb-database".to_string());
        let port = std::env::var("PGPORT").unwrap_or_else(|_| "5432".to_string());
        let url = format!(
            "host={} port={} user=benchmarkdbuser password=benchmarkdbpass dbname=hello_world",
            host, port
        );
        let mut client = Client::connect(&url, NoTls).expect("DB connection failed");

        let world_stmt = client
            .prepare("SELECT id, randomnumber FROM world WHERE id=$1")
            .expect("prepare world");
        let fortune_stmt = client
            .prepare("SELECT id, message FROM fortune")
            .expect("prepare fortune");

        Self {
            client,
            world_stmt,
            fortune_stmt,
            rng: WyRand::new(),
        }
    }

    #[inline]
    pub fn rand_id(&mut self) -> i32 {
        (self.rng.generate_range(0_u32..10000) as i32) + 1
    }

    /// Single database query — /db
    pub fn get_world(&mut self, id: i32) -> WorldRow {
        let row = self.client.query_one(&self.world_stmt, &[&id]).unwrap();
        WorldRow {
            id: row.get(0),
            random_number: row.get(1),
        }
    }

    /// Multiple queries — /queries
    pub fn get_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        let mut worlds = Vec::with_capacity(count);
        for _ in 0..count {
            let id = self.rand_id();
            worlds.push(self.get_world(id));
        }
        worlds
    }

    /// Fetch + randomize + bulk update — /updates
    pub fn update_worlds(&mut self, count: usize) -> Vec<WorldRow> {
        let mut worlds = Vec::with_capacity(count);
        for _ in 0..count {
            let id = self.rand_id();
            let mut w = self.get_world(id);
            w.random_number = self.rand_id();
            worlds.push(w);
        }

        // Sort by id for consistent UPDATE ordering (TechEmpower requirement)
        worlds.sort_by_key(|w| w.id);

        // Bulk UPDATE using VALUES list
        if !worlds.is_empty() {
            let mut sql = String::from(
                "UPDATE world SET randomnumber = v.r FROM (VALUES "
            );
            for (i, w) in worlds.iter().enumerate() {
                if i > 0 { sql.push(','); }
                sql.push('(');
                sql.push_str(&w.id.to_string());
                sql.push(',');
                sql.push_str(&w.random_number.to_string());
                sql.push(')');
            }
            sql.push_str(") AS v(i, r) WHERE world.id = v.i");
            self.client.execute(sql.as_str(), &[]).unwrap();
        }

        worlds
    }

    /// Fetch all fortunes — /fortunes
    pub fn get_fortunes(&mut self) -> Vec<Fortune> {
        let rows = self.client.query(&self.fortune_stmt, &[]).unwrap();
        let mut fortunes: Vec<Fortune> = rows
            .iter()
            .map(|row| Fortune {
                id: row.get(0),
                message: row.get(1),
            })
            .collect();

        // Add the extra fortune
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        // Sort by message (not by id)
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
        let rows = conn.client
            .query("SELECT id, randomnumber FROM world ORDER BY id", &[])
            .unwrap();
        let worlds: Vec<WorldRow> = rows
            .iter()
            .map(|row| WorldRow {
                id: row.get(0),
                random_number: row.get(1),
            })
            .collect();
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

/// HTML-escape a string (handles &, <, >, ", ').
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
