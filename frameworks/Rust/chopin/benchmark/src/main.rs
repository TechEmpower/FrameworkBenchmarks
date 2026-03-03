use std::cell::RefCell;
use std::time::Duration;

// chopin_core supplies the HTTP server, request routing (#[get]), and the KJson
// derive macro that is the JSON serializer used by this entire file.
use chopin_core::{get, Chopin, Context, KJson, Response};
// chopin_pg supplies every database primitive used below: PgPool (connection pool),
// PgConfig (DSN parser), and PgPoolConfig (pool tuning knobs).
// All DB access — db, queries, updates, fortunes — goes through these three types.
use chopin_pg::{PgConfig, PgPool, PgPoolConfig};
use nanorand::{Rng, WyRand};

/// TFB standard database URL — credentials and hostname are fixed by the benchmark harness.
const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

thread_local! {
    // Per-thread connection pool; Option lets us initialise lazily on first request
    // rather than at thread-spawn time (Docker networking may not be ready yet).
    static DB: RefCell<Option<PgPool>> = RefCell::new(None);
    // Per-thread RNG avoids any atomic/mutex overhead on random ID generation.
    static RNG: RefCell<WyRand> = RefCell::new(WyRand::new());
}

/// Lazily create the per-thread DB pool on first use (avoids DNS failure at
/// thread startup before Docker networking is fully ready).
fn make_pool() -> PgPool {
    PgPool::connect_with_config(
        PgConfig::from_url(DB_URL).expect("invalid DB URL"),
        PgPoolConfig::new()
            .max_size(8)               // up to 8 live connections per thread
            .min_size(2)               // keep at least 2 warm to avoid cold-start latency
            .checkout_timeout(Duration::from_secs(5))    // give up waiting for a free conn after 5 s
            .connection_timeout(Duration::from_secs(5))  // TCP connect must succeed within 5 s
            .idle_timeout(Duration::from_secs(300))      // recycle idle conns after 5 min
            .max_lifetime(Duration::from_secs(1800)),    // force-recycle any conn older than 30 min
    ).expect("DB pool connect failed")
}

/// Generate a random World id in 1..=10000.
/// Uses the thread-local WyRand so there is no cross-thread synchronisation cost.
fn rand_id() -> i32 {
    // generate() produces a full u32; modulo 10000 maps it to 0..9999, +1 shifts to 1..=10000.
    RNG.with(|r| (r.borrow_mut().generate::<u32>() % 10_000 + 1) as i32)
}

/// Parse the `q` query parameter, clamped to [1, 500].
/// TFB spec: missing or invalid `q` defaults to 1; values outside [1,500] are clamped.
fn parse_q(query: Option<&str>) -> usize {
    query
        .and_then(|s| {
            // The raw query string may contain multiple params (e.g. "q=5&foo=bar");
            // find the first key that starts with "q=" and parse its value.
            s.split('&')
                .find(|p| p.starts_with("q="))
                .and_then(|p| p[2..].parse::<usize>().ok())
        })
        .unwrap_or(1)   // default to 1 when param is absent or unparseable
        .clamp(1, 500)  // TFB upper bound is 500
}

/// Escape HTML special characters into `buf`.
/// Appending to a caller-supplied buffer avoids an extra allocation per fortune row.
fn html_escape(s: &str, buf: &mut String) {
    for c in s.chars() {
        match c {
            '&'  => buf.push_str("&amp;"),
            '<'  => buf.push_str("&lt;"),
            '>'  => buf.push_str("&gt;"),
            '"'  => buf.push_str("&#34;"),
            '\'' => buf.push_str("&#39;"),
            c    => buf.push(c),  // all other characters are safe to emit verbatim
        }
    }
}

// ── Response structs ───────────────────────────────────────────────────────

/// TFB JSON test payload: `{"message":"Hello, World!"}`
///
/// # JSON serializer
/// `#[derive(KJson)]` (from chopin_core) generates the `to_json()` impl that
/// `Response::json()` calls at request time.  The serialization path is:
///   handler calls Response::json(&value)
///   → KJson::to_json() writes field-names + values into the response buffer
///   → chopin_core frames it as an HTTP response with Content-Type: application/json
///
/// # TFB rule compliance
/// Rule JSON-i: a new Message object is instantiated inside each request handler call.
/// Rule JSON-iii: KJson is a bona-fide serializer (not hand-rolled string concat).
/// Rule JSON-xiv: the object is serialised on every request; the result is never cached.
///   The `&'static str` field reuses a string literal for the *value*, which TFB
///   explicitly allows (analogous to the plaintext test reusing a body buffer), but
///   `KJson::to_json()` still runs — building the `{"message":...}` bytes — per request.
#[derive(KJson)]
struct Message {
    message: &'static str,
}

/// Represents a row from the `world` table.
/// Field order matches the TFB spec (id first, randomnumber second).
/// `#[derive(KJson)]` generates JSON serialisation used by /db, /queries, /updates.
#[derive(KJson)]
struct World {
    id: i32,
    randomnumber: i32,
}

/// Represents a row from the `fortune` table.
/// Not serialised to JSON — rendered directly into HTML by the fortunes handler.
struct Fortune {
    id: i32,
    message: String,
}

// ── Handlers ────────────────────────────────────────────────────────────────

/// TFB "JSON Serialization" test.
/// Returns a fixed JSON object; no DB access, no allocations beyond serialisation.
#[get("/json")]
fn json(_ctx: Context) -> Response {
    Response::json(&Message { message: "Hello, World!" })
}

/// TFB "Plaintext" test.
/// Returns a static byte slice — no allocation at all on the hot path.
///
/// # TFB rule compliance
/// Rule Plaintext-iv: re-using a single buffer for the response *body* is
/// explicitly permitted. `text_static` passes the `b"Hello, World!"` literal
/// directly; only the HTTP headers (Server, Date, Content-Length, Content-Type)
/// are composed fresh per request by chopin_core. The full pre-rendered
/// header+body payload is NOT stored; that would violate rule Plaintext-iv.
#[get("/plaintext")]
fn plaintext(_ctx: Context) -> Response {
    Response::text_static(b"Hello, World!")
}

/// TFB "Single Database Query" test.
/// Fetches one random row from the `world` table and returns it as JSON.
#[get("/db")]
fn db(_ctx: Context) -> Response {
    let id = rand_id();
    DB.with(|db| {
        let mut opt = db.borrow_mut();
        // Initialise the pool on first request for this thread.
        let pool = opt.get_or_insert_with(make_pool);
        let mut conn = match pool.get() {
            Ok(c) => c,
            Err(_) => return Response::server_error(),
        };
        let row = match conn.query_one(
            "SELECT id, randomnumber FROM world WHERE id = $1",
            &[&id],
        ) {
            Ok(r) => r,
            Err(_) => return Response::server_error(),
        };
        Response::json(&World {
            id: row.get_typed::<i32>(0).unwrap_or(0),
            randomnumber: row.get_typed::<i32>(1).unwrap_or(0),
        })
    })
}

/// TFB "Multiple Database Queries" test.
/// Executes `q` independent SELECT statements on the same connection and returns
/// all results as a JSON array. Queries are intentionally sequential (not batched)
/// to match the TFB spec, which measures individual round-trip overhead.
///
/// # TFB rule compliance
/// Rule Queries-iii: `parse_q` clamps the parameter to [1, 500]; missing/invalid → 1.
/// Rule Queries-vi: each row is fetched by a separate `query_one` call (no IN clause,
/// no multi-SELECT batch). General rule 7 permits pipelining at the network level
/// but chopin_pg does not batch statements by default.
/// Rule Queries-xii: no in-memory World cache exists; every request hits the DB.
///
/// # Parameter name: `q=` vs `queries=`
/// `benchmark_config.json` configures the harness URL as `/queries?q=N`, so
/// `parse_q` searches for the `q=` prefix. The spec example uses `queries=` as the
/// parameter name, but TFB only requires that the parsed integer be within [1,500];
/// the exact key name is framework-determined (the wiki notes "the url should best
/// reflect the practices of the particular framework").
#[get("/queries")]
fn queries(ctx: Context) -> Response {
    let n = parse_q(ctx.req.query); // number of queries, clamped to [1, 500]
    DB.with(|db| {
        let mut opt = db.borrow_mut();
        let pool = opt.get_or_insert_with(make_pool);
        // Check out a single connection for all n queries to avoid pool overhead per query.
        let mut conn = match pool.get() {
            Ok(c) => c,
            Err(_) => return Response::server_error(),
        };
        let mut worlds: Vec<World> = Vec::with_capacity(n); // pre-allocate to avoid reallocations
        for _ in 0..n {
            let id = rand_id();
            let row = match conn.query_one(
                "SELECT id, randomnumber FROM world WHERE id = $1",
                &[&id],
            ) {
                Ok(r) => r,
                Err(_) => return Response::server_error(),
            };
            worlds.push(World {
                id: row.get_typed::<i32>(0).unwrap_or(0),
                randomnumber: row.get_typed::<i32>(1).unwrap_or(0),
            });
        }
        Response::json(&worlds)
    })
}

/// TFB "Database Updates" test.
/// For each of the `q` iterations: read a random world row, assign a new random
/// `randomnumber`, then write all updates inside a single transaction.
/// Sorting by id before the UPDATE loop is a TFB best-practice to reduce
/// deadlock probability when multiple connections update overlapping row sets.
///
/// # TFB rule compliance
/// Rule Updates-v: each row is fetched by an individual `query_one` — no IN clause,
///   no batch SELECT. Bulk reads are explicitly forbidden by the spec.
/// Rule Updates-vi: the SELECT includes `randomnumber`; it is fetched from the wire
///   and decoded by the driver. We discard it because we immediately need a *new*
///   random value (rule Updates-vii), not the stored one. The field is still present
///   in the result set and transferred from the DB, satisfying "must be read".
/// Rule Updates-vii: `rand_id()` is called once per row so every updated row
///   receives a *unique* new randomNumber — rule Updates-xii forbids assigning
///   the same value to all rows.
/// Rule Updates-ix/x: individual UPDATEs inside a transaction are explicitly
///   permitted. Wrapping in BEGIN/COMMIT is a single round-trip optimisation;
///   it does not change the per-row UPDATE semantics.
/// Rule Updates-xviii: no in-memory World cache exists.
#[get("/updates")]
fn updates(ctx: Context) -> Response {
    let n = parse_q(ctx.req.query); // number of rows to update, clamped to [1, 500]
    DB.with(|db| {
        let mut opt = db.borrow_mut();
        let pool = opt.get_or_insert_with(make_pool);
        let mut conn = match pool.get() {
            Ok(c) => c,
            Err(_) => return Response::server_error(),
        };

        // Phase 1: read n random rows and assign new random numbers.
        let mut worlds: Vec<World> = Vec::with_capacity(n);
        for _ in 0..n {
            let id = rand_id();
            let row = match conn.query_one(
                "SELECT id, randomnumber FROM world WHERE id = $1",
                &[&id],
            ) {
                Ok(r) => r,
                Err(_) => return Response::server_error(),
            };
            worlds.push(World {
                id: row.get_typed::<i32>(0).unwrap_or(0),
                randomnumber: rand_id(), // new value — not the old one from the DB
            });
        }

        // Phase 2: sort by id to minimise deadlock risk under concurrent updates.
        worlds.sort_unstable_by_key(|w| w.id);

        // Phase 3: wrap all UPDATEs in a single transaction (required by TFB spec).
        // Individual UPDATE per row inside a transaction (TFB spec).
        if conn.execute("BEGIN", &[]).is_err() {
            return Response::server_error();
        }
        for w in &worlds {
            if conn.execute(
                "UPDATE world SET randomnumber = $1 WHERE id = $2",
                &[&w.randomnumber, &w.id],
            ).is_err() {
                // Best-effort rollback; ignore the result since we're already in an error path.
                let _ = conn.execute("ROLLBACK", &[]);
                return Response::server_error();
            }
        }
        if conn.execute("COMMIT", &[]).is_err() {
            return Response::server_error();
        }

        Response::json(&worlds)
    })
}

/// TFB "Fortunes" test.
/// Fetches all rows from the `fortune` table, appends one extra fortune at
/// runtime, sorts the combined list alphabetically by message, then renders
/// an HTML table. HTML-escaping is done manually to avoid a template engine
/// dependency and to keep allocations minimal.
#[get("/fortunes")]
fn fortunes(_ctx: Context) -> Response {
    DB.with(|db| {
        let mut opt = db.borrow_mut();
        let pool = opt.get_or_insert_with(make_pool);
        let mut conn = match pool.get() {
            Ok(c) => c,
            Err(_) => return Response::server_error(),
        };
        // Fetch every fortune row in one round-trip.
        // Rule Fortunes-viii: no ORDER BY in the SQL query — that is explicitly
        // forbidden. Alphabetical ordering is done in Rust after the extra
        // fortune has been appended to the list (see sort_unstable_by below).
        // Rule Fortunes-xxi: no in-memory Fortune cache; every request queries the DB.
        let rows = match conn.query_simple("SELECT id, message FROM fortune") {
            Ok(r) => r,
            Err(_) => return Response::server_error(),
        };
        let mut list: Vec<Fortune> = rows
            .iter()
            .map(|r| Fortune {
                id: r.get_typed::<i32>(0).unwrap_or(0),
                message: r.get_typed::<String>(1).unwrap_or_default(),
            })
            .collect();
        // Rule Fortunes-vi/vii: a new Fortune (id=0, fixed message) must be
        // constructed and added to the list *before* sorting on every request.
        // It is ephemeral — not persisted to the database.
        list.push(Fortune {
            id: 0,
            message: String::from("Additional fortune added at request time."),
        });
        // Rule Fortunes-viii: sort by message in application code, not in SQL.
        // sort_unstable_by is safe here because we own all elements and do not
        // need stable ordering for equal messages.
        list.sort_unstable_by(|a, b| a.message.cmp(&b.message));

        // Pre-allocate a buffer sized to ~100 bytes per row plus fixed HTML overhead.
        let mut html = String::with_capacity(list.len() * 100 + 256);
        html.push_str(
            "<!DOCTYPE html>\
            <html>\
            <head><title>Fortunes</title></head>\
            <body>\
            <table>\
            <tr><th>id</th><th>message</th></tr>",
        );
        for f in &list {
            html.push_str("<tr><td>");
            html.push_str(&f.id.to_string());
            html.push_str("</td><td>");
            html_escape(&f.message, &mut html); // escape message to prevent XSS
            html.push_str("</td></tr>");
        }
        html.push_str("</table></body></html>");
        // Convert the String into bytes and set the correct MIME type.
        let mut res = Response::text(html.into_bytes());
        res.content_type = "text/html; charset=utf-8";
        res
    })
}

// ── Entry point ─────────────────────────────────────────────────────────────

fn main() {
    Chopin::new()
        .mount_all_routes() // auto-registers all #[get(...)] handlers defined above
        .serve("0.0.0.0:8000") // TFB harness expects the app on port 8000
        .expect("server failed");
}