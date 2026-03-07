//! TechEmpower benchmark application — all route handlers and DB logic.

use vortex_server::{App, RouteAction};
use vortex_server::http::date::DateCache;
use vortex_server::http::parser;
use vortex_server::http::response::{StaticResponse, DynJsonResponse, DynHtmlResponse};

const PLAINTEXT_CT: &[u8] = b"text/plain";
const PLAINTEXT_BODY: &[u8] = b"Hello, World!";
const JSON_MESSAGE: &str = "Hello, World!";
use vortex_server::db;
use vortex_server::db::wire;

const ROUTE_PLAINTEXT: u8 = 0;
const ROUTE_JSON: u8 = 1;
const ROUTE_DB: u8 = 2;
const ROUTE_QUERIES: u8 = 3;
const ROUTE_FORTUNES: u8 = 4;
const ROUTE_UPDATES: u8 = 5;

pub struct TfbApp;

/// Per-DB-connection state for tracking in-flight operations.
pub struct TfbDbState {
    op: u8,
    ids: Vec<i32>,
    random_numbers: Vec<i32>,
    worlds: Vec<(i32, i32)>,
    html: Vec<u8>,
    /// Pre-built Bind+Execute+Sync template for /db endpoint.
    db_template: Vec<u8>,
}

/// Offset of the 4-byte i32 param value within the /db wire template.
/// Bind: B(1) + len(4) + portal\0(1) + "w"\0(2) + fmt_count(2) + fmt(2) + param_count(2) + param_len(4) = 18
const DB_TEMPLATE_PARAM_OFFSET: usize = 18;

impl Default for TfbDbState {
    fn default() -> Self {
        // Pre-build the Bind+Execute+Sync template for /db (placeholder param=0)
        let mut db_template = Vec::with_capacity(64);
        wire::buf_bind_i32(&mut db_template, "w", &[0]);
        wire::buf_execute(&mut db_template);
        wire::buf_sync(&mut db_template);
        Self {
            op: 0,
            ids: Vec::with_capacity(500),
            random_numbers: Vec::with_capacity(500),
            worlds: Vec::with_capacity(500),
            html: Vec::with_capacity(4096),
            db_template,
        }
    }
}

impl App for TfbApp {
    type DbState = TfbDbState;

    fn classify(buf: &[u8]) -> RouteAction {
        match parser::classify_fast(buf) {
            parser::Route::Plaintext => RouteAction::Fast(ROUTE_PLAINTEXT),
            parser::Route::Json => RouteAction::Fast(ROUTE_JSON),
            parser::Route::Db => RouteAction::Db { id: ROUTE_DB, queries: 1 },
            parser::Route::Queries => RouteAction::Db {
                id: ROUTE_QUERIES,
                queries: db::clamp_queries(parser::parse_queries_param(buf)),
            },
            parser::Route::Fortunes => RouteAction::Db { id: ROUTE_FORTUNES, queries: 1 },
            parser::Route::Updates => RouteAction::Db {
                id: ROUTE_UPDATES,
                queries: db::clamp_queries(parser::parse_queries_param(buf)),
            },
            parser::Route::NotFound => RouteAction::NotFound,
        }
    }

    fn handle_fast(id: u8, recv: &[u8], send: &mut [u8], date: &DateCache) -> (usize, usize) {
        let count = parser::count_request_boundaries(recv);
        if count == 0 { return (0, 0); }

        let resp_len = match id {
            ROUTE_PLAINTEXT => StaticResponse::write(send, date, PLAINTEXT_CT, PLAINTEXT_BODY),
            ROUTE_JSON => {
                let mut body = [0u8; 64];
                let blen = vortex_server::json::write_message(&mut body, JSON_MESSAGE);
                DynJsonResponse::write(send, date, &body[..blen])
            }
            _ => return (0, 0),
        };

        // Duplicate response for pipelined requests
        let mut offset = resp_len;
        for _ in 1..count {
            send.copy_within(0..resp_len, offset);
            offset += resp_len;
        }
        (count, offset)
    }

    fn db_statements() -> Vec<(&'static str, &'static str, &'static [u32])> {
        vec![
            ("w", "SELECT id, randomNumber FROM World WHERE id = $1", &[23]),
            ("f", "SELECT id, message FROM Fortune", &[]),
            ("ub", "UPDATE world SET randomNumber = w.r FROM (SELECT unnest($1::int[]) AS i, unnest($2::int[]) AS r) AS w WHERE world.id = w.i", &[1007, 1007]),
        ]
    }

    fn db_start(id: u8, queries: i32, wbuf: &mut Vec<u8>, state: &mut TfbDbState) {
        state.op = id;
        match id {
            ROUTE_DB => {
                let rid = db::random_world_id();
                wbuf.extend_from_slice(&state.db_template);
                wbuf[DB_TEMPLATE_PARAM_OFFSET..DB_TEMPLATE_PARAM_OFFSET + 4]
                    .copy_from_slice(&rid.to_be_bytes());
            }
            ROUTE_QUERIES => {
                state.ids.clear();
                for _ in 0..queries { state.ids.push(db::random_world_id()); }
                for &id in &state.ids {
                    wire::buf_bind_i32(wbuf, "w", &[id]);
                    wire::buf_execute(wbuf);
                }
                wire::buf_sync(wbuf);
            }
            ROUTE_FORTUNES => {
                wire::buf_bind_no_params(wbuf, "f", &[1, 0]);
                wire::buf_execute(wbuf);
                wire::buf_sync(wbuf);
            }
            ROUTE_UPDATES => {
                state.ids.clear();
                state.random_numbers.clear();
                for _ in 0..queries { state.ids.push(db::random_world_id()); }
                state.ids.sort_unstable();
                for _ in 0..queries { state.random_numbers.push(db::random_world_id()); }
                // SELECT each world + batch UPDATE
                for &id in &state.ids {
                    wire::buf_bind_i32(wbuf, "w", &[id]);
                    wire::buf_execute(wbuf);
                }
                wire::buf_bind_i32_arrays(wbuf, "ub", &state.ids, &state.random_numbers);
                wire::buf_execute(wbuf);
                wire::buf_sync(wbuf);
            }
            _ => {}
        }
    }

    fn db_finish(
        state: &mut TfbDbState,
        rbuf: &[u8],
        rpos: usize,
        send: &mut [u8],
        date: &DateCache,
        body: &mut [u8],
    ) -> usize {
        match state.op {
            ROUTE_DB => {
                match wire::parse_single_world_buf(&rbuf[..rpos]) {
                    Some((id, rn)) => {
                        let blen = vortex_server::json::write_world(body, id, rn);
                        DynJsonResponse::write(send, date, &body[..blen])
                    }
                    None => 0,
                }
            }
            ROUTE_QUERIES => {
                state.worlds.clear();
                wire::parse_world_rows_buf(&rbuf[..rpos], &mut state.worlds);
                let blen = vortex_server::json::write_worlds(body, &state.worlds);
                DynJsonResponse::write(send, date, &body[..blen])
            }
            ROUTE_FORTUNES => {
                let mut fortunes = [(0i32, &b""[..]); 16];
                let count = wire::parse_fortune_rows_zerocopy(&rbuf[..rpos], &mut fortunes);
                vortex_server::template::render_fortunes_zerocopy(&fortunes, count, &mut state.html);
                DynHtmlResponse::write(send, date, &state.html)
            }
            ROUTE_UPDATES => {
                state.worlds.clear();
                for i in 0..state.ids.len() {
                    state.worlds.push((state.ids[i], state.random_numbers[i]));
                }
                let blen = vortex_server::json::write_worlds(body, &state.worlds);
                DynJsonResponse::write(send, date, &body[..blen])
            }
            _ => 0,
        }
    }
}
