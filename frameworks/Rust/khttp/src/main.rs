use khttp::{Headers, Method::*, RequestContext, ResponseHandle, Server, Status};
use std::{ffi::CStr, io, ptr};
use yarte::{Serialize, ywrite_html};

#[derive(Serialize)]
struct HelloMessage {
    message: &'static str,
}

fn main() {
    let mut app = Server::builder("0.0.0.0:8080").unwrap();

    app.route(Get, "/plaintext", |_ctx, res| {
        // headers
        let mut headers = Headers::new();
        headers.add(Headers::CONTENT_TYPE, b"text/plain");
        headers.add("server", b"khttp");

        // response
        res.ok(&headers, "Hello, World!")
    });

    app.route(Get, "/json", |_ctx, res| {
        // headers
        let mut headers = Headers::new();
        headers.add(Headers::CONTENT_TYPE, b"application/json");
        headers.add("server", b"khttp");

        // body
        let msg = HelloMessage {
            message: "Hello, World!",
        };
        let mut buf = Vec::with_capacity(32);
        msg.to_bytes_mut(&mut buf);

        // response
        res.ok(&headers, buf)
    });

    app.route(Get, "/fortunes", handle_fortunes);

    app.build().serve_epoll().unwrap();
}

// ---------------------------------------------------------------------
// GET /fortunes handler
// ---------------------------------------------------------------------

fn handle_fortunes(_ctx: RequestContext, res: &mut ResponseHandle) -> io::Result<()> {
    // headers
    let mut headers = Headers::new();
    headers.add(Headers::CONTENT_TYPE, b"text/html; charset=utf-8");
    headers.add("server", b"khttp");

    // response
    match fetch_fortunes_html() {
        Ok(body) => res.ok(&headers, body),
        Err(_) => res.send0(&Status::INTERNAL_SERVER_ERROR, &headers),
    }
}

// ---------------------------------------------------------------------
// /fortunes query implementation using postgres (libpq)
// ---------------------------------------------------------------------

use pq_sys::{
    ConnStatusType, ExecStatusType, PGconn, PQclear, PQconnectdb, PQerrorMessage, PQexecPrepared,
    PQfinish, PQgetlength, PQgetvalue, PQntuples, PQprepare, PQresultStatus, PQstatus,
};

const DB_CONNINFO: &CStr = c"postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
const PG_FORTUNES_SQL: &CStr = c"SELECT id, message FROM fortune";
const PG_FORTUNES_PREPARED_STMT: &CStr = c"s_fortunes";

#[derive(Serialize)]
struct Fortune<'a> {
    id: i32,
    message: &'a str,
}

fn fetch_fortunes_html() -> Result<Vec<u8>, String> {
    PG_CONN.with(|pg| unsafe {
        let res = PQexecPrepared(
            pg.conn,
            PG_FORTUNES_PREPARED_STMT.as_ptr(), // stmtName
            0,                                  // nParams
            ptr::null(),                        // paramValues
            ptr::null(),                        // paramLengths
            ptr::null(),                        // paramFormats
            1,                                  // resultFormat = 1 (binary)
        );
        if res.is_null() {
            return Err("PQexecPrepared returned null".to_owned());
        }
        if PQresultStatus(res) != ExecStatusType::PGRES_TUPLES_OK {
            PQclear(res);
            return Err("PQexecPrepared non-ok result status".to_owned());
        }

        let rows = PQntuples(res);
        let mut fortunes = Vec::with_capacity(rows as usize + 1);

        for i in 0..rows {
            // field 0: id (int)
            let id_ptr = PQgetvalue(res, i, 0) as *const i32;
            let id = i32::from_be(ptr::read_unaligned(id_ptr));

            // field 1: message (text)
            let msg_len = PQgetlength(res, i, 1) as usize;
            let msg_ptr = PQgetvalue(res, i, 1) as *const u8;
            let msg_slice = std::slice::from_raw_parts(msg_ptr, msg_len);
            let message = std::str::from_utf8_unchecked(msg_slice); // message fields are stored in utf8

            fortunes.push(Fortune { id, message });
        }

        // add extra fortune
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.",
        });

        // sort
        fortunes.sort_by(|a, b| a.message.cmp(b.message));

        // render html template
        let mut buf = Vec::with_capacity(2048);
        ywrite_html!(buf, "{{> fortunes }}");

        PQclear(res);
        Ok(buf)
    })
}

// TLS: connection per thread
thread_local! {
    static PG_CONN: PgConnection = PgConnection::new();
}

struct PgConnection {
    conn: *mut PGconn,
}

impl PgConnection {
    fn new() -> Self {
        unsafe {
            // connect
            let conn = PQconnectdb(DB_CONNINFO.as_ptr());
            if PQstatus(conn) != ConnStatusType::CONNECTION_OK {
                let err = get_pg_error_message(conn);
                PQfinish(conn);
                panic!("PQconnectdb failed: {err}");
            }

            // prepare fortunes statement
            let res = PQprepare(
                conn,
                PG_FORTUNES_PREPARED_STMT.as_ptr(),
                PG_FORTUNES_SQL.as_ptr(),
                0,
                ptr::null(),
            );
            if res.is_null() {
                PQfinish(conn);
                panic!("PQprepare returned null");
            }

            let st = PQresultStatus(res);
            PQclear(res);
            if st != ExecStatusType::PGRES_COMMAND_OK {
                let err = get_pg_error_message(conn);
                PQfinish(conn);
                panic!("prepare failed: {err}");
            }

            PgConnection { conn }
        }
    }
}

#[cold]
fn get_pg_error_message(conn: *mut PGconn) -> String {
    unsafe {
        CStr::from_ptr(PQerrorMessage(conn))
            .to_string_lossy()
            .into_owned()
    }
}
