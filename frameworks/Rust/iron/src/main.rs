extern crate iron;
extern crate persistent;
#[macro_use]
extern crate router;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate hyper;
extern crate mustache;
extern crate postgres;
extern crate r2d2;
extern crate r2d2_postgres;
extern crate rand;
extern crate rustc_serialize;

use hyper::header::{ContentType, Server};
use iron::modifiers::Header;
use iron::prelude::*;
use iron::status;
use iron::typemap::Key;
use persistent::Read;
use r2d2::Pool;
use r2d2_postgres::{PostgresConnectionManager, TlsMode};
use rand::distributions::{IndependentSample, Range};

#[derive(Serialize, Deserialize)]
struct Message {
    message: String,
}

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize, Clone)]
struct DatabaseRow {
    id: i32,
    randomNumber: i32,
}

struct CachedRows;
impl Key for CachedRows {
    type Value = Vec<DatabaseRow>;
}

pub type PostgresPool = Pool<PostgresConnectionManager>;

struct DbPool;
impl Key for DbPool {
    type Value = PostgresPool;
}

struct FortuneTemplate;
impl Key for FortuneTemplate {
    type Value = mustache::Template;
}

#[derive(RustcEncodable)]
struct FortuneRow {
    id: i32,
    message: String,
}

fn main() {
    let r2d2_config = r2d2::Config::default();
    let pg_conn_manager = PostgresConnectionManager::new(
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world",
        TlsMode::None,
    )
    .unwrap();
    let pool = r2d2::Pool::new(r2d2_config, pg_conn_manager).unwrap();
    let template = mustache::compile_str(
        "<!DOCTYPE html>
    <html> <head><title>Fortunes</title></head>
    <body> <table> 
    <tr><th>id</th><th>message</th></tr> 
    {{#.}} <tr><td>{{id}}</td><td>{{message}}</td></tr> 
    {{/.}} 
    </table> </body> </html>",
    )
    .unwrap();

    let mut cached_rows: Vec<DatabaseRow> = Vec::with_capacity(10000);
    let conn = pool.get().unwrap();

    for num in 1..10000 {
        let rows = &conn
            .query("SELECT id, randomnumber FROM World WHERE id = $1", &[&num])
            .unwrap();
        let row = rows.get(0);
        cached_rows.push(DatabaseRow {
            id: row.get(0),
            randomNumber: row.get(1),
        });
    }

    let app = router!(
        json: get "/json" => json_handler,
        single_db_query: get "/db" => single_db_query_handler,
        plaintext: get "/plaintext" => plaintext_handler,
        queries: get "/queries" => queries_handler,
        cachedworlds: get "/cached-worlds" => cached_queries_handler,
        fortune: get "/fortune" => fortune_handler,
        updates: get "/updates" => updates_handler
    );
    let mut middleware = Chain::new(app);
    middleware.link(Read::<DbPool>::both(pool));
    middleware.link(Read::<FortuneTemplate>::both(template));
    middleware.link(Read::<CachedRows>::both(cached_rows));

    println!("Starting server...");
    Iron::new(middleware).http("0.0.0.0:8080").unwrap();
}

fn json_handler(_: &mut Request) -> IronResult<Response> {
    let message: Message = Message {
        message: "Hello, World!".to_owned(),
    };
    let content_type = Header(ContentType::json());
    let server = Header(Server("Iron".to_owned()));
    Ok(Response::with((
        status::Ok,
        serde_json::to_string(&message).unwrap(),
        content_type,
        server,
    )))
}

fn plaintext_handler(_: &mut Request) -> IronResult<Response> {
    let server = Header(Server("Iron".to_owned()));
    Ok(Response::with((status::Ok, "Hello, World!", server)))
}

fn single_db_query_handler(req: &mut Request) -> IronResult<Response> {
    let content_type = Header(ContentType::json());
    let server = Header(Server("Iron".to_owned()));
    let pool = req.get::<Read<DbPool>>().unwrap();
    let conn = pool.get().unwrap();
    let row = random_row(conn);
    Ok(Response::with((
        status::Ok,
        serde_json::to_string(&row).unwrap(),
        server,
        content_type,
    )))
}

fn queries_handler(req: &mut Request) -> IronResult<Response> {
    let content_type = Header(ContentType::json());
    let server = Header(Server("Iron".to_owned()));
    let pool = req.get::<Read<DbPool>>().unwrap();
    let query = req.url.query().unwrap();
    let param = match get_param(query, "queries") {
        Some(n) => match n.parse::<usize>() {
            Ok(m) => match m {
                e @ 1...500 => e,
                e if e > 500 => 500,
                _ => 1,
            },
            _ => 1,
        },
        _ => 1,
    };
    let mut res: Vec<DatabaseRow> = Vec::with_capacity(param);
    for _ in 0..param {
        let conn = pool.get().unwrap();
        res.push(random_row(conn))
    }
    Ok(Response::with((
        status::Ok,
        serde_json::to_string(&res).unwrap(),
        server,
        content_type,
    )))
}

fn cached_queries_handler(req: &mut Request) -> IronResult<Response> {
    let content_type = Header(ContentType::json());
    let server = Header(Server("Iron".to_owned()));
    let cached_rows = req.get::<Read<CachedRows>>().unwrap().to_owned();
    let query = req.url.query().unwrap();
    let param = match get_param(query, "queries") {
        Some(n) => match n.parse::<usize>() {
            Ok(m) => match m {
                e @ 1...500 => e,
                e if e > 500 => 500,
                _ => 1,
            },
            _ => 1,
        },
        _ => 1,
    };

    let mut res: Vec<DatabaseRow> = Vec::with_capacity(param);
    for _ in 0..param {
        let mut rng = rand::thread_rng();
        let between = Range::new(1, 10000);
        let num = between.ind_sample(&mut rng);
        res.push(cached_rows[num].to_owned())
    }
    Ok(Response::with((
        status::Ok,
        serde_json::to_string(&res).unwrap(),
        server,
        content_type,
    )))
}

fn fortune_handler(req: &mut Request) -> IronResult<Response> {
    let content_type = Header(ContentType::html());
    let server = Header(Server("Iron".to_owned()));
    let template = req.get::<Read<FortuneTemplate>>().unwrap();
    let pool = req.get::<Read<DbPool>>().unwrap();
    let conn = pool.get().unwrap();
    let query_res = &conn.query("SELECT id, message FROM Fortune", &[]).unwrap();
    let query_res_iter = query_res.iter();
    let mut rows: Vec<FortuneRow> = query_res_iter
        .map(|row| FortuneRow {
            id: row.get(0),
            message: row.get(1),
        })
        .collect();
    rows.push(FortuneRow {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });
    rows.sort_by(|it, next| it.message.cmp(&next.message));
    let mut res = vec![];
    template.render(&mut res, &rows).unwrap();
    Ok(Response::with((status::Ok, res, server, content_type)))
}

fn updates_handler(req: &mut Request) -> IronResult<Response> {
    let mut rng = rand::thread_rng();
    let between = Range::new(1, 10000);
    let content_type = Header(ContentType::json());
    let server = Header(Server("Iron".to_owned()));
    let pool = req.get::<Read<DbPool>>().unwrap();
    let query = req.url.query().unwrap();
    let param = match get_param(query, "queries") {
        Some(n) => match n.parse::<usize>() {
            Ok(m) => match m {
                e @ 1...500 => e,
                e if e > 500 => 500,
                _ => 1,
            },
            _ => 1,
        },
        _ => 1,
    };
    let mut dbres: Vec<DatabaseRow> = Vec::with_capacity(param);
    for _ in 0..param {
        let conn = pool.get().unwrap();
        dbres.push(random_row(conn))
    }
    let conn = pool.get().unwrap();
    let trans = conn.transaction().unwrap();
    // Sorting guarantees no deadlocks between multiple concurrent threads
    dbres.sort_by_key(|it| it.id);
    let mut res: Vec<DatabaseRow> = Vec::with_capacity(param);
    for row in dbres {
        let num = between.ind_sample(&mut rng);
        trans
            .execute(
                "UPDATE World SET randomnumber = $1 WHERE id = $2",
                &[&num, &row.id],
            )
            .unwrap();
        res.push(DatabaseRow {
            id: row.id,
            randomNumber: num,
        })
    }
    trans.commit().unwrap();
    Ok(Response::with((
        status::Ok,
        serde_json::to_string(&res).unwrap(),
        server,
        content_type,
    )))
}

fn random_row(conn: r2d2::PooledConnection<PostgresConnectionManager>) -> DatabaseRow {
    let mut rng = rand::thread_rng();
    let between = Range::new(1, 10000);
    let num = between.ind_sample(&mut rng);
    let rows = &conn
        .query("SELECT id, randomnumber FROM World WHERE id = $1", &[&num])
        .unwrap();
    let row = rows.get(0);
    DatabaseRow {
        id: row.get(0),
        randomNumber: row.get(1),
    }
}

fn get_param<'a>(querystring: &'a str, param: &'a str) -> Option<&'a str> {
    let n = querystring
        .split("&")
        .find(|&it| !(it.find(param).is_none()));
    match n {
        Some(n) => n.split("=").nth(1),
        _ => n,
    }
}
