/*
 * Implementation for Rust/Iron for http://www.techempower.com/benchmarks/#section=code
 */

extern crate iron;
extern crate router;
extern crate serialize;
extern crate mysql;
extern crate urlencoded;
extern crate persistent;
extern crate typemap;
extern crate time;
extern crate htmlescape;

use std::io::net::ip::Ipv4Addr;
use iron::{Iron, AfterMiddleware, Request, Response, IronResult, Plugin, Chain, ChainBuilder, Set, status};
use iron::response::modifiers::{Status, Body, ContentType};

use router::Router;
use serialize::json;
use urlencoded::UrlEncodedQuery;
use std::default::Default;
use std::cmp::{min, max};
use persistent::Write;
use typemap::Assoc;
use iron::headers::content_type::MediaType;

use mysql::conn::{MyOpts};
use mysql::conn::pool::{MyPool};
use mysql::value::{from_value};

use htmlescape::encode_minimal;

use std::rand;
use std::rand::Rng;

// MySQL pool
struct Pool;
impl Assoc<mysql::conn::pool::MyPool> for Pool {}


// Server headers
struct ServerName;
impl Assoc<String> for ServerName {}

pub struct Utf8ContentType(pub ContentType);

impl Utf8ContentType {
    /// Create a new ContentType that is hard coded to return charset UTF-8
    #[inline]
    pub fn new<S: StrAllocating, S1: StrAllocating>(type_: S, subtype: S1) -> ContentType {
        ContentType(MediaType::new(type_.into_string(), subtype.into_string(), vec![(String::from_str("charset"), String::from_str("UTF-8"))]))
    }
}

// Handler responsible for adding Server and Date header
impl AfterMiddleware for ServerName {
    fn after(&self, _: &mut Request, res: &mut Response) -> IronResult<()> {
        res.headers.server = Some(String::from_str("Rust/Iron"));
        res.headers.date = Some(time::now_utc());
        Ok(())
    }
}
// Handler for /plaintext
fn plaintext_handler(_: &mut Request) -> IronResult<Response> {
    Ok(Response::new().set(Status(status::Ok)).set(Body("Hello, world!")))
}

// Automatically generate `Encodable` trait implementations
#[deriving(Encodable)]
pub struct HelloStruct  {
    message: String
}

// Handler for /json
fn json_handler(_: &mut Request) -> IronResult<Response> {
    let object = HelloStruct {
        message: "Hello, World!".to_string()
    };
    let encoded = json::encode(&object);

    Ok(Response::new().set(Status(status::Ok)).set(Body(encoded)))
}

// Automatically generate `Encodable` trait implementations
#[deriving(Encodable)]
#[allow(non_snake_case)]
pub struct WorldStruct  {
    id: int,
    randomNumber: int
}

// Handler for /db
#[allow(unused_must_use)]
fn db_handler(req: &mut Request) -> IronResult<Response> {
    let mutex = req.get::<Write<Pool, mysql::conn::pool::MyPool>>().unwrap();
    let pool = mutex.lock();
    let mut rng = rand::task_rng();
    let mut queries: Option<int> = None;
    // Extract the decoded data as hashmap, using the UrlEncodedQuery plugin.
    match req.get_ref::<UrlEncodedQuery>() {
        Some(hashmap) => {
            for (k, v) in hashmap.iter() {
                if k == &"queries".to_string() {
                    queries = from_str(v[0].as_slice());
                }
            }
        },
        None => queries = Some(1)
    }
    let nqueries;
    match queries {
        Some(number) => nqueries = max(1, min(number, 500i)),
        None         => nqueries = 1
    }

    // Stor result in a vector that will become a json list
    let mut vec = Vec::new();
    // Get our row id
    for _ in range(0i, nqueries) {
        let rndid = rng.gen_range(1i, 10000);
        pool.prepare("SELECT * FROM World WHERE id = ?;")
        .and_then(|mut stmt| {
            for row in &mut stmt.execute(&[&rndid]) {
                let row = row.unwrap();
                let world = WorldStruct {
                    id: from_value(&row[0]),
                    randomNumber: from_value(&row[1])
                };
                vec.push(world);
            }
            Ok(())
        });
    }
    Ok(Response::new().set(ContentType::new("application", "json")).set(Status(status::Ok)).set(Body(json::encode(&vec))))
}

// Automatically generate `Encodable` trait implementations
#[deriving(Encodable)]
pub struct FortuneStruct  {
    id: int,
    message: String
}

// Handler for /fortunes
#[allow(unused_must_use)]
fn fortunes_handler(req: &mut Request) -> IronResult<Response> {
    let mutex = req.get::<Write<Pool, mysql::conn::pool::MyPool>>().unwrap();
    let pool = mutex.lock();
    let mut vec = Vec::new();
    pool.prepare("SELECT * FROM fortune;")
    .and_then(|mut stmt| {
        for row in &mut stmt.execute([]) {
            let row = row.unwrap();
            let fortune = FortuneStruct {
                id: from_value(&row[0]),
                message: from_value(&row[1])
            };
            vec.push(fortune);
        }
        Ok(())
    });
    // Add the new "dynamic" fortune
    vec.push(FortuneStruct {
        id: 0,
        message: "Additional fortune added at request time.".to_string()
    });
    // Sort by message
    vec.sort_by(|a, b| a.message.cmp(&b.message));

    let mut rows = "".to_string();
    for f in vec.iter() {
        rows.push_str(format!("<tr><td>{}</td><td>{}</td></tr>", &f.id, encode_minimal(f.message.as_slice())).as_slice());
    }
    let data = format!("<!DOCTYPE HTML>
       <html>
       <head>
           <title>Fortunes</title>
        </head>
        <body>
            <table><tr><th>id</th><th>message</th></tr>
            {}
            </table>
        </body></html>", rows);
    Ok(Response::new().set(Utf8ContentType::new("text", "html")).set(Status(status::Ok)).set(Body(data)))

}

//Handler for /updates
#[allow(unused_must_use)]
fn updates_handler(req: &mut Request) -> IronResult<Response> {
    let mutex = req.get::<Write<Pool, mysql::conn::pool::MyPool>>().unwrap();
    let pool = mutex.lock();
    let mut rng = rand::task_rng();
    let mut queries: Option<int> = None;
    // Extract the decoded data as hashmap, using the UrlEncodedQuery plugin.
    match req.get_ref::<UrlEncodedQuery>() {
        Some(hashmap) => {
            for (k, v) in hashmap.iter() {
                if k == &"queries".to_string() {
                    queries = from_str(v[0].as_slice());
                }
            }
        },
        None => queries = Some(1)
    }
    let nqueries;
    match queries {
        Some(number) => nqueries = max(1, min(number, 500i)),
        None         => nqueries = 1
    }

    // Store result in a vector that will become a json list
    let mut vec = Vec::new();
    for _ in range(0i, nqueries) {
        // Get our row id
        let rndid = rng.gen_range(1i, 10000);
        pool.prepare("SELECT * FROM World WHERE id = ?;")
        .and_then(|mut stmt| {
            for row in &mut stmt.execute(&[&rndid]) {
                let row = row.unwrap();
                let mut world = WorldStruct {
                    id: from_value(&row[0]),
                    randomNumber: from_value(&row[1])
                };
                // Update random number to new value
                let rnd_number = rng.gen_range(1i, 10000);
                world.randomNumber = rnd_number;
                vec.push(world);
            }
            Ok(())
        });
    }
    // Persist the random numbers to database
    for w in vec.iter() {
        pool.prepare("UPDATE World SET randomNumber = ? WHERE id = ?;")
        .and_then(|mut stmt| {
            stmt.execute(&[&w.randomNumber, &w.id]).and(Ok(()))
        });
    }
    Ok(Response::new().set(ContentType::new("application", "json")).set(Status(status::Ok)).set(Body(json::encode(&vec))))
}

fn main() {

    // MySQL connection options, TODO fix configuration file?
    let opts = MyOpts{
        user: Some("benchmarkdbuser".to_string()),
        pass: Some("benchmarkdbpass".to_string()),
        tcp_addr: Some("127.0.0.1".to_string()),
        db_name: Some("hello_world".to_string()), ..Default::default()};

    let mut router = Router::new();
    router.get("/plaintext", plaintext_handler);
    router.get("/json", json_handler);
    router.get("/db", db_handler);
    router.get("/fortunes", fortunes_handler);
    router.get("/updates", updates_handler);

    // Our main chain
    let mut main_chain = ChainBuilder::new(router);
    // Make MySQL pool available to all the handlers
    main_chain.link(Write::<Pool, mysql::conn::pool::MyPool>::both(MyPool::new(opts.clone()).unwrap()));
    // Add the after middle that will add servername and date to every request
    main_chain.link_after(ServerName);

    Iron::new(main_chain).listen(Ipv4Addr(0, 0, 0, 0), 8080);
    println!("Listening on 0.0.0.0 port 8080");
}
