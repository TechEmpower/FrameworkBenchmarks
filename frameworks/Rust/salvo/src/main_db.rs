#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

#[macro_use]
extern crate diesel;

use anyhow::Error;
use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, Pool, PoolError, PooledConnection};
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;
use std::cmp;
use std::fmt::Write;

mod models;
mod schema;
use models::*;
use schema::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
pub type PgPool = Pool<ConnectionManager<PgConnection>>;

pub static DB_POOL: OnceCell<PgPool> = OnceCell::new();

pub fn connect() -> Result<PooledConnection<ConnectionManager<PgConnection>>, PoolError> {
    DB_POOL.get().unwrap().get()
}
pub fn build_pool(database_url: &str) -> Result<PgPool, PoolError> {
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    diesel::r2d2::Pool::builder().max_size(50).build(manager)
}

#[fn_handler]
async fn world_row(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);
    let conn = connect()?;
    let row = world::table.find(random_id).first::<World>(&conn)?;
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&row);
    Ok(())
}

#[fn_handler]
async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.get_query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    let conn = connect()?;
    for _ in 0..count {
        let id = rng.gen_range(1..10_001);
        let w = world::table.find(id).get_result::<World>(&conn)?;
        worlds.push(w);
    }
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&worlds);
    Ok(())
}
#[fn_handler]
async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.get_query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let conn = connect()?;
    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let w_id: i32 = rng.gen_range(1..10_001);
        let mut w = world::table.find(w_id).first::<World>(&conn)?;
        w.randomnumber = rng.gen_range(1..10_001);
        worlds.push(w);
    }
    worlds.sort_by_key(|w| w.id);
    conn.transaction::<(), Error, _>(|| {
        for w in &worlds {
            diesel::update(world::table)
                .filter(world::id.eq(w.id))
                .set(world::randomnumber.eq(w.randomnumber))
                .execute(&conn)?;
        }
        Ok(())
    })?;

    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_json(&worlds);
    Ok(())
}

#[fn_handler]
async fn fortunes(_req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let mut items = vec![Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    }];

    let conn = connect()?;
    for item in fortune::table.get_results::<Fortune>(&conn)? {
        items.push(item);
    }
    items.sort_by(|it, next| it.message.cmp(&next.message));
    
    let mut body = String::new();
    write!(&mut body, "{}", FortunesTemplate { items }).unwrap();

    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render_html_text(&body);
    Ok(())
}
markup::define! {
    FortunesTemplate(items: Vec<Fortune>) {
        {markup::doctype()}
        html {
            head {
                title { "Fortunes" }
            }
            body {
                table {
                    tr { th { "id" } th { "message" } }
                    @for item in items {
                        tr {
                            td { {item.id} }
                            td { {markup::raw(v_htmlescape::escape(&item.message).to_string())} }
                        }
                    }
                }
            }
        }
    }
}

#[tokio::main]
async fn main() {
    println!("Starting http server: 127.0.0.1:8080");

    DB_POOL
        .set(build_pool(&DB_URL).expect(&format!("Error connecting to {}", &DB_URL)))
        .ok();

    let router = Router::new()
        .push(Router::new().path("db").get(world_row))
        .push(Router::new().path("fortunes").get(fortunes))
        .push(Router::new().path("queries").get(queries))
        .push(Router::new().path("updates").get(updates));
    Server::new(router).bind(([0, 0, 0, 0], 8080)).await;
}
