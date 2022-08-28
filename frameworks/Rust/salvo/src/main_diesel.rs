#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

use std::cmp;
use std::fmt::Write;
use std::sync::Arc;
use std::thread::available_parallelism;

use anyhow::Error;
use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, Pool, PoolError, PooledConnection};
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;

mod models;
mod schema;
mod server;
use models::*;
use schema::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
type PgPool = Pool<ConnectionManager<PgConnection>>;

static DB_POOL: OnceCell<PgPool> = OnceCell::new();
static CACHED_WORLDS: OnceCell<Vec<World>> = OnceCell::new();

fn connect() -> Result<PooledConnection<ConnectionManager<PgConnection>>, PoolError> {
    unsafe { DB_POOL.get_unchecked().get() }
}
fn build_pool(database_url: &str, size: u32) -> Result<PgPool, PoolError> {
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    diesel::r2d2::Pool::builder()
        .max_size(size)
        .min_idle(Some(size))
        .test_on_check_out(false)
        .idle_timeout(None)
        .max_lifetime(None)
        .build(manager)
}

#[handler]
async fn world_row(res: &mut Response) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);
    let conn = connect()?;
    let row = world::table.find(random_id).first::<World>(&conn)?;
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(row));
    Ok(())
}

#[handler]
async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
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
    res.render(Json(worlds));
    Ok(())
}

#[handler]
async fn cached_queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let idx = rng.gen_range(0..10_000);
        unsafe {
            let w = CACHED_WORLDS.get_unchecked().get(idx).unwrap();
            worlds.push(w);
        }
    }
    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(worlds));
    Ok(())
}

#[handler]
async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
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
    res.render(Json(worlds));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response) -> Result<(), Error> {
    let conn = connect()?;
    let mut items = fortune::table.get_results::<Fortune>(&conn)?;
    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));
    let mut body = String::new();
    write!(&mut body, "{}", FortunesTemplate { items }).unwrap();

    res.headers_mut().insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Text::Html(body));
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

fn populate_cache() -> Result<(), Error> {
    let conn = connect()?;
    let worlds = world::table.limit(10_000).get_results::<World>(&conn)?;
    CACHED_WORLDS.set(worlds).unwrap();
    Ok(())
}

fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("db").get(world_row))
            .push(Router::with_path("fortunes").get(fortunes))
            .push(Router::with_path("queries").get(queries))
            .push(Router::with_path("cached_queries").get(cached_queries))
            .push(Router::with_path("updates").get(updates)),
    );
    let size = available_parallelism().map(|n| n.get()).unwrap_or(16);
    DB_POOL
        .set(build_pool(DB_URL, size as u32).unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL)))
        .ok();
    populate_cache().expect("error cache worlds");
    for _ in 1..size {
        let router = router.clone();
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve(router));
        });
    }
    println!("Starting http server: 127.0.0.1:8080");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(serve(router));
}

async fn serve(router: Arc<Router>) {
    server::builder().serve(Service::new(router)).await.unwrap();
}
