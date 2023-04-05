// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

use bytes::Bytes;
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
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;
use dotenv::dotenv;

mod models_diesel;
mod schema;
mod utils;
use models_diesel::*;
use schema::*;

type PgPool = Pool<ConnectionManager<PgConnection>>;

static DB_POOL: OnceCell<PgPool> = OnceCell::new();
static SERVER_HEADER: HeaderValue = HeaderValue::from_static("salvo");
static JSON_HEADER: HeaderValue = HeaderValue::from_static("application/json");
static HTML_HEADER: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");

fn connect() -> Result<PooledConnection<ConnectionManager<PgConnection>>, PoolError> {
    unsafe { DB_POOL.get_unchecked().get() }
}
fn create_pool(database_url: &str, size: u32) -> Result<PgPool, PoolError> {
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
    let mut conn = connect()?;
    let world = world::table.find(random_id).first::<World>(&mut conn)?;

    let data = serde_json::to_vec(&world).unwrap();
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count as usize);
    let mut rng = SmallRng::from_entropy();
    let mut conn = connect()?;
    for _ in 0..count {
        let id = rng.gen_range(1..10_001);
        let w = world::table.find(id).get_result::<World>(&mut conn)?;
        worlds.push(w);
    }

    let data = serde_json::to_vec(&worlds)?;
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut conn = connect()?;
    let mut worlds = Vec::with_capacity(count as usize);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let w_id: i32 = rng.gen_range(1..10_001);
        let mut w = world::table.find(w_id).first::<World>(&mut conn)?;
        w.randomnumber = rng.gen_range(1..10_001);
        worlds.push(w);
    }
    worlds.sort_by_key(|w| w.id);
    conn.transaction::<(), Error, _>(|conn| {
        for w in &worlds {
            diesel::update(world::table)
                .filter(world::id.eq(w.id))
                .set(world::randomnumber.eq(w.randomnumber))
                .execute(conn)?;
        }
        Ok(())
    })?;

    let data = serde_json::to_vec(&worlds)?;
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response) -> Result<(), Error> {
    let mut conn = connect()?;
    let mut items = fortune::table.get_results::<Fortune>(&mut conn)?;
    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));
    let mut data = String::new();
    write!(&mut data, "{}", FortunesTemplate { items }).unwrap();

    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, HTML_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
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

fn main() {
    dotenv().ok();
    
    let db_url: String = utils::get_env_var("TECHEMPOWER_POSTGRES_URL");
    let max_pool_size: u32 = utils::get_env_var("TECHEMPOWER_MAX_POOL_SIZE");
    DB_POOL
        .set(
            create_pool(&db_url, max_pool_size)
                .unwrap_or_else(|_| panic!("Error connecting to {}", &db_url)),
        )
        .ok();

    let router = Arc::new(
        Router::new()
            .push(Router::with_path("db").get(world_row))
            .push(Router::with_path("fortunes").get(fortunes))
            .push(Router::with_path("queries").get(queries))
            .push(Router::with_path("updates").get(updates)),
    );
    let thread_count = available_parallelism().map(|n| n.get()).unwrap_or(16);
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    for _ in 1..thread_count {
        let router = router.clone();
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve(router));
        });
    }
    println!("Started http server: 127.0.0.1:8080");
    rt.block_on(serve(router));
}

async fn serve(router: Arc<Router>) {
    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    Server::new(acceptor).serve(router).await
}
