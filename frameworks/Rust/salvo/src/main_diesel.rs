// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

use std::cmp;
use std::fmt::Write;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use anyhow::Error;
use diesel::prelude::*;

use futures_util::stream::{FuturesUnordered, TryStreamExt};
use diesel_async::pooled_connection::deadpool::Pool;
use diesel_async::pooled_connection::AsyncDieselConnectionManager;
use diesel_async::{AsyncPgConnection, RunQueryDsl};
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;

mod models;
mod schema;
use models::*;
use schema::*;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
type PgPool = Pool<AsyncPgConnection>;

static DB_POOL: OnceCell<PgPool> = OnceCell::new();

fn pool() -> &'static PgPool {
    unsafe { DB_POOL.get_unchecked() }
}
fn build_pool(database_url: &str, size: usize) -> PgPool {
    let config = AsyncDieselConnectionManager::<AsyncPgConnection>::new(database_url);
    Pool::builder(config)
        .max_size(size)
        .wait_timeout(None)
        .build()
        .unwrap_or_else(|_| panic!("Error connecting to {}", &DB_URL))
}

#[handler]
async fn world_row(res: &mut Response) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);
    let mut conn = pool().get().await?;
    let row = world::table
        .find(random_id)
        .first::<World>(&mut conn)
        .await?;
    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(row));
    Ok(())
}

#[handler]
async fn queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let worlds = {
        let mut rng = SmallRng::from_entropy();
        let mut conn = pool().get().await?;
        (0..count)
            .map(|_| {
                let id = rng.gen_range(1..10_001);
                world::table.find(id).first::<World>(&mut conn)
            })
            .collect::<FuturesUnordered<_>>()
    }.try_collect::<Vec<_>>().await?;
    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(worlds));
    Ok(())
}

#[handler]
async fn updates(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut conn = pool().get().await?;
    let mut worlds = {
        let mut rng = SmallRng::from_entropy();
        (0..count)
            .map(|_| {
                let w_id: i32 = rng.gen_range(1..10_001);
                let new_id = rng.gen_range(1..10_001);
                let fut = world::table.find(w_id).first::<World>(&mut conn);
                async move {
                    let mut w = fut.await?;
                    w.randomnumber = new_id;
                    Result::<_, Error>::Ok(w)
                }
            })
            .collect::<FuturesUnordered<_>>()
    }
    .try_collect::<Vec<_>>()
    .await?;
    worlds.sort_by_key(|w| w.id);
    conn.build_transaction()
        .run(|conn| {
            Box::pin(async {
                for w in &worlds {
                    diesel::update(world::table.find(w.id))
                        .set(world::randomnumber.eq(w.randomnumber))
                        .execute(conn)
                        .await?;
                }
                Ok::<(), Error>(())
            })
        })
        .await?;

    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(worlds));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response) -> Result<(), Error> {
    let mut conn = pool().get().await?;
    let mut items = fortune::table.get_results::<Fortune>(&mut conn).await?;
    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));
    let mut body = String::new();
    write!(&mut body, "{}", FortunesTemplate { items }).unwrap();

    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("salvo"));
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

#[tokio::main]
async fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("db").get(world_row))
            .push(Router::with_path("fortunes").get(fortunes))
            .push(Router::with_path("queries").get(queries))
            .push(Router::with_path("updates").get(updates)),
    );
    DB_POOL.set(build_pool(DB_URL, 5)).ok();

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let acceptor = TcpListener::new(addr).bind().await;
    Server::new(acceptor).serve(router).await;
}
