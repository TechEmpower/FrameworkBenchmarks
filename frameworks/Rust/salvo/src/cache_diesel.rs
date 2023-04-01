// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

use std::cmp;
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use anyhow::Error;
use diesel::prelude::*;

use diesel_async::{AsyncConnection, AsyncPgConnection, RunQueryDsl};
use moka::sync::Cache as MokaCache;
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

static CACHED_WORLDS: OnceCell<MokaCache<usize, World>> = OnceCell::new();

#[handler]
async fn cached_queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<usize>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let idx = rng.gen_range(0..10_000);
        unsafe {
            let w = CACHED_WORLDS.get_unchecked().get(&idx).unwrap();
            worlds.push(w);
        }
    }
    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("S"));
    res.render(Json(worlds));
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

async fn populate_cache() -> Result<(), Error> {
    let mut conn = AsyncPgConnection::establish(DB_URL).await?;
    let worlds = world::table
        .limit(10_000)
        .get_results::<World>(&mut conn)
        .await?;
    let cache = MokaCache::new(10_000);
    for (i, word) in worlds.into_iter().enumerate() {
        cache.insert(i, word);
    }
    CACHED_WORLDS.set(cache).unwrap();
    Ok(())
}

#[tokio::main]
async fn main() {
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("cached_queries").get(cached_queries)),
    );
    populate_cache().await.expect("error cache worlds");

    let addr = SocketAddr::from((Ipv4Addr::UNSPECIFIED, 8080));
    let acceptor = TcpListener::new(addr).bind().await;
    Server::new(acceptor).serve(router).await;
}
