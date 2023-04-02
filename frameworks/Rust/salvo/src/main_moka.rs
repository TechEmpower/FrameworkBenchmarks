// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::thread::available_parallelism;

use salvo::conn::tcp::TcpAcceptor;
use anyhow::Error;
use moka::sync::Cache as MokaCache;
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::http::header::{self, HeaderValue};
use salvo::prelude::*;

mod models;
mod utils;
use models::*;
mod pg_conn;
use pg_conn::PgConnection;

const DB_URL: &str = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
static CACHED_WORLDS: OnceCell<MokaCache<usize, World>> = OnceCell::new();

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

#[handler]
fn cached_queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count as usize);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let idx = rng.gen_range(0..10_000);
        unsafe {
            let w = CACHED_WORLDS.get_unchecked().get(&idx).unwrap();
            worlds.push(w);
        }
    }
    res.headers_mut()
        .insert(header::SERVER, HeaderValue::from_static("salvo"));
    res.render(Json(worlds));
    Ok(())
}

async fn populate_cache() -> Result<(), Error> {
    let conn = PgConnection::create(DB_URL).await?;
    let worlds = conn.get_worlds(10_000).await?;
    let cache = MokaCache::new(10_000);
    for (i, word) in worlds.into_iter().enumerate() {
        cache.insert(i, word);
    }
    CACHED_WORLDS.set(cache).unwrap();
    Ok(())
}

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        populate_cache().await.expect("error cache worlds");
    });

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    for _ in 1..available_parallelism().map(|n| n.get()).unwrap_or(16) {
        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(serve());
        });
    }
    println!("Started http server: 127.0.0.1:8080");
    rt.block_on(serve());
}

async fn serve() {
    let router = Router::with_path("cached_queries").get(cached_queries);

    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    Server::new(acceptor).serve(router).await
}
