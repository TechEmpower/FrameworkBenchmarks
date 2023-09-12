// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::thread::available_parallelism;

use anyhow::Error;
use bytes::Bytes;
use dotenv::dotenv;
use lru::LruCache;
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;

mod models_pg;
mod utils;
use models_pg::*;
mod db_pg;
use db_pg::PgConnection;

static CACHED_WORLDS: OnceCell<LruCache<usize, World>> = OnceCell::new();

#[handler]
fn cached_queries(req: &mut Request, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));
    let mut worlds = Vec::with_capacity(count as usize);
    let mut rng = SmallRng::from_entropy();
    for _ in 0..count {
        let idx = rng.gen_range(0..10_000);
        unsafe {
            let w = CACHED_WORLDS.get_unchecked().peek(&idx).cloned().unwrap();
            worlds.push(w);
        }
    }
    let data = serde_json::to_vec(&worlds)?;
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    res.body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

async fn populate_cache() -> Result<(), Error> {
    let db_url: String = utils::get_env_var("TECHEMPOWER_POSTGRES_URL");
    let conn = PgConnection::create(&db_url).await?;
    let worlds = conn.get_worlds(10_000).await?;
    let mut cache = LruCache::new(NonZeroUsize::new(10_000).unwrap());
    for (i, word) in worlds.into_iter().enumerate() {
        cache.put(i, word);
    }
    CACHED_WORLDS.set(cache).unwrap();
    Ok(())
}

fn main() {
    dotenv().ok();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        populate_cache().await.expect("error cache worlds");
    });

    let router = Arc::new(Router::with_path("cached_queries").get(cached_queries));
    let thread_count = available_parallelism().map(|n| n.get()).unwrap_or(16);
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
