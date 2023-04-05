// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::fmt::Write;
use std::sync::Arc;
use std::thread::available_parallelism;

use anyhow::Error;
use bytes::Bytes;
use deadpool_postgres::Pool;
use futures_util::{stream::FuturesUnordered, TryStreamExt};
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;
use dotenv::dotenv;

mod db_pg_pool;
mod models_pg_pool;
mod utils;

use db_pg_pool::*;
use models_pg_pool::*;

static DB_POOL: OnceCell<Pool> = OnceCell::new();

static SERVER_HEADER: HeaderValue = HeaderValue::from_static("salvo");
static JSON_HEADER: HeaderValue = HeaderValue::from_static("application/json");
static HTML_HEADER: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");

fn pool() -> &'static Pool {
    unsafe { DB_POOL.get_unchecked() }
}
#[handler]
async fn world_row(res: &mut Response) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);

    let client = pool().get().await?;
    let select = prepare_fetch_world_by_id_statement(&client).await;
    let world = fetch_world_by_id(&client, random_id, &select).await?;

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

    let mut rng = SmallRng::from_entropy();
    let client = pool().get().await?;
    let select = prepare_fetch_world_by_id_statement(&client).await;
    let future_worlds = FuturesUnordered::new();

    for _ in 0..count {
        let w_id = (rng.gen::<u32>() % 10_000 + 1) as i32;
        future_worlds.push(fetch_world_by_id(&client, w_id, &select));
    }
    let worlds: Vec<World> = future_worlds.try_collect().await?;

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

    let mut rng = SmallRng::from_entropy();
    let client = pool().get().await?;
    let select = prepare_fetch_world_by_id_statement(&client).await;

    let future_worlds = FuturesUnordered::new();

    for _ in 0..count {
        let query_id = rng.gen_range(1..10_001);
        future_worlds.push(fetch_world_by_id(&client, query_id, &select));
    }

    let worlds: Vec<World> = future_worlds.try_collect().await?;
    let update = prepare_update_world_by_id_statement(&client).await;

    let future_world_updates = FuturesUnordered::new();
    for w in &worlds {
        let random_id = rng.gen_range(1..10_001);
        let w_id = w.id;
        future_world_updates.push(update_world(&client, &update, random_id, w_id));
    }
    let _world_updates: Vec<u64> = future_world_updates.try_collect().await?;

    let data = serde_json::to_vec(&worlds)?;
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response) -> Result<(), Error> {
    let client = pool().get().await?;
    let select = prepare_fetch_all_fortunes_statement(&client).await;
    let mut items = fetch_all_fortunes(client, &select).await?;
    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });
    items.sort_by(|a, b| a.message.cmp(&b.message));

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
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        DB_POOL.set(create_pool(db_url, max_pool_size).await).ok();
    });

    let router = Arc::new(
        Router::new()
            .push(Router::with_path("db").get(world_row))
            .push(Router::with_path("fortunes").get(fortunes))
            .push(Router::with_path("queries").get(queries))
            .push(Router::with_path("updates").get(updates)),
    );
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
