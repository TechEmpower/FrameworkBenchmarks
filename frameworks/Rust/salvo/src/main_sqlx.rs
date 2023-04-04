// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fmt::Write;
use std::sync::Arc;
use std::thread::available_parallelism;

use anyhow::Error;
use bytes::Bytes;
use once_cell::sync::OnceCell;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;
use sqlx::PgPool;
use dotenv::dotenv;

mod db_sqlx;
mod models_sqlx;
mod utils;

use db_sqlx::*;
use models_sqlx::*;

static DB_POOL: OnceCell<PgPool> = OnceCell::new();

static SERVER_HEADER: HeaderValue = HeaderValue::from_static("salvo");
static JSON_HEADER: HeaderValue = HeaderValue::from_static("application/json");
static HTML_HEADER: HeaderValue = HeaderValue::from_static("text/html; charset=utf-8");

fn pool() -> &'static PgPool {
    unsafe { DB_POOL.get_unchecked() }
}
#[handler]
async fn world_row(res: &mut Response) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);

    let conn = pool().acquire().await?;
    let world = fetch_world(conn, random_id).await?;

    let data = serde_json::to_vec(&world).unwrap();
    let headers = res.headers_mut();
    headers.insert(header::SERVER, SERVER_HEADER.clone());
    headers.insert(header::CONTENT_TYPE, JSON_HEADER.clone());
    res.set_body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response) -> Result<(), Error> {
    let conn = pool().acquire().await?;
    let mut items = fetch_fortunes(conn).await?;
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

    let db_url: String = utils::get_env_var("TECHEMPOWER_DATABASE_URL");
    let max_pool_size: u32 = utils::get_env_var("TECHEMPOWER_MAX_POOL_SIZE");
    let min_pool_size: u32 = utils::get_env_var("TECHEMPOWER_MIN_POOL_SIZE");
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    rt.block_on(async {
        DB_POOL
            .set(create_pool(db_url, max_pool_size, min_pool_size).await)
            .ok();
    });
    let router = Arc::new(
        Router::new()
            .push(Router::with_path("db").get(world_row))
            .push(Router::with_path("fortunes").get(fortunes)),
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
