// #[global_allocator]
// static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;
// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::cmp;
use std::fmt::Write;
use std::thread::available_parallelism;
use std::time::Duration;

use anyhow::Error;
use bytes::Bytes;
use dotenv::dotenv;
use mongodb::{
    options::{ClientOptions, Compressor},
    Client, Database,
};
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};
use salvo::conn::tcp::TcpAcceptor;
use salvo::http::header::{self, HeaderValue};
use salvo::http::ResBody;
use salvo::prelude::*;

mod db_mongo;
mod models_mongo;
mod utils;

use db_mongo::*;
use models_mongo::*;

#[handler]
async fn world_row(res: &mut Response, depot: &mut Depot) -> Result<(), Error> {
    let mut rng = SmallRng::from_entropy();
    let random_id = rng.gen_range(1..10_001);

    let db = depot.obtain::<Database>().unwrap();
    let world = find_world_by_id(db.clone(), random_id).await?;

    let data = serde_json::to_vec(&world).unwrap();
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    res.body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn queries(req: &mut Request, depot: &mut Depot, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let mut rng = SmallRng::from_entropy();
    let mut ids: Vec<i32> = Vec::with_capacity(count as usize);
    for _ in 0..count {
        ids.push(rng.gen_range(1..10_001));
    }
    let db = depot.obtain::<Database>().unwrap();
    let worlds = find_worlds(db.clone(), ids).await?;

    let data = serde_json::to_vec(&worlds)?;
    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    res.body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn updates(req: &mut Request, depot: &mut Depot, res: &mut Response) -> Result<(), Error> {
    let count = req.query::<u16>("q").unwrap_or(1);
    let count = cmp::min(500, cmp::max(1, count));

    let mut rng = SmallRng::from_entropy();
    let mut ids: Vec<i32> = Vec::with_capacity(count as usize);
    for _ in 0..count {
        ids.push(rng.gen_range(1..10_001));
    }

    let db = depot.obtain::<Database>().unwrap();
    let mut worlds = find_worlds(db.clone(), ids).await?;
    for world in &mut worlds {
        world.random_number = rng.gen_range(1..10_001);
    }
    let data = serde_json::to_vec(&worlds)?;
    update_worlds(db.clone(), worlds).await?;

    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("application/json"));
    res.body(ResBody::Once(Bytes::from(data)));
    Ok(())
}

#[handler]
async fn fortunes(res: &mut Response, depot: &mut Depot) -> Result<(), Error> {
    let db = depot.obtain::<Database>().unwrap();
    let items = fetch_fortunes(db.clone()).await?;

    let mut data = String::new();
    write!(&mut data, "{}", FortunesTemplate { items })?;

    let headers = res.headers_mut();
    headers.insert(header::SERVER, HeaderValue::from_static("salvo"));
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/html; charset=utf-8"),
    );
    res.body(ResBody::Once(Bytes::from(data)));
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

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let thread_count = available_parallelism().map(|n| n.get()).unwrap_or(16);
    for _ in 1..thread_count {
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
    let db_url: String = utils::get_env_var("TECHEMPOWER_MONGODB_URL");
    let max_pool_size: u32 = utils::get_env_var("TECHEMPOWER_MAX_POOL_SIZE");
    let min_pool_size: u32 = utils::get_env_var("TECHEMPOWER_MIN_POOL_SIZE");
    let mut client_options = ClientOptions::parse(db_url).await.unwrap();
    client_options.max_pool_size = Some(max_pool_size);
    client_options.min_pool_size = Some(min_pool_size);
    client_options.connect_timeout = Some(Duration::from_millis(200));

    // the server will select the algorithm it supports from the list provided by the driver
    client_options.compressors = Some(vec![
        Compressor::Snappy,
        Compressor::Zlib {
            level: Default::default(),
        },
        Compressor::Zstd {
            level: Default::default(),
        },
    ]);

    let client = Client::with_options(client_options).unwrap();
    let database = client.database("hello_world");

    let router = Router::new()
        .hoop(salvo::affix::inject(database))
        .push(Router::with_path("db").get(world_row))
        .push(Router::with_path("fortunes").get(fortunes))
        .push(Router::with_path("queries").get(queries))
        .push(Router::with_path("updates").get(updates));

    let acceptor: TcpAcceptor = utils::reuse_listener().unwrap().try_into().unwrap();
    Server::new(acceptor).serve(router).await
}
