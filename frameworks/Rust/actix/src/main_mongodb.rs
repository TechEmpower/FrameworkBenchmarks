mod models_mongodb;
mod utils;

use models_mongodb::{Fortune, World};
use utils::{Queries, Result, CONNECTION_POOL_SIZE};

use actix_http::{
    header::{HeaderValue, CONTENT_TYPE, SERVER},
    KeepAlive, StatusCode,
};
use actix_web::{web, App, HttpResponse, HttpServer};
use anyhow::bail;
use futures::{stream::FuturesUnordered, TryStreamExt};
use mongodb::bson::doc;
use mongodb::{options::ClientOptions, Client};
use rand::{prelude::SmallRng, Rng, SeedableRng};
use tokio::runtime::Handle;
use yarte::ywrite_html;

use std::time::Duration;

struct Data {
    client: Client,
    tokio_runtime: tokio::runtime::Handle,
}

async fn find_random_world(data: web::Data<Data>) -> Result<World> {
    let runtime = data.tokio_runtime.clone();
    runtime
        .spawn(async move {
            let mut rng = SmallRng::from_entropy();
            let id = (rng.gen::<u32>() % 10_000 + 1) as i32;

            let coll = data.client.database("hello_world").collection("world");
            let world = coll
                .find_one(doc! { "id": id as f32 }, None)
                .await?
                .expect("should find world");
            Ok(world)
        })
        .await?
}

#[actix_web::get("/db")]
async fn db(data: web::Data<Data>) -> Result<HttpResponse<Vec<u8>>> {
    let world = find_random_world(data).await?;
    let mut bytes = Vec::with_capacity(48);
    serde_json::to_writer(&mut bytes, &world)?;

    let mut res = HttpResponse::with_body(StatusCode::OK, bytes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

    Ok(res)
}

async fn find_random_worlds(data: web::Data<Data>, num_of_worlds: usize) -> Result<Vec<World>> {
    let mut futs = FuturesUnordered::new();
    for _ in 0..num_of_worlds {
        futs.push(find_random_world(data.clone()))
    }

    let mut worlds = Vec::with_capacity(num_of_worlds);
    while let Some(world) = futs.try_next().await? {
        worlds.push(world);
    }

    Ok(worlds)
}

#[actix_web::get("/queries")]
async fn queries(
    data: web::Data<Data>,
    query: web::Query<Queries>,
) -> Result<HttpResponse<Vec<u8>>> {
    let n_queries = query.q;

    let worlds = find_random_worlds(data, n_queries).await?;

    let mut bytes = Vec::with_capacity(35 * n_queries);
    serde_json::to_writer(&mut bytes, &worlds)?;

    let mut res = HttpResponse::with_body(StatusCode::OK, bytes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

    Ok(res)
}

#[actix_web::get("/updates")]
async fn updates(
    data: web::Data<Data>,
    query: web::Query<Queries>,
) -> Result<HttpResponse<Vec<u8>>> {
    let tokio_runtime = data.tokio_runtime.clone();
    let client = data.client.clone();

    let mut worlds = find_random_worlds(data, query.q).await?;

    let mut rng = SmallRng::from_entropy();
    let mut updates = Vec::new();
    for world in worlds.iter_mut() {
        let new_random_number = (rng.gen::<u32>() % 10_000 + 1) as i32;
        updates.push(doc! {
            "q": { "id": world.id }, "u": { "$set": { "randomNumber": new_random_number }}
        });
        world.random_number = new_random_number;
    }

    tokio_runtime
        .spawn(async move {
            client
                .database("hello_world")
                .run_command(
                    doc! {
                        "update": "world",
                        "updates": updates,
                        "ordered": false,
                    },
                    None,
                )
                .await
        })
        .await??;

    let mut bytes = Vec::with_capacity(35 * worlds.len());
    serde_json::to_writer(&mut bytes, &worlds)?;

    let mut res = HttpResponse::with_body(StatusCode::OK, bytes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

    Ok(res)
}

#[actix_web::get("/fortunes")]
async fn fortune(data: web::Data<Data>) -> Result<HttpResponse<Vec<u8>>> {
    async fn fetch_fortunes(client: &Client) -> Result<Vec<Fortune>> {
        let fortunes_cursor = client
            .database("hello_world")
            .collection::<Fortune>("fortune")
            .find(None, None)
            .await?;

        let mut fortunes: Vec<Fortune> = fortunes_cursor.try_collect().await?;
        fortunes.push(Fortune {
            id: 0,
            message: "Additional fortune added at request time.".to_string(),
        });

        fortunes.sort_by(|a, b| a.message.cmp(&b.message));

        Ok(fortunes)
    }

    let d = data.clone();
    let fortunes = data
        .tokio_runtime
        .spawn(async move { fetch_fortunes(&d.client).await })
        .await??;

    let mut body = Vec::with_capacity(2048);
    ywrite_html!(body, "{{> fortune }}");

    let mut res = HttpResponse::with_body(StatusCode::OK, body);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut().insert(
        CONTENT_TYPE,
        HeaderValue::from_static("text/html; charset=utf-8"),
    );

    Ok(res)
}

fn main() {
    actix_web::rt::System::with_tokio_rt(|| tokio::runtime::Runtime::new().unwrap())
        .block_on(async_main())
        .unwrap();
}

async fn async_main() -> Result<()> {
    println!("Starting http server: 0.0.0.0:8080");

    // use a separate, multithreaded tokio runtime for db queries for better performance
    let handle = Handle::current();

    let uri = std::env::var("ACTIX_TECHEMPOWER_MONGODB_URL")
        .or_else(|_| bail!("missing ACTIX_TECHEMPOWER_MONGODB_URL env variable"))?;
    let mut options = ClientOptions::parse(uri).await?;
    options.max_pool_size = Some(CONNECTION_POOL_SIZE as u32);
    let client = Client::with_options(options)?;

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(Data {
                client: client.clone(),
                tokio_runtime: handle.clone(),
            }))
            .service(fortune)
            .service(db)
            .service(queries)
            .service(updates)
    })
    .keep_alive(KeepAlive::Os)
    .client_request_timeout(Duration::from_secs(0))
    .backlog(1024)
    .bind("0.0.0.0:8080")?
    .run()
    .await?;

    Ok(())
}
