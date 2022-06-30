mod models;
mod utils;

use std::fmt::Write;
use std::time::Duration;

use actix_http::KeepAlive;
use actix_web::{
    http::{
        header::{HeaderValue, CONTENT_TYPE, SERVER},
        StatusCode,
    },
    web, App, HttpResponse, HttpServer,
};
use deadpool_postgres::{Config, Pool, PoolConfig, Runtime};
use futures::{stream::FuturesUnordered, TryStreamExt};
use models::{Fortune, World};
use rand::{prelude::SmallRng, Rng, SeedableRng};
use tokio_postgres::{types::ToSql, NoTls};
use utils::{Queries, Result, CONNECTION_POOL_SIZE};
use yarte::ywrite_html;

async fn find_random_world(pool: &Pool) -> Result<World> {
    let conn = pool.get().await?;
    let world = conn
        .prepare("SELECT * FROM world WHERE id=$1")
        .await
        .unwrap();

    let mut rng = SmallRng::from_entropy();
    let id = (rng.gen::<u32>() % 10_000 + 1) as i32;

    let row = conn.query_one(&world, &[&id]).await?;

    Ok(World {
        id: row.get(0),
        randomnumber: row.get(1),
    })
}

async fn find_random_worlds(pool: &Pool, num_of_worlds: usize) -> Result<Vec<World>> {
    let mut futs = FuturesUnordered::new();
    for _ in 0..num_of_worlds {
        futs.push(find_random_world(pool));
    }

    let mut worlds = Vec::with_capacity(num_of_worlds);
    while let Some(world) = futs.try_next().await? {
        worlds.push(world);
    }

    Ok(worlds)
}

#[actix_web::get("/db")]
async fn db(data: web::Data<Pool>) -> Result<HttpResponse<Vec<u8>>> {
    let world = find_random_world(&data).await?;
    let mut bytes = Vec::with_capacity(48);
    serde_json::to_writer(&mut bytes, &world)?;

    let mut res = HttpResponse::with_body(StatusCode::OK, bytes);
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Actix"));
    res.headers_mut()
        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

    Ok(res)
}

#[actix_web::get("/queries")]
async fn queries(
    data: web::Data<Pool>,
    query: web::Query<Queries>,
) -> Result<HttpResponse<Vec<u8>>> {
    let n_queries = query.q;

    let worlds = find_random_worlds(&data, n_queries).await?;

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
    data: web::Data<Pool>,
    query: web::Query<Queries>,
) -> Result<HttpResponse<Vec<u8>>> {
    let mut worlds = find_random_worlds(&data, query.q).await?;

    let mut rng = SmallRng::from_entropy();

    let mut updates = "UPDATE world SET randomnumber = CASE id ".to_string();
    let mut params: Vec<&(dyn ToSql + Sync)> = Vec::with_capacity(query.q as usize * 3);

    let mut n_params = 1;
    for world in worlds.iter_mut() {
        let new_random_number = (rng.gen::<u32>() % 10_000 + 1) as i32;
        write!(&mut updates, "when ${} then ${} ", n_params, n_params + 1).unwrap();
        world.randomnumber = new_random_number;
        n_params += 2;
    }

    // need separate loop to borrow immutably
    for world in worlds.iter() {
        params.push(&world.id);
        params.push(&world.randomnumber);
    }

    updates.push_str("ELSE randomnumber END WHERE id IN (");
    for world in worlds.iter() {
        write!(&mut updates, "${},", n_params).unwrap();
        params.push(&world.id);
        n_params += 1;
    }

    updates.pop(); // drop trailing comma
    updates.push(')');

    let conn = data.get().await?;
    let stmt = conn.prepare(&updates).await?;
    conn.query(&stmt, &params).await?;

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
async fn fortune(data: web::Data<Pool>) -> Result<HttpResponse<Vec<u8>>> {
    let conn = data.get().await?;
    let stmt = conn.prepare("SELECT * FROM Fortune").await?;
    let params: &[&'static str] = &[];
    let s = conn.query_raw(&stmt, params).await?;

    let mut stream = Box::pin(s);
    let mut fortunes = Vec::new();

    while let Some(row) = stream.try_next().await? {
        fortunes.push(Fortune {
            id: row.get(0),
            message: row.get(1),
        });
    }

    fortunes.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    fortunes.sort_by(|a, b| a.message.cmp(&b.message));

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

#[actix_web::main]
async fn main() -> Result<()> {
    println!("Starting http server: 0.0.0.0:8080");

    let mut cfg = Config::new();
    cfg.host = Some("tfb-database".to_string());
    cfg.dbname = Some("hello_world".to_string());
    cfg.user = Some("benchmarkdbuser".to_string());
    cfg.password = Some("benchmarkdbpass".to_string());
    let pc = PoolConfig::new(CONNECTION_POOL_SIZE);
    cfg.pool = pc.into();
    let pool = cfg.create_pool(Some(Runtime::Tokio1), NoTls).unwrap();

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(pool.clone()))
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
