use std::convert::identity;

use nanorand::{Rng, WyRand};
use sqlx::Arguments;
use stretto::AsyncCache;
use viz::{
    get, header::SERVER, types::State, BytesMut, Error, IntoHandler, Response,
    ResponseExt, Result, Router, ServiceMaker,
};

mod db_sqlx;
mod models_sqlx;
mod server;
mod utils;

use db_sqlx::{Counter, DatabaseConnection, PgArguments, PgError, PgPoolOptions};
use models_sqlx::{Fortune, World};

async fn db(
    State(mut rng): State<WyRand>,
    DatabaseConnection(mut conn): DatabaseConnection,
) -> Result<Response> {
    let random_id = (rng.generate::<u32>() % 10_000 + 1) as i32;
    let mut args = PgArguments::default();
    args.add(random_id);

    let world: World =
        sqlx::query_as_with("SELECT id, randomnumber FROM World WHERE id = $1", args)
            .fetch_one(&mut conn)
            .await
            .map_err(PgError)?;

    let mut res = Response::json(world)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn fortunes(DatabaseConnection(mut conn): DatabaseConnection) -> Result<Response> {
    let mut items: Vec<Fortune> = sqlx::query_as("SELECT * FROM Fortune")
        .fetch_all(&mut conn)
        .await
        .map_err(PgError)?;

    items.push(Fortune {
        id: 0,
        message: "Additional fortune added at request time.".to_string(),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));

    let mut buf = BytesMut::with_capacity(2048);
    buf.extend(FortunesTemplate { items }.to_string().as_bytes());

    let mut res = Response::html(buf.freeze());
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn queries(
    Counter(count): Counter,
    State(mut rng): State<WyRand>,
    DatabaseConnection(mut conn): DatabaseConnection,
) -> Result<Response> {
    let mut worlds = Vec::with_capacity(count as usize);

    for _ in 0..count {
        let id = (rng.generate::<u32>() % 10_000 + 1) as i32;

        let mut args = PgArguments::default();
        args.add(id);

        let world = sqlx::query_as_with::<_, World, _>(
            "SELECT id, randomnumber FROM World WHERE id = $1",
            args,
        )
        .fetch_one(&mut conn)
        .await
        .map_err(PgError)?;

        worlds.push(world);
    }

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn cached_queries(
    Counter(count): Counter,
    State(mut rng): State<WyRand>,
    State(cached): State<AsyncCache<i32, World>>,
) -> Result<Response> {
    let worlds = (0..count)
        .map(|_| {
            let id = (rng.generate::<u32>() % 10_000 + 1) as i32;
            cached.get(&id).map(|v| v.read())
        })
        .filter_map(identity)
        .collect::<Vec<_>>();

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

async fn updates(
    Counter(count): Counter,
    State(mut rng): State<WyRand>,
    DatabaseConnection(mut conn): DatabaseConnection,
) -> Result<Response> {
    let mut worlds = Vec::with_capacity(count as usize);

    for _ in 0..count {
        let id = (rng.generate::<u32>() % 10_000 + 1) as i32;

        let mut args = PgArguments::default();
        args.add(id);

        let world = sqlx::query_as_with::<_, World, _>(
            "SELECT id, randomnumber FROM World WHERE id = $1",
            args,
        )
        .fetch_one(&mut conn)
        .await
        .map_err(PgError)?;

        worlds.push(world);
    }

    for w in &mut worlds {
        let randomnumber = (rng.generate::<u32>() % 10_000 + 1) as i32;
        let mut args = PgArguments::default();
        args.add(randomnumber);
        args.add(w.id);
        w.randomnumber = randomnumber;

        sqlx::query_with("UPDATE World SET randomNumber = $1 WHERE id = $2", args)
            .execute(&mut conn)
            .await
            .map_err(PgError)?;
    }

    let mut res = Response::json(worlds)?;
    res.headers_mut().insert(SERVER, utils::HDR_SERVER);
    Ok(res)
}

#[tokio::main]
async fn main() -> Result<()> {
    const DB_URL: &str =
        "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    let pool = PgPoolOptions::new()
        .max_connections(56)
        .min_connections(56)
        .connect(DB_URL)
        .await
        .map_err(PgError)?;

    let rng = WyRand::new();

    let cached = AsyncCache::new(10_000, 1e6 as i64, tokio::spawn).unwrap();

    {
        let mut conn = pool.acquire().await.map_err(PgError)?;
        let mut args = PgArguments::default();
        args.add(10_000);
        let worlds: Vec<World> =
            sqlx::query_as_with("SELECT id, randomnumber FROM World LIMIT $1", args)
                .fetch_all(&mut conn)
                .await
                .map_err(PgError)?;

        for w in worlds {
            cached.insert(w.id, w, 1).await;
        }
        cached.wait().await.expect("cache insert failed");
    }

    let app = Router::new()
        .route("/db", get(db.into_handler()))
        .route("/fortunes", get(fortunes.into_handler()))
        .route("/queries", get(queries.into_handler()))
        .route("/updates", get(updates.into_handler()))
        .with(State::new(pool))
        .route(
            "/cached_queries",
            get(cached_queries.into_handler()).with(State::new(cached)),
        )
        .with(State::new(rng));

    server::builder()
        .serve(ServiceMaker::from(app))
        .await
        .map_err(Error::normal)
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
