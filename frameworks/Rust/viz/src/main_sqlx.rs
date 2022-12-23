use std::{borrow::Cow, convert::identity, thread::available_parallelism};

use nanorand::{Rng, WyRand};
use once_cell::sync::OnceCell;
use sqlx::{postgres::PgRow, Arguments, Pool, Row};
use viz::{
    header::{HeaderValue, SERVER},
    types::State,
    BytesMut, Error, Request, RequestExt, Response, ResponseExt, Result, Router,
    ServiceMaker,
};

mod db_sqlx;
mod models_sqlx;
mod server;
mod utils;

use db_sqlx::{
    Counter, DatabaseConnection, PgArguments, PgError, PgPoolOptions, Postgres,
};
use models_sqlx::{Fortune, World};
use utils::RANGE;

const DB_URL: &str =
    "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";
static CACHED: OnceCell<Vec<World>> = OnceCell::new();

async fn db(mut req: Request) -> Result<Response> {
    let (State(mut rng), DatabaseConnection(mut conn)) =
        req.extract::<(State<WyRand>, DatabaseConnection)>().await?;

    let random_id = rng.generate_range(RANGE);

    let world =
        sqlx::query_as::<_, World>("SELECT id, randomnumber FROM World WHERE id = $1")
            .bind(random_id)
            .fetch_one(&mut conn)
            .await
            .map_err(PgError)?;

    let mut res = Response::json(world)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn fortunes(mut req: Request) -> Result<Response> {
    let DatabaseConnection(mut conn) = req.extract::<DatabaseConnection>().await?;

    let mut items = sqlx::query("SELECT * FROM Fortune")
        .map(|row: PgRow| Fortune {
            id: row.get(0),
            message: Cow::Owned(row.get(1)),
        })
        .fetch_all(&mut conn)
        .await
        .map_err(PgError)?;

    items.push(Fortune {
        id: 0,
        message: Cow::Borrowed("Additional fortune added at request time."),
    });

    items.sort_by(|it, next| it.message.cmp(&next.message));

    let mut buf = BytesMut::with_capacity(2048);
    buf.extend(FortunesTemplate { items }.to_string().as_bytes());

    let mut res = Response::html(buf.freeze());
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn queries(mut req: Request) -> Result<Response> {
    let (Counter(count), State(mut rng), DatabaseConnection(mut conn)) = req
        .extract::<(Counter, State<WyRand>, DatabaseConnection)>()
        .await?;

    let mut worlds = Vec::<World>::with_capacity(count as usize);

    for _ in 0..count {
        let id = rng.generate_range(RANGE);

        sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
            .bind(id)
            .fetch_one(&mut conn)
            .await
            .map(|world| worlds.push(world))
            .map_err(PgError)?;
    }

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn cached_queries(mut req: Request) -> Result<Response> {
    let (Counter(count), State(mut rng)) =
        req.extract::<(Counter, State<WyRand>)>().await?;

    let worlds = (0..count)
        .map(|_| {
            let id = rng.generate_range(RANGE) as usize;
            CACHED.get()?.get(id)
        })
        .filter_map(identity)
        .collect::<Vec<_>>();

    let mut res = Response::json(worlds)?;
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn updates(mut req: Request) -> Result<Response> {
    let (Counter(count), State(mut rng), DatabaseConnection(mut conn)) = req
        .extract::<(Counter, State<WyRand>, DatabaseConnection)>()
        .await?;

    let mut worlds = Vec::<World>::with_capacity(count as usize);

    for _ in 0..count {
        let id = rng.generate_range(RANGE);

        sqlx::query_as("SELECT id, randomnumber FROM World WHERE id = $1")
            .bind(id)
            .fetch_one(&mut conn)
            .await
            .map(|world| worlds.push(world))
            .map_err(PgError)?;
    }

    for w in &mut worlds {
        let randomnumber = rng.generate_range(RANGE);
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
    res.headers_mut()
        .insert(SERVER, HeaderValue::from_static("Viz"));
    Ok(res)
}

async fn populate_cache(pool: Pool<Postgres>) -> Result<(), Error> {
    let mut conn = pool.acquire().await.map_err(Error::normal)?;
    let worlds = sqlx::query_as("SELECT * FROM World LIMIT $1")
        .bind(10_000)
        .fetch_all(&mut conn)
        .await
        .map_err(PgError)?;
    CACHED
        .set(worlds)
        .map_err(|_| PgError::from(sqlx::Error::RowNotFound).into())
}

#[tokio::main]
async fn main() -> Result<()> {
    let max = available_parallelism().map(|n| n.get()).unwrap_or(16) as u32;

    let pool = PgPoolOptions::new()
        .max_connections(max)
        .min_connections(max)
        .connect(DB_URL)
        .await
        .map_err(PgError)?;

    populate_cache(pool.clone()).await?;

    let rng = WyRand::new();

    let app = Router::new()
        .get("/db", db)
        .get("/fortunes", fortunes)
        .get("/queries", queries)
        .get("/updates", updates)
        .with(State::new(pool))
        .get("/cached_queries", cached_queries)
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
