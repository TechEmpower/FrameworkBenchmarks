use std::sync::Arc;

use vidi::{Request, RequestExt, Response, ResponseExt, Result, Router, types::State};
use yarte::Template;

mod db_pg;
mod models;
mod server;
mod utils;

use db_pg::{PgConnection, get_conn};

#[derive(Template)]
#[template(path = "fortune.hbs")]
pub struct FortunesTemplate {
    pub fortunes: Vec<models::Fortune>,
}

const DB_URL: &str =
    "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

async fn db(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let world = conn.get_world().await?;

    let res = Response::json(world)?;

    Ok(res)
}

async fn fortunes(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;

    let fortunes = conn.tell_fortune().await?;

    let buf = FortunesTemplate { fortunes }
        .call()
        .expect("error rendering template");

    Ok(Response::html(buf))
}

async fn queries(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;
    let count = utils::get_query_param(req.query_string());

    let worlds = conn.get_worlds(count).await?;

    let res = Response::json(worlds)?;

    Ok(res)
}

async fn updates(req: Request) -> Result<Response> {
    let conn = get_conn(req.state::<Arc<PgConnection>>())?;
    let count = utils::get_query_param(req.query_string());

    let worlds = conn.update(count).await?;

    let res = Response::json(worlds)?;

    Ok(res)
}

async fn app() {
    let conn = PgConnection::connect(DB_URL).await;

    let app = Router::new()
        .get("/db", db)
        .get("/fortunes", fortunes)
        .get("/queries", queries)
        .get("/updates", updates)
        .with(State::new(conn));

    server::serve(app).await.unwrap()
}

fn main() {
    server::run(app)
}
