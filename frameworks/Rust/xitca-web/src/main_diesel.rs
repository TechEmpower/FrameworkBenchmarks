#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[macro_use]
extern crate diesel;

mod db_diesel;
mod schema;
mod ser;
mod util;

use std::{error::Error, io};

use xitca_web::{
    dev::{bytes::Bytes, fn_service},
    http::{
        header::{CONTENT_TYPE, SERVER},
        Method,
    },
    request::WebRequest,
    App, HttpServer,
};

use self::db_diesel::{create, DieselPool};
use self::util::{
    internal, json, json_response, not_found, plain_text, AppState, HandleResult, QueryParse,
};

type State = AppState<DieselPool>;

#[tokio::main(flavor = "current_thread")]
async fn main() -> io::Result<()> {
    let config = "postgres://benchmarkdbuser:benchmarkdbpass@tfb-database/hello_world";

    HttpServer::new(move || {
        App::with_async_state(move || async move {
            let pool = create(config).await.unwrap();
            AppState::new(pool)
        })
        .service(fn_service(handle))
    })
    .disable_vectored_write()
    .max_request_headers::<8>()
    .bind("0.0.0.0:8080")?
    .run()
    .await
}

async fn handle(req: &mut WebRequest<'_, State>) -> HandleResult {
    let inner = req.request_mut();

    match (inner.method(), inner.uri().path()) {
        (&Method::GET, "/plaintext") => plain_text(req),
        (&Method::GET, "/json") => json(req),
        (&Method::GET, "/db") => db(req).await,
        (&Method::GET, "/fortunes") => fortunes(req).await,
        (&Method::GET, "/queries") => queries(req).await,
        (&Method::GET, "/updates") => updates(req).await,
        _ => not_found(),
    }
}

async fn db(req: &mut WebRequest<'_, State>) -> HandleResult {
    match req.state().client().get_world().await {
        Ok(world) => json_response(req, &world),
        Err(_) => internal(),
    }
}

async fn fortunes(req: &mut WebRequest<'_, State>) -> HandleResult {
    match _fortunes(req.state().client()).await {
        Ok(body) => {
            let mut res = req.as_response(body);

            res.headers_mut().append(SERVER, util::SERVER_HEADER_VALUE);
            res.headers_mut()
                .append(CONTENT_TYPE, util::HTML_HEADER_VALUE);

            Ok(res)
        }
        Err(_) => internal(),
    }
}

async fn queries(req: &mut WebRequest<'_, State>) -> HandleResult {
    let num = req.request_mut().uri().query().parse_query();

    match req.state().client().get_worlds(num).await {
        Ok(worlds) => json_response(req, worlds.as_slice()),
        Err(_) => internal(),
    }
}

async fn updates(req: &mut WebRequest<'_, State>) -> HandleResult {
    let num = req.request_mut().uri().query().parse_query();

    match req.state().client().update(num).await {
        Ok(worlds) => json_response(req, worlds.as_slice()),
        Err(_) => internal(),
    }
}

#[inline]
async fn _fortunes(pool: &DieselPool) -> Result<Bytes, Box<dyn Error + Send + Sync + 'static>> {
    use sailfish::TemplateOnce;
    let fortunes = pool.tell_fortune().await?.render_once()?;
    Ok(fortunes.into())
}
