#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod ser;
mod util;

#[cfg(all(feature = "diesel", not(feature = "toasty")))]
#[path = "./db_diesel.rs"]
mod orm;

#[cfg(all(feature = "toasty", not(feature = "diesel")))]
#[path = "./db_toasty.rs"]
mod orm;

use xitca_web::{
    App,
    codegen::route,
    handler::{html::Html, json::Json, query::Query, state::StateRef},
    http::{WebResponse, header::SERVER},
};

use orm::Pool;
use ser::{Num, World};
use util::{HandleResult, SERVER_HEADER_VALUE};

fn main() -> std::io::Result<()> {
    App::new()
        .with_async_state(Pool::create)
        .at_typed(db)
        .at_typed(fortunes)
        .at_typed(queries)
        .at_typed(updates)
        .map(|mut res: WebResponse| {
            res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
            res
        })
        .serve()
        .disable_vectored_write()
        .bind("0.0.0.0:8080")?
        .run()
        .wait()
}

#[route("/db", method = get)]
async fn db(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<World>> {
    pool.db().await.map(Json)
}

#[route("/fortunes", method = get)]
async fn fortunes(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Html<String>> {
    pool.fortunes().await?.render_once().map(Html)
}

#[route("/queries", method = get)]
async fn queries(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.queries(num).await.map(Json)
}

#[route("/updates", method = get)]
async fn updates(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.updates(num).await.map(Json)
}
