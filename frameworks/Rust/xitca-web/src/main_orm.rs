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

use ser::{Num, World};
use util::{HandleResult, SERVER_HEADER_VALUE};
use xitca_web::{
    App,
    codegen::route,
    handler::{html::Html, json::Json, query::Query, state::StateRef},
    http::{WebResponse, header::SERVER},
};

use orm::Pool;

fn main() -> std::io::Result<()> {
    App::new()
        .with_async_state(Pool::create)
        .at_typed(db)
        .at_typed(fortunes)
        .at_typed(queries)
        .at_typed(updates)
        .map(header)
        .serve()
        .disable_vectored_write()
        .bind("0.0.0.0:8080")?
        .run()
        .wait()
}

fn header(mut res: WebResponse) -> WebResponse {
    res.headers_mut().insert(SERVER, SERVER_HEADER_VALUE);
    res
}

#[route("/db", method = get)]
async fn db(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<World>> {
    pool.get_world().await.map(Json)
}

#[route("/fortunes", method = get)]
async fn fortunes(StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Html<String>> {
    let html = pool.tell_fortune().await?.render_once()?;
    Ok(Html(html))
}

#[route("/queries", method = get)]
async fn queries(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.get_worlds(num).await.map(Json)
}

#[route("/updates", method = get)]
async fn updates(Query(Num(num)): Query<Num>, StateRef(pool): StateRef<'_, Pool>) -> HandleResult<Json<Vec<World>>> {
    pool.update(num).await.map(Json)
}
