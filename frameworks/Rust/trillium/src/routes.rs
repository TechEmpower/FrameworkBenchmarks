use trillium_router::Router;
mod db;
mod fortune;
mod json;
mod plaintext;
mod queries;
mod updates;

pub fn router() -> Router {
    Router::build(|mut router| {
        router.get("/fortunes", fortune::handler);
        router.get("/json", json::handler);
        router.get("/queries/:queries", queries::handler);
        router.get("/queries", queries::handler);
        router.get("/db", db::handler);
        router.get("/plaintext", plaintext::handler);
        router.get("/updates/:updates", updates::handler);
        router.get("/updates", updates::handler);
    })
}
