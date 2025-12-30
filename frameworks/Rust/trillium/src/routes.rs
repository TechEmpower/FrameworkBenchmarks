use trillium_router::Router;
mod cached_queries;
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
        let cached_queries = cached_queries::handler();
        router.get("/cached-queries/:count", cached_queries.clone());
        router.get("/cached-queries", cached_queries);
    })
}
