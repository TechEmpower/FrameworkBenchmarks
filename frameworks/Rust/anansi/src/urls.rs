use anansi::web::prelude::*;
use crate::prelude::Request;
#[cfg(not(feature = "raw"))]
use crate::hello::world::views::WorldView;

#[cfg(not(feature = "raw"))]
pub fn routes<R: Request>() -> Router<R> {
    Router::new()
        .route("/json", WorldView::json)
        .route("/db", WorldView::db)
        .route("/queries", WorldView::queries)
        .route("/fortunes", WorldView::fortunes)
        .route("/updates", WorldView::updates)
        .route("/plaintext", WorldView::plaintext)
        .route("/cached-queries", WorldView::cached_queries)
}

#[cfg(feature = "raw")]
use crate::hello::world::raw::WorldView;

#[cfg(feature = "raw")]
use crate::hello::middleware::Pg;

#[cfg(feature = "raw")]
pub fn routes<R: Request + Pg>() -> Router<R> {
    Router::new()
        .route("/db", WorldView::db)
        .route("/queries", WorldView::queries)
        .route("/fortunes", WorldView::raw_fortunes)
        .route("/updates", WorldView::updates)
        .route("/cached-queries", WorldView::cached_queries)
}
