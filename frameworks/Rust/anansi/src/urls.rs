use anansi::web::prelude::*;
#[cfg(not(feature = "raw"))]
use crate::hello::world::views::WorldView;

#[cfg(not(feature = "raw"))]
routes! {
    path!("/json", WorldView::json),
    path!("/db", WorldView::db),
    path!("/queries", WorldView::queries),
    path!("/fortunes", WorldView::fortunes),
    path!("/updates", WorldView::updates),
    path!("/plaintext", WorldView::plaintext),
}

#[cfg(feature = "raw")]
use crate::hello::world::raw::WorldView;

#[cfg(feature = "raw")]
routes! {
    path!("/db", WorldView::db),
    path!("/queries", WorldView::queries),
    path!("/fortunes", WorldView::raw_fortunes),
    path!("/updates", WorldView::updates),
}
