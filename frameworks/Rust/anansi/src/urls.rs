use anansi::web::prelude::*;
use crate::hello::world::views::WorldView;

routes! {
    path!("/json", WorldView::json),
    path!("/db", WorldView::db),
    path!("/queries", WorldView::queries),
    path!("/fortunes", WorldView::fortunes),
    path!("/updates", WorldView::updates),
    path!("/plaintext", WorldView::plaintext),
}
