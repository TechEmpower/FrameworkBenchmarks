use crate::db::Db;
use crate::routes::router;

pub fn application() -> impl trillium::Handler {
    (
        #[cfg(debug_assertions)]
        trillium_logger::logger(),
        Db::default(),
        router(),
    )
}
