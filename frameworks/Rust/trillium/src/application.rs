use crate::db::Db;
use crate::routes::router;
use trillium_logger::Logger;

pub fn application() -> impl trillium::Handler {
    (
        if cfg!(debug_assertions) {
            Some(Logger::new())
        } else {
            None
        },
        Db::default(),
        router(),
    )
}
