use crate::*;

pub static DB: Lazy<ArcRwLock<Option<DbPoolConnection>>> =
    Lazy::new(|| Arc::new(RwLock::new(None)));
