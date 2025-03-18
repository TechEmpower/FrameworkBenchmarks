use crate::*;

pub static DB: Lazy<ArcRwLock<Option<DbPoolConnection>>> =
    Lazy::new(|| Arc::new(RwLock::new(None)));
pub static CACHE: Lazy<ArcRwLock<Vec<QueryRow>>> = Lazy::new(|| arc_rwlock(vec![]));
