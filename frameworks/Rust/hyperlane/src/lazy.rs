use crate::*;

pub static DB: Lazy<ArcRwLock<Option<DbPoolConnection>>> =
    Lazy::new(|| Arc::new(RwLock::new(None)));
pub static QUERY_SQL: Lazy<ArcRwLock<HashMap<Queries, String>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));
