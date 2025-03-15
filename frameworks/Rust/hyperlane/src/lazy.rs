use crate::*;

pub static DB: Lazy<ArcRwLock<Option<DbPoolConnection>>> =
    Lazy::new(|| Arc::new(RwLock::new(None)));
pub static UPDATE_STATE: Lazy<ArcRwLock<HashMap<Queries, Statement>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));
pub static UPDATE_QUERY: Lazy<ArcRwLock<HashMap<Queries, Vec<QueryRow>>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));
pub static QUERY_STATE: Lazy<ArcRwLock<HashMap<Queries, Statement>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));
