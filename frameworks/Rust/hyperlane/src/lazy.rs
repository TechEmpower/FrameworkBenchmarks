use crate::*;

pub static DB: Lazy<ArcRwLock<Option<DbPoolConnection>>> =
    Lazy::new(|| Arc::new(RwLock::new(None)));

pub static UPDATE_STATE: Lazy<ArcRwLock<HashMap<usize, Statement>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));

pub static UPDATE_QUERY: Lazy<ArcRwLock<HashMap<usize, Vec<QueryRow>>>> =
    Lazy::new(|| arc_rwlock(HashMap::new()));
