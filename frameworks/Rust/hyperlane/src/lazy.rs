use crate::*;

pub static DB: OnceCell<DbPoolConnection> = OnceCell::new();
pub static CACHE: OnceCell<Vec<QueryRow>> = OnceCell::new();
