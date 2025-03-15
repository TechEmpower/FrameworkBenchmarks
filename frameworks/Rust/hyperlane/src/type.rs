use crate::*;

pub type DbPoolConnection = Pool<Postgres>;
pub type Queries = i32;

#[allow(bad_style)]
#[derive(Serialize, Default, Clone)]
pub struct QueryRow {
    pub id: i32,
    pub randomNumber: i32,
}

impl QueryRow {
    #[inline]
    pub fn new(id: i32, random_number: i32) -> Self {
        Self {
            id,
            randomNumber: random_number,
        }
    }
}

#[derive(Serialize)]
pub struct Fortunes {
    pub id: i32,
    pub message: String,
}

impl Fortunes {
    #[inline]
    pub fn new(id: i32, message: String) -> Self {
        Self { id, message }
    }
}
