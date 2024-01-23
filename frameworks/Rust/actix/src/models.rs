use diesel::Queryable;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub message: &'static str,
}

#[allow(non_snake_case)]
#[derive(Serialize, Queryable, Debug)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

impl World {
    #[allow(dead_code)]
    pub fn new(id: i32, randomnumber: i32) -> Self {
        Self { id, randomnumber }
    }
}

#[allow(non_snake_case)]
#[derive(Serialize, Queryable, Debug)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}
