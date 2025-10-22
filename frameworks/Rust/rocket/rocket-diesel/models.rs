use serde::{Deserialize, Serialize};

#[allow(non_snake_case)]
#[derive(Debug, Deserialize, Serialize)]
#[serde(crate = "rocket::serde")]
pub struct Message {
    pub message: &'static str,
}

#[allow(non_snake_case)]
#[derive(Debug, Clone, Serialize, Queryable)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[allow(non_snake_case)]
#[derive(Debug, Deserialize, Serialize, Queryable)]
#[serde(crate = "rocket::serde")]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}
