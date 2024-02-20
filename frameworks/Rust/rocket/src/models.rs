use rocket::serde::{Deserialize, Serialize};
use rocket_db_pools::sqlx::FromRow;

#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
pub struct Message {
    pub message: &'static str,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
#[serde(crate = "rocket::serde")]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
#[serde(crate = "rocket::serde")]
pub struct World {
    pub id: i32,
    #[sqlx(rename = "randomnumber")]
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}
