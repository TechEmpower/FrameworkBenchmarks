use rocket::serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
#[serde(crate = "rocket::serde")]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
#[serde(crate = "rocket::serde")]
pub struct World {
    pub id: i32,
    #[sqlx(rename = "randomnumber")]
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}
