use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize, FromRow)]
pub struct World {
    pub id: i32,
    #[sqlx(rename = "randomnumber")]
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}
