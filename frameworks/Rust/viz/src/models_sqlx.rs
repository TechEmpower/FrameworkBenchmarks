use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(Debug, PartialEq, Deserialize, Serialize, FromRow)]
pub struct World {
    pub id: i32,
    pub randomnumber: i32,
}

#[derive(Debug, PartialEq, Deserialize, Serialize, FromRow)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}
