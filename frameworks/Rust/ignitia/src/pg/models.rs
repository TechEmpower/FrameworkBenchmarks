use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct World {
    pub id: i32,
    #[serde(rename = "randomNumber")]
    pub randomnumber: i32,
}
