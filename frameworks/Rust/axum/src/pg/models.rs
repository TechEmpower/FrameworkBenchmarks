use std::borrow::Cow;

use serde::{Deserialize, Serialize};

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: Cow<'static, str>,
}

#[allow(non_snake_case)]
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct World {
    pub id: i32,
    #[serde(rename = "randomNumber")]
    pub randomnumber: i32,
}
