use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Fortune {
    pub id: f32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct FortuneInfo {
    pub id: i32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct World {
    pub id: f32,
    #[serde(rename = "randomNumber")]
    pub random_number: f32,
}
