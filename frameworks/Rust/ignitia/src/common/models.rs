use serde::{Deserialize, Serialize};

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct FortuneInfo {
    pub id: i32,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct World {
    pub id: i32,
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}
