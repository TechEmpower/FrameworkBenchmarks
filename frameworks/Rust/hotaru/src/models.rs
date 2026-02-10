use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct World {
    pub id: i32,
    #[serde(rename = "randomNumber")]
    pub random_number: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Fortune {
    pub id: i32,
    pub message: String,
}

#[derive(Debug, Deserialize)]
pub struct QueryParams {
    pub queries: Option<u16>,
}

#[derive(Debug, Deserialize)]
pub struct CountParams {
    pub count: Option<u16>,
}
