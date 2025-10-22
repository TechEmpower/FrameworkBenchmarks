use serde_derive::{Serialize, Deserialize};

#[derive(Serialize)]
pub struct JsonMessage {
    pub message: &'static str,
}

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize, Clone)]
pub struct World {
    pub id: f32,
    pub randomNumber: f32,
}

#[derive(Deserialize, Serialize)]
pub struct Fortune {
    pub id: f32,
    pub message: String
}

#[allow(non_snake_case)]
#[derive(Serialize, Deserialize, Clone)]
pub struct CachedWorld {
    pub id: f32,
    pub randomNumber: f32,
}