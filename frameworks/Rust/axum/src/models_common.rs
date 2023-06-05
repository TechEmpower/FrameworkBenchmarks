use serde::Serialize;

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}
