use saphir::prelude::*;
use serde_derive::Serialize;

#[derive(Serialize)]
struct RspMessage<'t0> {
    message: &'t0 str,
}

pub struct JsonController;

#[controller]
impl JsonController {
    #[get("/")]
    async fn return_json(&self) -> (u16, Json<RspMessage<'static>>) {
        (200, Json(RspMessage { message: crate::HELLO_WORLD }))
    }
}