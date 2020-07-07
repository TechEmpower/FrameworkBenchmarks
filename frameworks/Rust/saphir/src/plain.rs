use saphir::prelude::*;

pub struct PlainController;

#[controller(name = "plaintext")]
impl PlainController {
    #[get("/")]
    async fn return_plain(&self) -> (u16, &'static str) {
        (200, crate::HELLO_WORLD)
    }
}