use crate::utils::{Message, JSON_LEN, PLAINTEXT_LEN};
use roa::http::header::CONTENT_LENGTH;
use roa::preload::*;
use roa::{Context, Result};

static HELLO_WORLD: &str = "Hello, World!";

#[inline]
pub async fn json(ctx: &mut Context) -> Result {
    ctx.resp.headers.insert(CONTENT_LENGTH, JSON_LEN.clone());
    ctx.write_json(&Message {
        message: HELLO_WORLD,
    })
}

#[inline]
pub async fn plaintext(ctx: &mut Context) -> Result {
    ctx.resp
        .headers
        .insert(CONTENT_LENGTH, PLAINTEXT_LEN.clone());
    ctx.write(HELLO_WORLD);
    Ok(())
}
