use crate::*;

pub async fn request(ctx: Context) {
    let _ = ctx
        .set_response_header(CONNECTION, CONNECTION_KEEP_ALIVE)
        .await
        .set_response_header(CONTENT_TYPE, APPLICATION_JSON)
        .await
        .set_response_header(SERVER, HYPERLANE)
        .await
        .set_response_header(DATE, gmt())
        .await;
}
