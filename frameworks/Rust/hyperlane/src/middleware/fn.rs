use super::*;

pub async fn request(ctx: Context) {
    ctx.set_response_version(HttpVersion::HTTP1_1)
        .await
        .set_response_header(CONNECTION, KEEP_ALIVE)
        .await
        .set_response_header(SERVER, HYPERLANE)
        .await
        .set_response_header(DATE, &gmt())
        .await
        .set_response_status_code(200)
        .await
        .set_response_header(CONTENT_TYPE, APPLICATION_JSON)
        .await;
}
