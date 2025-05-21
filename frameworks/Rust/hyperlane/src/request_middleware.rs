use super::*;

pub async fn request(ctx: Context) {
    ctx.set_response_header(CONNECTION, CONNECTION_KEEP_ALIVE)
        .await
        .set_response_header(SERVER, HYPERLANE)
        .await
        .set_response_header(DATE, gmt())
        .await;
    #[cfg(feature = "plaintext")]
    {
        ctx.set_response_header(CONTENT_TYPE, TEXT_PLAIN).await;
    }
    #[cfg(feature = "fortunes")]
    {
        ctx.set_response_header(CONTENT_TYPE, content_type_charset(TEXT_HTML, UTF8))
            .await;
    }
    #[cfg(any(
        feature = "json",
        feature = "db",
        feature = "query",
        feature = "update",
        feature = "cached_query"
    ))]
    {
        ctx.set_response_header(CONTENT_TYPE, APPLICATION_JSON)
            .await;
    }
}
