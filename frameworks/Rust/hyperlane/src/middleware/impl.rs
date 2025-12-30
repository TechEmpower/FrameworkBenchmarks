use super::*;

impl ServerHook for RequestMiddleware {
    async fn new(_ctx: &Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &Context) {
        ctx.set_response_version(HttpVersion::Http1_1)
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
}
