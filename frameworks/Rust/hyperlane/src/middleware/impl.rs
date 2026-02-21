use super::*;

impl ServerHook for RequestMiddleware {
    async fn new(_ctx: &mut Context) -> Self {
        Self
    }

    async fn handle(self, ctx: &mut Context) {
        ctx.get_mut_response()
            .set_version(HttpVersion::Http1_1)
            .set_header(CONNECTION, KEEP_ALIVE)
            .set_header(SERVER, HYPERLANE)
            .set_header(DATE, gmt())
            .set_status_code(200)
            .set_header(CONTENT_TYPE, APPLICATION_JSON);
    }
}
