use crate::*;

pub async fn request(controller_data: ControllerData) {
    let _ = controller_data
        .set_response_header(CONNECTION, CONNECTION_KEEP_ALIVE)
        .await
        .set_response_header(
            CONTENT_TYPE,
            format!("{}; {}", APPLICATION_JSON, CHARSET_UTF_8),
        )
        .await
        .set_response_header(SERVER, HYPERLANE)
        .await
        .set_response_header(DATE, generate_rfc1123_timestamp())
        .await
        .set_response_status_code(200)
        .await;
}
