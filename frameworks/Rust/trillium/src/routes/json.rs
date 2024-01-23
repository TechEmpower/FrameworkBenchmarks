use serde_json::json;
use trillium::Conn;
use trillium_api::ApiConnExt;

pub async fn handler(conn: Conn) -> Conn {
    conn.with_json(&json!({"message": "Hello, World!"}))
}
