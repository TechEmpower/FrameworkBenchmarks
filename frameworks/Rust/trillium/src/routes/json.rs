use serde_json::{json, to_string};
use trillium::{conn_try, Conn, KnownHeaderName};

pub async fn handler(conn: Conn) -> Conn {
    let body = conn_try!(to_string(&json!({"message": "Hello, World!"})), conn);
    conn.ok(body)
        .with_header(KnownHeaderName::ContentType, "application/json")
}
