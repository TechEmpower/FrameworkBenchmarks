use trillium::{Conn, KnownHeaderName};

pub async fn handler(conn: Conn) -> Conn {
    conn.with_header(KnownHeaderName::ContentType, "text/plain")
        .ok("Hello, World!")
}
