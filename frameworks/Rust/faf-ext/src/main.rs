use faf::const_http::*;
use faf::{cb, concats, Empty, Request, Response};
use yarte::Serialize;

const TEXT_PLAIN_CONTENT_TYPE: &str = "Content-Type: text/plain";
const CONTENT_LENGTH: &str = "Content-Length: ";

const JSON_CONTENT_TYPE: &str = "Content-Type: application/json";

#[inline]
fn plaintext(res: &mut Response<'_>) {
    const PLAINTEXT_BODY: &str = "Hello, World!";
    const PLAINTEXT_BODY_SIZE: &str = "13";
    #[rustfmt::skip]
    const PLAINTEXT_HEADERS: &str =
        concats!(HTTP_200_OK, CRLF, SERVER, CRLF, TEXT_PLAIN_CONTENT_TYPE, CRLF, CONTENT_LENGTH, PLAINTEXT_BODY_SIZE, CRLF);

    res.extend(PLAINTEXT_HEADERS);
    res.date_end();
    res.extend(PLAINTEXT_BODY);
}

#[derive(Serialize)]
pub struct Message {
    pub message: &'static str,
}

#[inline]
fn json(res: &mut Response<'_>) {
    const JSON_BODY_SIZE: &str = "27";
    #[rustfmt::skip]
    const JSON_HEADERS: &str =
        concats!(HTTP_200_OK, CRLF, SERVER, CRLF, JSON_CONTENT_TYPE, CRLF, CONTENT_LENGTH, JSON_BODY_SIZE, CRLF);

    res.extend(JSON_HEADERS);
    res.date_end();
    Message {
        message: "Hello, World!",
    }
    .to_bytes_mut(&mut res.buf);
}

#[inline]
fn err_404(res: &mut Response<'_>) {
    res.extend(HTTP_404_NOTFOUND);
}

#[cb]
async fn c<'a>(req: Request<'a>, _s: &Empty, res: &mut Response<'a>) {
    match req.method() {
        GET => match req.path() {
            "/plaintext" => plaintext(res),
            "/json" => json(res),
            _ => err_404(res),
        },
        _ => err_404(res),
    };
}

fn main() {
    faf::with_default().go_empty(c);
}
