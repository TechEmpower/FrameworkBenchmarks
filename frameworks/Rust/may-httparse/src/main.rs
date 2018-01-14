extern crate may;
extern crate httparse;
extern crate bytes;
extern crate time;
#[macro_use] extern crate serde_json;

use may::coroutine;
use may::net::TcpListener;
use httparse::Status;
use bytes::BufMut;
use std::io::{Read, Write};

fn req_parse(buf: &[u8], path: &mut String) -> Option<usize> {
    let mut headers = [httparse::EMPTY_HEADER; 16];
    let mut req = httparse::Request::new(&mut headers);

    if let Ok(Status::Complete(i)) = req.parse(buf) {
        path.clear();
        path.push_str(req.path.unwrap_or("/"));
        return Some(i);
    }

    None
}

fn main() {
    may::config().set_io_workers(4);

    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();
    while let Ok((mut stream, _)) = listener.accept() {
        coroutine::spawn(move || {
            let mut buf = Vec::new();
            let mut path = String::new();

            loop {
                let json = json!({
                    "message": "Hello, World!"
                }).to_string();

                if let Some(i) = req_parse(&buf, &mut path) {
                    let response = match &*path {
                        "/plaintext" => "Hello, World!",
                        "/json" => &json,
                        _ => "Cannot find page"
                    };

                    let s = format!("\
                        HTTP/1.1 200 OK\r\n\
                        Server: May\r\n\
                        Date: {}\r\n\
                        Content-Length: {}\r\n\
                        \r\n\
                        {}", time::now().rfc822(), response.len(), response);

                    stream.write_all(s.as_bytes())
                          .expect("Cannot write to socket");
                    
                    buf = buf.split_off(i);
                } else {
                    let mut temp_buf = vec![0; 512];
                    match stream.read(&mut temp_buf) {
                        Ok(0) => return, // connection was closed
                        Ok(n) => {
                            buf.put(&temp_buf[0..n]);
                        },
                        Err(err) => println!("err = {:?}", err),
                    }
                }
            }
        });
    }
}
