use std::collections::HashMap;
use thruster::{Context, Request, Response};

pub struct Ctx {
    pub body: String,
    pub method: String,
    pub path: String,
    pub request_body: String,
    pub params: HashMap<String, String>,
    pub query_params: HashMap<String, String>,
    pub headers: Vec<(String, String)>,
    pub status_code: u32,
}

impl Context for Ctx {
    fn get_response(&self) -> Response<String> {
        let mut builder = Response::builder();
        builder.status(200);

        for header_pair in &self.headers {
            let key: &str = &header_pair.0;
            let val: &str = &header_pair.1;
            builder.header(key, val);
        }

        builder.body(self.body.clone()).unwrap()
    }

    fn set_body(&mut self, body: String) {
        self.body = body;
    }
}

pub fn generate_context(request: Request) -> Ctx {
    let method = request.method().to_owned();
    let path = request.path().to_owned();
    let request_body = request.raw_body().to_owned();

    Ctx {
        body: "".to_owned(),
        method: method,
        path: path,
        params: request.params,
        query_params: request.query_params,
        request_body: request_body,
        headers: Vec::new(),
        status_code: 200,
    }
}
