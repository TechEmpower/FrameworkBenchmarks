use smallvec::SmallVec;
use std::collections::HashMap;
use thruster::{Context, Request, Response};

pub struct Ctx {
    pub body: String,
    pub method: String,
    pub path: String,
    pub request_body: String,
    pub params: HashMap<String, String>,
    pub query_params: HashMap<String, String>,
    pub headers: SmallVec<[(String, String); 8]>,
    pub status_code: u32,
}

impl Ctx {
    pub fn new(context: Ctx) -> Ctx {
        Ctx {
            body: context.body,
            method: context.method,
            path: context.path,
            params: context.params,
            query_params: context.query_params,
            request_body: context.request_body,
            headers: SmallVec::new(),
            status_code: 200,
        }
    }

    pub fn set_header(&mut self, key: String, val: String) {
        self.headers.push((key, val));
    }
}

impl Context for Ctx {
    fn get_response(&self) -> Response {
        let mut response = Response::new();
        response.status_code(200, "OK");

        for header_pair in &self.headers {
            let key: &str = &header_pair.0;
            let val: &str = &header_pair.1;
            response.header(key, val);
        }

        response.body(&self.body);

        response
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
        headers: SmallVec::new(),
        status_code: 200,
    }
}
