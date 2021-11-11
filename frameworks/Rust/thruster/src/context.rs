use smallvec::SmallVec;
use std::collections::HashMap;
use thruster::{Context, Request, Response};

pub struct Ctx {
    pub body: String,
    pub method: String,
    pub path: String,
    pub request_body: String,
    pub params: HashMap<String, String>,
    pub headers: SmallVec<[(String, String); 8]>,
    pub status_code: u32,
    response: Response,
}

impl Ctx {
    pub fn set_header(&mut self, key: &str, val: &str) {
        self.response.header(key, val);
    }
}

impl Context for Ctx {
    fn get_response(mut self) -> Response {
        self.response.status_code(200, "OK");

        self.response.body(&self.body);

        self.response
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
        request_body: request_body,
        headers: SmallVec::new(),
        status_code: 200,
        response: Response::new(),
    }
}
