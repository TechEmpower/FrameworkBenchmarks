use std::collections::{HashMap};
use fanta::{Context, Request, Response};

pub struct Ctx {
  pub body: String,
  pub method: String,
  pub path: String,
  pub request_body: String,
  pub params: HashMap<String, String>,
  pub headers: Vec<(String, String)>,
  pub status_code: u32
}

impl Ctx {
  pub fn new(context: Ctx) -> Ctx {
    Ctx {
      body: context.body,
      method: context.method,
      path: context.path,
      params: context.params,
      request_body: context.request_body,
      headers: Vec::new(),
      status_code: 200
    }
  }

  pub fn set_header(&mut self, key: &str, val: &str) {
    self.headers.push((key.to_owned(), val.to_owned()));
  }
}

impl Context for Ctx {
  fn get_response(&self) -> Response {
    let mut response = Response::new();
    response.body(&self.body);
    response.status_code(self.status_code, "");

    for header_pair in &self.headers {
      response.header(&header_pair.0, &header_pair.1);
    }

    response
  }

  fn set_body(&mut self, body: String) {
    self.body = body;
  }
}

pub fn generate_context(request: &Request) -> Ctx {
  Ctx {
    body: "".to_owned(),
    method: request.method().to_owned(),
    path: request.path().to_owned(),
    params: request.params().clone(),
    request_body: request.raw_body().to_owned(),
    headers: Vec::new(),
    status_code: 200
  }
}
