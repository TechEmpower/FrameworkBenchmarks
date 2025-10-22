use saphir::responder::Responder;
use sailfish::TemplateOnce;
use crate::models::Fortune;

#[derive(TemplateOnce)]
#[template(path = "fortune.stpl")]
pub struct FortunesTemplate {
    fortunes: Vec<Fortune>,
}

impl FortunesTemplate {
    pub fn new(fortunes: Vec<Fortune>) -> Self {
        Self { fortunes }
    }
}

impl Responder for FortunesTemplate {
    fn respond_with_builder(self, builder: saphir::prelude::Builder, _ctx: &saphir::http_context::HttpContext) -> saphir::prelude::Builder {
        match self.render_once() {
            Ok(r) => builder.body(r).header("Content-Type", "text/html; charset=utf-8"),
            Err(_) => builder.status(500),
        }
    }
}