use ohkami::{IntoResponse, Response};
use yarte::Template;
use crate::models::Fortune;


#[derive(Template)]
#[template(path="fortunes")]
pub struct FortunesTemplate {
    pub fortunes: Vec<Fortune>,
}
impl IntoResponse for FortunesTemplate {
    fn into_response(self) -> Response {
        match self.call() {
            Ok(template) => Response::OK().html(template),
            Err(error)   => {
                eprintln!("Failed to render template: {error}");
                Response::InternalServerError()
            }
        }
    }
}
