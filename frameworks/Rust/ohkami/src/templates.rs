use ohkami::{Response, IntoResponse};
use crate::models::Fortune;


#[derive(yarte::Template)]
#[template(path="fortunes")]
pub struct FortunesTemplate {
    pub fortunes: Vec<Fortune>,
}
impl IntoResponse for FortunesTemplate {
    #[inline(always)]
    fn into_response(self) -> Response {
        ohkami::utils::HTML(
            <Self as yarte::Template>::call(&self)
                .expect("Failed to render fortunes template")
        ).into_response()
    }
}
