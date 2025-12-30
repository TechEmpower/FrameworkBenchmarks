#![cfg(feature = "rt_tokio")]

use ohkami::{IntoResponse, Response};
use yarte::Template;
use crate::models::Fortune;

#[derive(Template)]
#[template(src = r#"
<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
    {{~# each fortunes ~}}
    <tr><td>{{id}}</td><td>{{message}}</td></tr>
    {{~/each ~}}
</table></body></html>
"#)]
pub struct FortunesTemplate {
    pub fortunes: Vec<Fortune>,
}

impl IntoResponse for FortunesTemplate {
    #[inline]
    fn into_response(self) -> Response {
        match Template::call(&self) {
            Ok(template) => Response::OK().with_html(template),
            Err(_)       => Response::InternalServerError(),
        }
    }
}
