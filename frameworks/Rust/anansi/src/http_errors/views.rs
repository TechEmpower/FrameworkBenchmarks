use anansi::{check, viewer, render};
use anansi::web::{Result, Response};
use crate::project::Request;
use anansi::site::Site;

#[viewer]
impl<R: Request> ErrorView<R> {
    #[check(Site::is_visitor)]
    pub async fn not_found(_req: &mut R) -> Result<Response> {
        render!("not_found")
    }
}
