use anansi::{check, viewer, render};
use anansi::web::{Result, Response};
use anansi::util::auth::records::Group;
use crate::project::Request;

#[viewer]
impl<R: Request> ErrorView<R> {
    #[check(Group::is_visitor)]
    pub async fn not_found(_req: &mut R) -> Result<Response> {
        render!("not_found")
    }
}
