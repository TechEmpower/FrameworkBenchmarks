use rocket::request::{self, Request, FromRequest, Outcome};
use std::sync::atomic::{AtomicI32, Ordering};

/// A global atomic counter for generating IDs.
static ID_COUNTER: AtomicI32 = AtomicI32::new(0);

/// A type that represents a request's ID.
#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub struct RequestId(pub i32);

/// Returns the current request's ID, assigning one only as necessary.
#[rocket::async_trait]
impl<'r> FromRequest<'r> for RequestId {
    type Error = ();

    async fn from_request(request: &'r Request<'_>) -> Outcome<Self, Self::Error> {
        // The closure passed to `local_cache` will be executed at most once per
             // request: the first time the `RequestId` guard is used. If it is
             // requested again, `local_cache` will return the same value.
             request::Outcome::Success(*request.local_cache(|| {
                 RequestId(ID_COUNTER.fetch_add(1, Ordering::Relaxed))
             }))
    }
}