//! Middleware that applies to mane requests at the HTTP level.

use tide::{Middleware, Next, Request, Result};

/// Middleware to add the `server` header to each response.
#[derive(Debug, Default, Clone)]
pub struct ServerHeader;

#[async_trait::async_trait]
impl<State: Clone + Send + Sync + 'static> Middleware<State> for ServerHeader {
    async fn handle(&self, request: Request<State>, next: Next<'_, State>) -> Result {
        let mut response = next.run(request).await;
        response.insert_header("server", "tide");
        Ok(response)
    }
}
