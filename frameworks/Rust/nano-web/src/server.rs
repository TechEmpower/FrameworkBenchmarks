// anyhow::Result - Provides a convenient Result<T, anyhow::Error> type alias
// Much nicer than Result<T, Box<dyn std::error::Error>> for error handling
use anyhow::Result;
use axum::{
    // State extractor - Axum's way to inject shared state into handler functions
    extract::State,
    // HTTP primitives - StatusCode is an enum, HeaderMap is HashMap-like for headers
    http::{header, HeaderMap, StatusCode, Uri},
    // Trait that converts types into HTTP responses (implemented for tuples, strings, etc)
    response::IntoResponse,
    // Routing - get() creates a route that only handles GET requests
    routing::get,
    Router,
};
use chrono::Utc;
// PathBuf owns path data (vs &Path which borrows) - we need owned data for the config struct
use std::path::PathBuf;
// Arc for thread-safe sharing of NanoWeb across all HTTP request handlers
use std::sync::Arc;
// TcpListener for async TCP socket binding in Tokio
use tokio::net::TcpListener;
// Tower middleware system - composable request/response processing
use tower::ServiceBuilder;
use tower_http::{set_header::SetResponseHeaderLayer, trace::TraceLayer};
use tracing::{debug, info};

use crate::routes::NanoWeb;

// #[derive(Clone)] - Automatically generates clone() method that clones each field
// We need Clone because Axum requires state to be cloneable (shared across request handlers)
#[derive(Clone)]
pub struct AxumServeConfig {
    pub public_dir: PathBuf, // Owned path - could be moved between threads
    pub port: u16,           // u16 is Copy (stack allocated) - clone just copies the bits
    pub dev: bool,           // bool is Copy - no heap allocation to worry about
    pub spa_mode: bool,
    pub config_prefix: String, // String owns heap-allocated data - clone() allocates new string
    pub log_requests: bool,
}

// AppState gets cloned for every HTTP request handler (Axum requirement)
// Arc<NanoWeb> clone is cheap (just increments reference count)
// AxumServeConfig clone is more expensive (String allocation) but happens per-request
#[derive(Clone)]
struct AppState {
    server: Arc<NanoWeb>,    // Shared read-only access to our route cache
    config: AxumServeConfig, // Configuration that handlers might need
}

// async fn - Returns a Future that implements the async state machine
// Result<()> - Returns () on success, anyhow::Error on failure
pub async fn start_axum_server(config: AxumServeConfig) -> Result<()> {
    // Arc::new allocates NanoWeb on the heap and wraps in atomic reference counter
    // Needed because multiple concurrent request handlers will access the same cache
    let server = Arc::new(NanoWeb::new());

    // Populate routes using our existing route system
    // &config.public_dir - borrowing because populate_routes doesn't need ownership
    server.populate_routes(&config.public_dir, &config.config_prefix)?;

    let state = AppState {
        server,                 // Arc<NanoWeb> moved into state
        config: config.clone(), // Clone needed because we use config again below (line 65)
    };

    info!("Routes loaded: {}", state.server.routes.len());

    let app = create_router(state);

    // format! macro - creates owned String by formatting template
    let addr = format!("0.0.0.0:{}", config.port);
    // TcpListener::bind is async - returns Future<Result<TcpListener, Error>>
    // .await suspends this function until the future completes
    // ? operator propagates any bind errors up to caller
    let listener = TcpListener::bind(&addr).await?;

    info!("Starting server on http://{}", addr);
    info!("Serving directory: {:?}", config.public_dir);

    // axum::serve takes ownership of listener and app, runs until server shuts down
    // This is the main server loop that handles all incoming HTTP connections
    axum::serve(listener, app).await?;

    // Only reached when server shuts down gracefully
    Ok(())
}

// Takes AppState by value (ownership) - Router needs to own the state for sharing
fn create_router(state: AppState) -> Router {
    // ServiceBuilder uses the builder pattern to compose middleware layers
    // Each .layer() call wraps the next layer - processed in reverse order during requests
    let middleware_stack = ServiceBuilder::new()
        // Security headers - SetResponseHeaderLayer adds these to every response
        .layer(SetResponseHeaderLayer::overriding(
            header::X_CONTENT_TYPE_OPTIONS,
            // .parse() converts &str to HeaderValue, .unwrap() panics on invalid headers
            // Safe because these are known-good header values
            "nosniff".parse::<axum::http::HeaderValue>().unwrap(),
        ))
        .layer(SetResponseHeaderLayer::overriding(
            header::X_FRAME_OPTIONS,
            "SAMEORIGIN".parse::<axum::http::HeaderValue>().unwrap(),
        ))
        .layer(SetResponseHeaderLayer::overriding(
            header::REFERRER_POLICY,
            "strict-origin-when-cross-origin"
                .parse::<axum::http::HeaderValue>()
                .unwrap(),
        ));

    // Router::new() creates empty router, then we build it with method chaining
    let app = Router::new()
        // .route() maps HTTP path patterns to handler functions
        // get() means only handle GET requests to this path
        .route("/_health", get(health_handler)) // health_handler is a function pointer
        .route("/", get(root_handler)) // Explicit route for root
        .fallback(get(file_handler)); // Catch-all for everything else

    // Conditional middleware - only add tracing if log_requests is true
    if state.config.log_requests {
        app.layer(
            // TraceLayer automatically creates tracing spans for HTTP requests
            TraceLayer::new_for_http()
                // Closure that creates a span for each request
                // |request: &Request| - closure parameter with explicit type
                .make_span_with(|request: &axum::extract::Request| {
                    // tracing::info_span! macro creates a span with key=value fields
                    // % means "use Display formatting" for the value
                    tracing::info_span!(
                        "request",
                        method = %request.method(),
                        path = %request.uri().path(),
                    )
                })
                // Closure called when response is ready
                // Multiple parameters in closure - latency calculated by TraceLayer
                .on_response(
                    |response: &axum::response::Response,
                     latency: std::time::Duration,
                     _span: &tracing::Span| {
                        // _span prefix means "unused parameter"
                        tracing::info!(
                            status = %response.status(),
                            duration_ms = %latency.as_millis(),
                            "request completed"
                        );
                    },
                ),
        )
        .layer(middleware_stack) // Add our security headers
        .with_state(state) // Inject AppState into all handlers
    } else {
        // Same router but without tracing middleware
        app.layer(middleware_stack).with_state(state)
    }
}

// impl IntoResponse - returns "some type that implements IntoResponse"
// This lets us return different types (tuples, strings, etc.) from the same function
// Axum provides IntoResponse impls for many types including tuples
async fn health_handler() -> impl IntoResponse {
    let timestamp = Utc::now().to_rfc3339(); // ISO 8601 timestamp string
                                             // r#"..."# is a raw string literal - no need to escape quotes inside
    let response = format!(r#"{{"status":"ok","timestamp":"{}"}}"#, timestamp);
    // Tuple that implements IntoResponse: (StatusCode, Headers, Body)
    // Array syntax [(key, value)] creates headers - more ergonomic than HeaderMap
    (
        StatusCode::OK,
        [(header::CONTENT_TYPE, "application/json")],
        response, // String implements IntoResponse as the response body
    )
}

// Axum handler function - parameters are "extractors" that Axum provides
// headers: HeaderMap - Axum extracts request headers automatically
// State(state): State<AppState> - Pattern matching to extract our injected state
async fn root_handler(headers: HeaderMap, State(state): State<AppState>) -> impl IntoResponse {
    // Delegate to serve_file with root path
    // .await because serve_file is async and we need its result
    serve_file("/".to_string(), headers, state).await
}

async fn file_handler(
    uri: Uri, // Uri extractor - Axum parses the request URI
    headers: HeaderMap,
    State(state): State<AppState>,
) -> impl IntoResponse {
    // Extract path from URI and convert to owned String
    // .to_string() because serve_file needs owned data (not borrowed &str)
    let path = uri.path().to_string();
    serve_file(path, headers, state).await
}

async fn serve_file(
    path: String, // Owned string - can be moved around without lifetime issues
    request_headers: HeaderMap,
    state: AppState, // AppState by value - cloned by Axum for each request
) -> impl IntoResponse {
    debug!("Serving path: {}", path);

    // Security: validate path to prevent directory traversal attacks
    // if let Err(e) - pattern matching on Result, only execute if validation failed
    if let Err(e) = crate::path::validate_request_path(&path) {
        tracing::warn!("Path validation failed for '{}': {}", path, e);
        // Early return with error response - into_response() converts tuple to Response
        return (StatusCode::BAD_REQUEST, "Bad Request").into_response();
    }

    // Route lookup using our existing system
    // mut because we might reassign it in the fallback logic below
    let mut route = state.server.get_route(&path);

    // Fallback 1: Try with trailing slash (common web server behavior)
    if route.is_none() && !path.ends_with('/') {
        // format! creates new owned String - needed because get_route expects &str
        let path_with_slash = format!("{}/", path);
        route = state.server.get_route(&path_with_slash);
    }

    // Fallback 2: SPA mode - serve index.html for any unmatched routes
    if route.is_none() && state.config.spa_mode {
        // Single Page App fallback - serve root for all unmatched routes
        route = state.server.get_route("/");
        if route.is_some() {
            debug!("SPA fallback for: {}", path);
        }
    }

    // Pattern match on Option<CachedRoute> - extract route or return 404
    // This converts Option<CachedRoute> to CachedRoute or early returns
    let route = match route {
        Some(r) => r, // Extract the CachedRoute from Some(route)
        None => {
            debug!("Route not found: {}", path);
            // Early return - convert tuple to HTTP response and exit function
            return (StatusCode::NOT_FOUND, "Not Found").into_response();
        }
    };

    // Dev mode file refresh - check if file changed on disk and reload if needed
    // Variable shadowing: creating new `route` variable that shadows the previous one
    let route = if state.config.dev {
        // refresh_if_modified returns Result<Option<CachedRoute>, Error>
        match state
            .server
            .refresh_if_modified(&path, &state.config.config_prefix)
        {
            Ok(Some(updated_route)) => {
                debug!("Route refreshed: {}", path);
                updated_route // Use the newly loaded route
            }
            Ok(None) => route, // File unchanged, use existing route
            Err(e) => {
                debug!("Failed to refresh route {}: {}", path, e);
                route // Error refreshing, fallback to existing route
            }
        }
    } else {
        route // Production mode - always use cached route
    };

    // Extract Accept-Encoding from request headers for our compression system
    // Method chaining with Option combinators - common Rust pattern
    let accept_encoding = request_headers
        .get(header::ACCEPT_ENCODING) // Option<&HeaderValue>
        .and_then(|h| h.to_str().ok()) // Option<&str> - convert HeaderValue to str
        .unwrap_or(""); // &str - default to empty string if None

    // Use our compression system with pre-computed compressed files
    // Destructuring assignment - get_best_encoding returns a tuple (String, &[u8])
    let (encoding, content) = route.content.get_best_encoding(accept_encoding);

    // Build response with optimized headers
    // HeaderMap::new() creates empty header map - mut because we'll modify it
    let mut response_headers = HeaderMap::new();

    // Insert headers from our cached route
    // .parse() converts Arc<str> to HeaderValue, .unwrap() safe because we control the values
    response_headers.insert(
        header::CONTENT_TYPE,
        route.headers.content_type.parse().unwrap(), // "text/html", "application/json", etc
    );
    response_headers.insert(
        header::LAST_MODIFIED,
        route.headers.last_modified.parse().unwrap(), // "Wed, 15 Nov 2023 08:12:31 GMT"
    );
    response_headers.insert(header::ETAG, route.headers.etag.parse().unwrap()); // "abc123-456"
    response_headers.insert(
        header::CACHE_CONTROL,
        route.headers.cache_control.parse().unwrap(), // "public, max-age=31536000"
    );

    // Add compression encoding header if content is compressed
    // "identity" means uncompressed - only add header for gzip, br, etc
    if encoding != "identity" {
        response_headers.insert(header::CONTENT_ENCODING, encoding.parse().unwrap());
    }

    debug!(
        "Serving {} bytes with encoding: {} (from pre-compressed cache)",
        content.len(), // &[u8] has .len() method
        encoding
    );

    // Final response tuple: (StatusCode, HeaderMap, Body)
    // content.clone() - cloning &[u8] is cheap, just copies the slice reference
    // Arc<Vec<u8>> inside CompressedContent makes the actual data sharing cheap
    (StatusCode::OK, response_headers, content.clone()).into_response()
}
