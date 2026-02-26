// oxelot-http: A high-performance async HTTP framework for Zig
//
// Features:
// - io_uring-based async I/O
// - SIMD HTTP parsing (picohttpparser)
// - Path-based routing with parameter extraction
// - JSON serialization helpers
// - WebSocket support
// - Async PostgreSQL client (optional)

const std = @import("std");

// Core types
const request_mod = @import("request.zig");
pub const Request = request_mod.Request;
pub const parseRequest = request_mod.parseRequest;
pub const FormFieldIterator = request_mod.FormFieldIterator;
pub const urlDecode = request_mod.urlDecode;

// Multipart/file upload support
pub const multipart = @import("multipart.zig");
pub const MultipartIterator = multipart.MultipartIterator;
pub const MultipartConfig = multipart.MultipartConfig;
pub const MultipartPart = multipart.Part;
pub const MultipartError = multipart.MultipartError;

// Streaming multipart support
pub const StreamingMultipartParser = multipart.StreamingMultipartParser;
pub const StreamingConfig = multipart.StreamingConfig;
pub const StreamingPart = multipart.StreamingPart;
pub const StreamingError = multipart.StreamingError;
pub const TempFileCleanup = multipart.TempFileCleanup;
pub const Response = @import("response.zig").Response;
pub const CommonResponses = @import("response.zig").CommonResponses;

// Routing
const router_mod = @import("router.zig");
pub const Router = router_mod.Router;
pub const Handler = router_mod.Handler;
pub const Context = router_mod.Context;
pub const WebSocketHandler = router_mod.WebSocketHandler;

// Middleware system
pub const mw = @import("middleware.zig");
pub const Middleware = mw.Middleware;
pub const Next = mw.Next;
pub const MiddlewareChain = mw.MiddlewareChain;

// Built-in middleware
pub const logging = @import("middleware/logging.zig");
pub const cors = @import("middleware/cors.zig");
pub const compression = @import("middleware/compression.zig");
pub const basicAuth = @import("middleware/basic_auth.zig");
pub const rateLimit = @import("middleware/rate_limit.zig");
pub const static = @import("middleware/static.zig");

// Server
const server_mod = @import("server.zig");
pub const Server = server_mod.Server;
pub const ServerConfig = server_mod.Config;
pub const AsyncPollResponseWriter = server_mod.AsyncPollResponseWriter;
pub const DB_POLL_TAG_BIT = server_mod.DB_POLL_TAG_BIT;
pub const getAsyncPollState = server_mod.getAsyncPollState;
pub const getAsyncPollRing = server_mod.getAsyncPollRing;
pub const getCurrentConnPtr = server_mod.getCurrentConnPtr;
pub const getHttpDate = server_mod.getHttpDate;
pub const IoUring = std.os.linux.IoUring;

// HTTP types
pub const Method = @import("method.zig").Method;
pub const Status = @import("status.zig").Status;
pub const Headers = @import("headers.zig").Headers;
pub const ContentType = @import("headers.zig").ContentType;
pub const CommonHeaders = @import("headers.zig").CommonHeaders;

// JSON utilities (original)
pub const json = @import("json.zig");
pub const ObjectBuilder = json.ObjectBuilder;
pub const ArrayBuilder = json.ArrayBuilder;

// SIMD JSON (fast serialization + zimdjson parsing)
pub const simd_json = @import("simd_json.zig");
pub const JsonWriter = simd_json.JsonWriter;
pub const stringify = simd_json.stringify;
pub const stringifyAlloc = simd_json.stringifyAlloc;

// WebSocket support
pub const websocket = @import("websocket.zig");
pub const WebSocketCodec = websocket.Codec;
pub const WebSocketFrame = websocket.Frame;
pub const WebSocketOpcode = websocket.Opcode;
pub const WebSocketConnection = websocket.Connection;
pub const WebSocketCloseCode = websocket.CloseCode;

/// Queue a WebSocket write for io_uring processing
/// Use this for cross-thread WebSocket writes to avoid race conditions with io_uring
pub const queueWebSocketWrite = server_mod.queueWebSocketWrite;

// HTTP Client
pub const client = @import("client.zig");
pub const Client = client.Client;
pub const ClientConfig = client.Config;
pub const ClientResponse = client.Response;
pub const RequestBuilder = client.RequestBuilder;
pub const ConnectionPool = client.ConnectionPool;
pub const PoolConfig = client.PoolConfig;
pub const PoolStats = client.PoolStats;
pub const request = client.request;

// PostgreSQL client (optional - requires libpq)
// Import separately: const pg = @import("pg");
// pub const pg = @import("pg.zig"); // Uncomment if you want it bundled

/// Create a new router
pub fn router(allocator: std.mem.Allocator) Router {
    return Router.init(allocator);
}

/// Create and start a server with the given router
pub fn serve(allocator: std.mem.Allocator, r: *Router, address: []const u8, port: u16) !void {
    var server = Server.init(allocator, r, .{});
    defer server.deinit();
    try server.run(address, port);
}

test {
    _ = @import("request.zig");
    _ = @import("response.zig");
    _ = @import("router.zig");
    _ = @import("method.zig");
    _ = @import("status.zig");
    _ = @import("headers.zig");
    _ = @import("json.zig");
    _ = @import("simd_json.zig");
    _ = @import("websocket.zig");
    _ = @import("multipart.zig");
    _ = @import("middleware.zig");
    _ = @import("middleware/logging.zig");
    _ = @import("middleware/cors.zig");
    _ = @import("middleware/compression.zig");
    _ = @import("middleware/basic_auth.zig");
    _ = @import("middleware/rate_limit.zig");
    _ = @import("middleware/static.zig");
    _ = @import("server.zig");
    _ = @import("client.zig");
    _ = @import("client/pool.zig");
    _ = @import("client/tls.zig");
    // Metrics and tracing are separate modules - their tests are run via separate test targets
}
