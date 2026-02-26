// HTTP Client Example
//
// This example demonstrates using the HTTP client to make requests.
//
// Build and run:
//   zig build-exe examples/http_client.zig -Mroot=src/http.zig && ./http_client

const std = @import("std");
const http = @import("http");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Create client with default configuration
    // tls_insecure = false means we verify certificates using system CA bundle
    var client = http.Client.init(allocator, .{
        .timeout_ms = 10_000,
        .max_redirects = 5,
        .follow_redirects = true,
        .tls_insecure = false, // Verify certificates (default)
    });
    defer client.deinit();

    // Example 1: Simple GET request
    std.debug.print("=== GET Request ===\n", .{});
    {
        var response = client.get("http://httpbin.org/get") catch |err| {
            std.debug.print("Request failed: {}\n", .{err});
            return;
        };
        defer response.deinit();

        std.debug.print("Status: {d} {s}\n", .{ response.status.code(), response.status.phrase() });
        std.debug.print("Content-Type: {s}\n", .{response.headers.get("Content-Type") orelse "unknown"});
        std.debug.print("Body length: {d} bytes\n", .{response.body.len});

        // Print first 200 chars of body
        const preview_len = @min(response.body.len, 200);
        std.debug.print("Body preview: {s}...\n\n", .{response.body[0..preview_len]});
    }

    // Example 2: POST request with JSON body
    std.debug.print("=== POST Request with JSON ===\n", .{});
    {
        var response = client.post("http://httpbin.org/post", .{
            .body = "{\"name\": \"zig-http\", \"version\": \"1.0\"}",
            .content_type = "application/json",
        }) catch |err| {
            std.debug.print("Request failed: {}\n", .{err});
            return;
        };
        defer response.deinit();

        std.debug.print("Status: {d} {s}\n", .{ response.status.code(), response.status.phrase() });
        std.debug.print("Body length: {d} bytes\n\n", .{response.body.len});
    }

    // Example 3: Request builder with custom headers
    std.debug.print("=== Request Builder with Headers ===\n", .{});
    {
        var builder = http.request(&client, .GET, "http://httpbin.org/headers");
        var response = builder
            .header("X-Custom-Header", "custom-value")
            .header("Accept", "application/json")
            .send() catch |err| {
            std.debug.print("Request failed: {}\n", .{err});
            return;
        };
        defer response.deinit();

        std.debug.print("Status: {d} {s}\n", .{ response.status.code(), response.status.phrase() });

        const preview_len = @min(response.body.len, 300);
        std.debug.print("Body: {s}\n\n", .{response.body[0..preview_len]});
    }

    // Example 4: Redirect handling
    std.debug.print("=== Redirect Handling ===\n", .{});
    {
        // httpbin.org/redirect/2 redirects twice before returning
        var response = client.get("http://httpbin.org/redirect/2") catch |err| {
            std.debug.print("Request failed: {}\n", .{err});
            return;
        };
        defer response.deinit();

        std.debug.print("Final Status: {d} {s}\n", .{ response.status.code(), response.status.phrase() });
        std.debug.print("(Followed redirects automatically)\n\n", .{});
    }

    // Example 5: HTTPS request (TLS)
    std.debug.print("=== HTTPS Request (TLS) ===\n", .{});
    {
        var response = client.get("https://httpbin.org/get") catch |err| {
            std.debug.print("HTTPS request failed: {}\n", .{err});
            std.debug.print("(This is expected if TLS handshake fails)\n\n", .{});
            std.debug.print("All examples completed!\n", .{});
            return;
        };
        defer response.deinit();

        std.debug.print("Status: {d} {s}\n", .{ response.status.code(), response.status.phrase() });
        std.debug.print("Body length: {d} bytes\n", .{response.body.len});

        const preview_len = @min(response.body.len, 200);
        std.debug.print("Body preview: {s}...\n\n", .{response.body[0..preview_len]});
    }

    std.debug.print("All examples completed!\n", .{});

    // Clean up cached CA bundle to avoid leak warnings during testing
    http.client.deinitCaBundle();
}
