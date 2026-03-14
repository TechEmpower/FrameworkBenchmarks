// SPDX-License-Identifier: MIT
// TechEmpower Framework Benchmarks - Aeronet Implementation
// Implements Test 1 (JSON) and Test 6 (Plaintext) endpoints

#include <aeronet/aeronet.hpp>
#include <aeronet/json-serializer.hpp>
#include <charconv>
#include <cstdlib>
#include <cstring>
#include <glaze/glaze.hpp>
#include <iostream>
#include <optional>
#include <system_error>

// Test 1: JSON message structure
struct MessageResponse {
  std::string_view message;
};

// Glaze metadata for JSON serialization
template <>
struct glz::meta<MessageResponse> {
  using T = MessageResponse;
  static constexpr auto value = glz::object("message", &T::message);
};

int main(int argc, char* argv[]) {
  // Enable signal handler for graceful shutdown on Ctrl+C
  aeronet::SignalHandler::Enable();

  try {
    using namespace aeronet;

    auto parseEnvUInt = [](const char* envVar) -> std::optional<uint32_t> {
      const char* value = std::getenv(envVar);
      if (value == nullptr || value[0] == '\0') {
        return std::nullopt;
      }
      uint32_t parsed = 0;
      const auto [ptr, errc] = std::from_chars(value, value + std::strlen(value), parsed);
      if (errc != std::errc{} || ptr != value + std::strlen(value)) {
        return std::nullopt;
      }
      return parsed;
    };

    uint16_t port = 8080;
    if (argc > 1) {
      const auto [ptr, errc] = std::from_chars(argv[1], argv[1] + std::strlen(argv[1]), port);
      if (errc != std::errc{} || ptr != argv[1] + std::strlen(argv[1])) {
        std::cerr << "Invalid port number: " << argv[1] << "\n";
        return EXIT_FAILURE;
      }
    }

    HttpServerConfig config;
    config.port = port;
    if (const auto threads = parseEnvUInt("AERONET_THREADS"); threads.has_value()) {
      config.nbThreads = *threads;
    } else if (const auto threads = parseEnvUInt("THREADS"); threads.has_value()) {
      config.nbThreads = *threads;
    }

    Router router;

    // Test 6: Plaintext endpoint
    // Returns a simple "Hello, World!" text response
    router.setPath(http::Method::GET, "/plaintext", [](const HttpRequest& req) {
      return req.makeResponse("Hello, World!", "text/plain; charset=UTF-8");
    });

    // Test 1: JSON endpoint
    router.setPath(http::Method::GET, "/json", [](const HttpRequest& req) {
      MessageResponse msg{"Hello, World!"};
      return req.makeResponse(aeronet::SerializeToJson(msg), "application/json");
    });

    // Health check endpoint (optional, useful for Docker)
    router.setPath(http::Method::GET, "/health",
                   [](const HttpRequest& req) { return req.makeResponse("OK", "text/plain"); });

    // Start the server
    std::cout << "Starting TechEmpower benchmark server on port " << port << '\n';
    std::cout << "  - JSON test (Test 1): GET /json\n";
    std::cout << "  - Plaintext test (Test 6): GET /plaintext\n";
    std::cout << "  - Health check: GET /health\n";

    HttpServer server(config, std::move(router));
    server.run();  // blocking run, until Ctrl+C

  } catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << '\n';
    return 1;
  }

  return 0;
}
