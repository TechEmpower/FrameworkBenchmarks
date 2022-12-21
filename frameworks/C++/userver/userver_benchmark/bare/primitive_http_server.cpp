#include "primitive_http_server.hpp"

#include <userver/components/component_context.hpp>
#include <userver/engine/io/socket.hpp>

#include "../common/db_helpers.hpp"

#include "../controllers/cached_queries/handler.hpp"
#include "../controllers/json/handler.hpp"
#include "../controllers/multiple_queries/handler.hpp"
#include "../controllers/plaintext/handler.hpp"
#include "../controllers/single_query/handler.hpp"
#include "../controllers/updates/handler.hpp"

#include "primitive_http_connection.hpp"

namespace userver_techempower::bare {

namespace {

constexpr std::string_view kPlainTextUrlPrefix{"/plaintext"};
constexpr std::string_view kJsontUrlPrefix{"/json"};
constexpr std::string_view kSingleQueryUrlPrefix{"/db"};
constexpr std::string_view kMultipleQueriesUrlPrefix{"/queries"};
constexpr std::string_view kUpdatesUrlPrefix{"/updates"};
constexpr std::string_view kCachedQueriesUrlPrefix{"/cached-queries"};

bool StartsWith(std::string_view source, std::string_view pattern) {
  return source.substr(0, pattern.length()) == pattern;
}

}  // namespace

PrimitiveHttpServer::PrimitiveHttpServer(
    const userver::components::ComponentConfig& config,
    const userver::components::ComponentContext& context)
    : userver::components::TcpAcceptorBase(config, context),
      single_query_{context.FindComponent<single_query::Handler>()},
      multiple_queries_{context.FindComponent<multiple_queries::Handler>()},
      updates_{context.FindComponent<updates::Handler>()},
      cached_queries_{context.FindComponent<cached_queries::Handler>()} {}

PrimitiveHttpServer::~PrimitiveHttpServer() = default;

void PrimitiveHttpServer::ProcessSocket(userver::engine::io::Socket&& socket) {
  const auto fd = socket.Fd();
  connections_[fd] =
      std::make_unique<PrimitiveHttpConnection>(*this, std::move(socket));
}

PrimitiveHttpServer::Response PrimitiveHttpServer::HandleRequest(
    std::string_view url) const {
  if (StartsWith(url, kPlainTextUrlPrefix)) {
    return {plaintext::Handler::GetResponse(), "text/plain"};
  }

  if (StartsWith(url, kJsontUrlPrefix)) {
    return {userver::formats::json::ToString(json::Handler::GetResponse()),
            "application/json"};
  }

  if (StartsWith(url, kSingleQueryUrlPrefix)) {
    return {userver::formats::json::ToString(single_query_.GetResponse()),
            "application/json"};
  }

  if (StartsWith(url, kMultipleQueriesUrlPrefix)) {
    const auto queries = db_helpers::ParseParamFromQuery(
        url.substr(kMultipleQueriesUrlPrefix.size()), "queries");

    return {userver::formats::json::ToString(
                multiple_queries_.GetResponse(queries)),
            "application/json"};
  }

  if (StartsWith(url, kUpdatesUrlPrefix)) {
    const auto queries = db_helpers::ParseParamFromQuery(
        url.substr(kMultipleQueriesUrlPrefix.size()), "queries");

    return {userver::formats::json::ToString(updates_.GetResponse(queries)),
            "application/json"};
  }

  if (StartsWith(url, kCachedQueriesUrlPrefix)) {
    const auto count = db_helpers::ParseParamFromQuery(
        url.substr(kCachedQueriesUrlPrefix.size()), "count");

    return {
        userver::formats::json::ToString(cached_queries_.GetResponse(count)),
        "application/json"};
  }

  return {};
}

}  // namespace userver_techempower::bare