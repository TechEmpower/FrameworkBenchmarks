#include "simple_router.hpp"

#include <userver/components/component_context.hpp>

#include "../controllers/cached_queries/handler.hpp"
#include "../controllers/json/handler.hpp"
#include "../controllers/multiple_queries/handler.hpp"
#include "../controllers/plaintext/handler.hpp"
#include "../controllers/single_query/handler.hpp"
#include "../controllers/updates/handler.hpp"

namespace userver_techempower::bare {

namespace {

constexpr std::string_view kPlainTextUrlPrefix{"/plaintext"};
constexpr std::string_view kJsontUrlPrefix{"/json"};
constexpr std::string_view kSingleQueryUrlPrefix{"/db"};
constexpr std::string_view kMultipleQueriesUrlPrefix{"/queries"};
constexpr std::string_view kUpdatesUrlPrefix{"/updates"};
constexpr std::string_view kCachedQueriesUrlPrefix{"/cached-queries"};

// NOLINTNEXTLINE
const std::string kContentTypePlain{"text/plain"};
// NOLINTNEXTLINE
const std::string kContentTypeJson{"application/json"};

bool StartsWith(std::string_view source, std::string_view pattern) {
  return source.substr(0, pattern.length()) == pattern;
}

}  // namespace

SimpleRouter::SimpleRouter(const userver::components::ComponentConfig& config,
                           const userver::components::ComponentContext& context)
    : userver::components::LoggableComponentBase{config, context},
      single_query_{context.FindComponent<single_query::Handler>()},
      multiple_queries_{context.FindComponent<multiple_queries::Handler>()},
      updates_{context.FindComponent<updates::Handler>()},
      cached_queries_{context.FindComponent<cached_queries::Handler>()} {}

SimpleRouter::~SimpleRouter() = default;

SimpleResponse SimpleRouter::RouteRequest(std::string_view url) const {
  if (StartsWith(url, kPlainTextUrlPrefix)) {
    return {plaintext::Handler::GetResponse(), kContentTypePlain};
  }

  if (StartsWith(url, kJsontUrlPrefix)) {
    return {ToString(json::Handler::GetResponse()), kContentTypeJson};
  }

  if (StartsWith(url, kSingleQueryUrlPrefix)) {
    return {ToString(single_query_.GetResponse()), kContentTypeJson};
  }

  if (StartsWith(url, kMultipleQueriesUrlPrefix)) {
    const auto queries = db_helpers::ParseParamFromQuery(
        url.substr(kMultipleQueriesUrlPrefix.size()), "queries");

    return {ToString(multiple_queries_.GetResponse(queries)), kContentTypeJson};
  }

  if (StartsWith(url, kUpdatesUrlPrefix)) {
    const auto queries = db_helpers::ParseParamFromQuery(
        url.substr(kMultipleQueriesUrlPrefix.size()), "queries");

    return {ToString(updates_.GetResponse(queries)), kContentTypeJson};
  }

  if (StartsWith(url, kCachedQueriesUrlPrefix)) {
    const auto count = db_helpers::ParseParamFromQuery(
        url.substr(kCachedQueriesUrlPrefix.size()), "count");

    return {ToString(cached_queries_.GetResponse(count)), "application/json"};
  }

  throw std::runtime_error{"No handler found for url"};
}

}  // namespace userver_techempower::bare
