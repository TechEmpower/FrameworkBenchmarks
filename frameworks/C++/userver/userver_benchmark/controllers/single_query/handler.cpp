#include "handler.hpp"

#include <userver/components/component_context.hpp>
#include <userver/http/common_headers.hpp>
#include <userver/storages/postgres/postgres.hpp>

namespace userver_techempower::single_query {

namespace {

constexpr std::size_t kBestConcurrencyWildGuess = 256;

}

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerBase{config, context},
      pg_{context
              .FindComponent<userver::components::Postgres>(
                  db_helpers::kDbComponentName)
              .GetCluster()},
      semaphore_{kBestConcurrencyWildGuess} {}

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  request.GetHttpResponse().SetHeader(userver::http::headers::kContentType,
                                      "application/json");
  return GetResponse();
}

std::string Handler::GetResponse() const {
  const auto row = [this] {
    const auto lock = semaphore_.Acquire();
    return pg_
        ->Execute(db_helpers::kClusterHostType, db_helpers::kDefaultPgCC,
                  db_helpers::kSelectRowQuery, db_helpers::GenerateRandomId())
        .AsSingleRow<db_helpers::WorldTableRow>(
            userver::storages::postgres::kRowTag);
  }();

  userver::formats::json::StringBuilder sb{};
  WriteToStream(row, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::single_query
