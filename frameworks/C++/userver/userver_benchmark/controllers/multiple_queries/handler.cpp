#include "handler.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/http/common_headers.hpp>
#include <userver/storages/postgres/postgres.hpp>

#include <boost/container/small_vector.hpp>

namespace userver_techempower::multiple_queries {

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
      query_arg_name_{"queries"},
      semaphore_{kBestConcurrencyWildGuess} {}

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  const auto queries =
      db_helpers::ParseParamFromQuery(request, query_arg_name_);

  request.GetHttpResponse().SetHeader(userver::http::headers::kContentType,
                                      "application/json");
  return GetResponse(queries);
}

std::string Handler::GetResponse(int queries) const {
  const auto db_result = [this, queries] {
    const auto lock = semaphore_.Acquire();

    auto query_queue = pg_->CreateQueryQueue(db_helpers::kClusterHostType,
                                             db_helpers::kDefaultPgCC.execute);
    query_queue.Reserve(queries);
    for (std::size_t i = 0; i < static_cast<std::size_t>(queries); ++i) {
      query_queue.Push(db_helpers::kDefaultPgCC, db_helpers::kSelectRowQuery,
                       db_helpers::GenerateRandomId());
    }

    return query_queue.Collect(db_helpers::kDefaultPgCC.execute);
  }();

  boost::container::small_vector<db_helpers::WorldTableRow, 20> result(queries);
  for (std::size_t i = 0; i < static_cast<std::size_t>(queries); ++i) {
    result[i] = db_result[i].AsSingleRow<db_helpers::WorldTableRow>(
        userver::storages::postgres::kRowTag);
  }

  userver::formats::json::StringBuilder sb{};
  WriteToStream(result, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::multiple_queries
