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
  boost::container::small_vector<db_helpers::WorldTableRow, 20> result(queries);
  for (auto& value : result) {
    value.id = db_helpers::GenerateRandomId();
  }

  {
    const auto lock = semaphore_.Acquire();

    auto trx =
        pg_->Begin(db_helpers::kClusterHostType, {}, db_helpers::kDefaultPgCC);
    for (auto& value : result) {
      value.random_number = trx.Execute(db_helpers::kDefaultPgCC,
                                        db_helpers::kSelectRowQuery, value.id)
                                .AsSingleRow<db_helpers::WorldTableRow>(
                                    userver::storages::postgres::kRowTag)
                                .random_number;
    }
    trx.Commit();
  }

  userver::formats::json::StringBuilder sb{};
  WriteToStream(result, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::multiple_queries
