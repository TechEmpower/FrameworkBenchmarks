#include "handler.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/storages/postgres/postgres.hpp>

#include <boost/container/small_vector.hpp>

namespace userver_techempower::multiple_queries {

namespace {

constexpr std::size_t kBestConcurrencyWildGuess = 256;

}

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerJsonBase{config, context},
      pg_{context
              .FindComponent<userver::components::Postgres>(
                  db_helpers::kDbComponentName)
              .GetCluster()},
      query_arg_name_{"queries"},
      semaphore_{kBestConcurrencyWildGuess} {}

userver::formats::json::Value Handler::HandleRequestJsonThrow(
    const userver::server::http::HttpRequest& request,
    const userver::formats::json::Value&,
    userver::server::request::RequestContext&) const {
  const auto queries =
      db_helpers::ParseParamFromQuery(request, query_arg_name_);

  return GetResponse(queries);
}

userver::formats::json::Value Handler::GetResponse(int queries) const {
  boost::container::small_vector<db_helpers::WorldTableRow, 500> result(
      queries);
  for (auto& value : result) {
    value.id = db_helpers::GenerateRandomId();
  }

  {
    const auto lock = semaphore_.Acquire();
    for (auto& value : result) {
      value.random_number = pg_->Execute(db_helpers::kClusterHostType,
                                         db_helpers::kSelectRowQuery, value.id)
                                .AsSingleRow<db_helpers::WorldTableRow>(
                                    userver::storages::postgres::kRowTag)
                                .random_number;
    }
  }

  return userver::formats::json::ValueBuilder{result}.ExtractValue();
}

}  // namespace userver_techempower::multiple_queries
