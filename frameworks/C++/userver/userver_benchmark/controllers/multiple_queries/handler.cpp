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

  // even though this adds a round-trip for Begin/Commit we expect this to be
  // faster due to the pool semaphore contention reduction - now we have a
  // connection for ourselves until we are done with it, otherwise we would
  // likely wait on the semaphore with every new query.
  {
    const auto lock = semaphore_.Acquire();
    auto transaction = pg_->Begin(
        db_helpers::kClusterHostType,
        userver::storages::postgres::TransactionOptions{
            userver::storages::postgres::TransactionOptions::Mode::kReadOnly});
    for (auto& value : result) {
      value.random_number =
          transaction.Execute(db_helpers::kSelectRowQuery, value.id)
              .AsSingleRow<db_helpers::WorldTableRow>(
                  userver::storages::postgres::kRowTag)
              .random_number;
    }
    transaction.Commit();
  }

  return userver::formats::json::ValueBuilder{result}.ExtractValue();
}

}  // namespace userver_techempower::multiple_queries
