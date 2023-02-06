#include "handler.hpp"

#include "../../common/db_helpers.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/storages/postgres/postgres.hpp>

#include <boost/container/small_vector.hpp>

namespace userver_techempower::multiple_queries {

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerJsonBase{config, context},
      pg_{context
              .FindComponent<userver::components::Postgres>(
                  db_helpers::kDbComponentName)
              .GetCluster()},
      query_arg_name_{"queries"} {}

userver::formats::json::Value Handler::HandleRequestJsonThrow(
    const userver::server::http::HttpRequest& request,
    const userver::formats::json::Value&,
    userver::server::request::RequestContext&) const {
  const auto queries =
      db_helpers::ParseParamFromQuery(request, query_arg_name_);

  return GetResponse(queries);
}

userver::formats::json::Value Handler::GetResponse(int queries) const {
  boost::container::small_vector<int, 500> random_ids(queries);
  std::generate(random_ids.begin(), random_ids.end(),
                db_helpers::GenerateRandomId);

  boost::container::small_vector<db_helpers::WorldTableRow, 500> result{};
  for (auto id : random_ids) {
    result.push_back(pg_->Execute(db_helpers::kClusterHostType,
                                  db_helpers::kSelectRowQuery, id)
                         .AsSingleRow<db_helpers::WorldTableRow>(
                             userver::storages::postgres::kRowTag));
  }

  return userver::formats::json::ValueBuilder{result}.ExtractValue();
}

}  // namespace userver_techempower::multiple_queries
