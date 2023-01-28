#include "handler.hpp"

#include "../../common/db_helpers.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/storages/postgres/postgres.hpp>

#include <boost/container/small_vector.hpp>

namespace userver_techempower::updates {

namespace {

constexpr const char* kUpdateQueryStr{R"(
UPDATE World w SET
  randomNumber = new_numbers.randomNumber
FROM ( SELECT
  UNNEST($1) as id,
  UNNEST($2) as randomNumber
) new_numbers
WHERE w.id = new_numbers.id
)"};

}

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerJsonBase{config, context},
      pg_{context.FindComponent<userver::components::Postgres>("hello-world-db")
              .GetCluster()},
      query_arg_name_{"queries"},
      update_query_{kUpdateQueryStr} {}

userver::formats::json::Value Handler::HandleRequestJsonThrow(
    const userver::server::http::HttpRequest& request,
    const userver::formats::json::Value&,
    userver::server::request::RequestContext&) const {
  const auto queries =
      db_helpers::ParseParamFromQuery(request, query_arg_name_);

  return GetResponse(queries);
}

userver::formats::json::Value Handler::GetResponse(int queries) const {
  std::vector<int> random_ids(queries);
  std::generate(random_ids.begin(), random_ids.end(),
                db_helpers::GenerateRandomId);
  std::sort(random_ids.begin(), random_ids.end());

  boost::container::small_vector<db_helpers::WorldTableRow, 500> result{};
  for (auto id : random_ids) {
    result.push_back(pg_->Execute(db_helpers::kClusterHostType,
                                  db_helpers::kSelectRowQuery, id)
                         .AsSingleRow<db_helpers::WorldTableRow>(
                             userver::storages::postgres::kRowTag));
  }

  std::vector<int> random_numbers(queries);
  std::generate(random_numbers.begin(), random_numbers.end(),
                db_helpers::GenerateRandomValue);

  pg_->Execute(db_helpers::kClusterHostType, update_query_, random_ids,
               random_numbers);

  return userver::formats::json::ValueBuilder{result}.ExtractValue();
}

}  // namespace userver_techempower::updates
