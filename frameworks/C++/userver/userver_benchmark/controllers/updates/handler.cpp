#include "handler.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/http/common_headers.hpp>
#include <userver/storages/postgres/postgres.hpp>

#include <boost/container/small_vector.hpp>

namespace userver::storages::postgres::io::traits {

// Hijack userver's whitelist of allowed containers
template <typename T, std::size_t Size>
struct IsCompatibleContainer<boost::container::small_vector<T, Size>>
    : std::true_type {};

}  // namespace userver::storages::postgres::io::traits

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

constexpr std::size_t kBestConcurrencyWildGuess = 128;

}  // namespace

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerBase{config, context},
      pg_{context.FindComponent<userver::components::Postgres>("hello-world-db")
              .GetCluster()},
      query_arg_name_{"queries"},
      update_query_{db_helpers::CreateNonLoggingQuery(kUpdateQueryStr)},
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
  boost::container::small_vector<int, 20> ids(queries);
  for (auto& id : ids) {
    id = db_helpers::GenerateRandomId();
  }
  // we have to sort ids to not deadlock in update
  std::sort(ids.begin(), ids.end(),
            [](const auto& lhs, const auto& rhs) { return lhs < rhs; });

  boost::container::small_vector<int, 20> values(queries);
  for (auto& value : values) {
    value = db_helpers::GenerateRandomValue();
  }

  const auto db_results = [this, &ids, &values] {
    const auto lock = semaphore_.Acquire();

    auto query_queue = pg_->CreateQueryQueue(db_helpers::kClusterHostType,
                                             db_helpers::kDefaultPgCC.execute);
    query_queue.Reserve(ids.size() + 1 /* for the update query */);
    for (const auto id : ids) {
      query_queue.Push(db_helpers::kDefaultPgCC, db_helpers::kSelectRowQuery,
                       id);
    }

    query_queue.Push(db_helpers::kDefaultPgCC, update_query_, ids, values);

    return query_queue.Collect(db_helpers::kDefaultPgCC.execute);
  }();

  boost::container::small_vector<db_helpers::WorldTableRow, 20> result(queries);
  for (std::size_t i = 0; i < result.size(); ++i) {
    result[i] = db_results[i].AsSingleRow<db_helpers::WorldTableRow>(
        userver::storages::postgres::kRowTag);
  }

  userver::formats::json::StringBuilder sb{};
  WriteToStream(result, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::updates
