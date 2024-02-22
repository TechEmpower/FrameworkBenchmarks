#include "handler.hpp"

#include <userver/components/component_context.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/http/common_headers.hpp>
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
  // userver's PG doesn't accept boost::small_vector as an input, sadly
  std::vector<db_helpers::WorldTableRow> values(queries);
  for (auto& value : values) {
    value.id = db_helpers::GenerateRandomId();
  }
  // we have to sort ids to not deadlock in update
  std::sort(values.begin(), values.end(),
            [](const auto& lhs, const auto& rhs) { return lhs.id < rhs.id; });

  boost::container::small_vector<db_helpers::WorldTableRow, 20> result;

  {
    const auto lock = semaphore_.Acquire();

    auto trx =
        pg_->Begin(db_helpers::kClusterHostType, {}, db_helpers::kDefaultPgCC);
    for (auto& value : values) {
      value.random_number = trx.Execute(db_helpers::kDefaultPgCC,
                                        db_helpers::kSelectRowQuery, value.id)
                                .AsSingleRow<db_helpers::WorldTableRow>(
                                    userver::storages::postgres::kRowTag)
                                .random_number;
    }

    // We copy values here (and hope compiler optimizes it into one memcpy call)
    // to not serialize into json within transaction
    result.assign(values.begin(), values.end());

    for (auto& value : values) {
      value.random_number = db_helpers::GenerateRandomValue();
    }

    trx.ExecuteDecomposeBulk(db_helpers::kDefaultPgCC, update_query_, values,
                             values.size());
    trx.Commit();
  }

  userver::formats::json::StringBuilder sb{};
  WriteToStream(result, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::updates
