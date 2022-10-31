#include <userver/clients/http/component.hpp>
#include <userver/components/minimal_server_component_list.hpp>
#include <userver/formats/json/inline.hpp>
#include <userver/formats/serialize/common_containers.hpp>
#include <userver/server/handlers/ping.hpp>
#include <userver/server/handlers/tests_control.hpp>
#include <userver/testsuite/testsuite_support.hpp>
#include <userver/utils/daemon_run.hpp>
#include <userver/utils/rand.hpp>

#include <userver/storages/postgres/postgres.hpp>
#include <userver/storages/secdist/component.hpp>

#include <boost/container/small_vector.hpp>

namespace plaintext {

class Handler final : public userver::server::handlers::HttpHandlerBase {
public:
  static constexpr std::string_view kName = "plaintext-handler";

  using HttpHandlerBase::HttpHandlerBase;

  std::string
  HandleRequestThrow(const userver::server::http::HttpRequest &request,
                     userver::server::request::RequestContext &) const final {
    request.GetHttpResponse().SetContentType("text/plain");
    return "Hello, World!";
  }
};

} // namespace plaintext

namespace json {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
public:
  static constexpr std::string_view kName = "json-handler";

  using HttpHandlerJsonBase::HttpHandlerJsonBase;

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest &,
      const userver::formats::json::Value &,
      userver::server::request::RequestContext &) const final {
    return userver::formats::json::MakeObject("message", "Hello, World!");
  }
};

} // namespace json

namespace db_common {

constexpr int kMaxWorldRows = 10000;
const userver::storages::postgres::Query kSelectRowQuery{
    "SELECT id, randomNumber FROM World WHERE id = $1"};
constexpr auto kClusterHostType =
    userver::storages::postgres::ClusterHostType::kMaster;

struct TableRow final {
  int id;
  int random_number;
};

int GenerateRandomId() {
  return static_cast<int>(userver::utils::RandRange(1, kMaxWorldRows + 1));
}

userver::formats::json::Value
Serialize(const TableRow &value,
          userver::formats::serialize::To<userver::formats::json::Value>) {
  return userver::formats::json::MakeObject("id", value.id, "randomNumber",
                                            value.random_number);
}

size_t ParseParamFromQuery(const userver::server::http::HttpRequest &request,
                           const std::string &name) {
  const auto &arg_str = request.GetArg(name);
  if (arg_str.empty()) {
    return 1;
  }

  try {
    int value = std::stoi(arg_str);
    return std::min(500, std::max(1, value));
  } catch (const std::invalid_argument &) {
    return 1;
  }
}

} // namespace db_common

namespace single_query {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
public:
  static constexpr std::string_view kName = "single-query-handler";

  Handler(const userver::components::ComponentConfig &config,
          const userver::components::ComponentContext &context)
      : userver::server::handlers::HttpHandlerJsonBase{config, context},
        pg_{context
                .FindComponent<userver::components::Postgres>("hello-world-db")
                .GetCluster()} {}

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest &,
      const userver::formats::json::Value &,
      userver::server::request::RequestContext &) const final {
    const auto row =
        pg_->Execute(userver::storages::postgres::ClusterHostType::kMaster,
                     db_common::kSelectRowQuery, db_common::GenerateRandomId())
            .AsSingleRow<db_common::TableRow>(
                userver::storages::postgres::kRowTag);

    return db_common::Serialize(row, {});
  }

private:
  const userver::storages::postgres::ClusterPtr pg_;
};

} // namespace single_query

namespace multiple_queries {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
public:
  static constexpr std::string_view kName = "multiple-queries-handler";

  Handler(const userver::components::ComponentConfig &config,
          const userver::components::ComponentContext &context)
      : userver::server::handlers::HttpHandlerJsonBase{config, context},
        pg_{context
                .FindComponent<userver::components::Postgres>("hello-world-db")
                .GetCluster()} {}

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest &request,
      const userver::formats::json::Value &,
      userver::server::request::RequestContext &) const final {
    const auto queries_count =
        db_common::ParseParamFromQuery(request, kQueryArgName);

    boost::container::small_vector<int, 500> random_ids(queries_count);
    std::generate(random_ids.begin(), random_ids.end(),
                  db_common::GenerateRandomId);

    boost::container::small_vector<db_common::TableRow, 500> result{};
    for (auto id : random_ids) {
      result.push_back(pg_->Execute(db_common::kClusterHostType,
                                    db_common::kSelectRowQuery, id)
                           .AsSingleRow<db_common::TableRow>(
                               userver::storages::postgres::kRowTag));
    }

    return userver::formats::json::ValueBuilder{result}.ExtractValue();
  }

private:
  const userver::storages::postgres::ClusterPtr pg_;

  const std::string kQueryArgName{"queries"};
};

} // namespace multiple_queries

namespace updates {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
public:
  static constexpr std::string_view kName = "updates-handler";

  Handler(const userver::components::ComponentConfig &config,
          const userver::components::ComponentContext &context)
      : userver::server::handlers::HttpHandlerJsonBase{config, context},
        pg_{context
                .FindComponent<userver::components::Postgres>("hello-world-db")
                .GetCluster()} {}

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest &request,
      const userver::formats::json::Value &,
      userver::server::request::RequestContext &) const final {
    const auto queries_count =
        db_common::ParseParamFromQuery(request, kQueryArgName);

    std::vector<int> random_ids(queries_count);
    std::generate(random_ids.begin(), random_ids.end(),
                  db_common::GenerateRandomId);
    std::sort(random_ids.begin(), random_ids.end());

    boost::container::small_vector<db_common::TableRow, 500> result{};
    for (auto id : random_ids) {
      result.push_back(pg_->Execute(db_common::kClusterHostType,
                                    db_common::kSelectRowQuery, id)
                           .AsSingleRow<db_common::TableRow>(
                               userver::storages::postgres::kRowTag));
    }

    std::vector<int> random_numbers(queries_count);
    std::generate(random_numbers.begin(), random_numbers.end(),
                  db_common::GenerateRandomId);

    pg_->Execute(db_common::kClusterHostType, update_query_, random_ids,
                 random_numbers);

    return userver::formats::json::ValueBuilder{result}.ExtractValue();
  }

private:
  const userver::storages::postgres::ClusterPtr pg_;

  const std::string kQueryArgName{"queries"};
  const userver::storages::postgres::TransactionOptions trx_options_{
      userver::storages::postgres::IsolationLevel::kReadUncommitted};
  const userver::storages::postgres::Query update_query_{
      R"(UPDATE World w SET
  randomNumber = new_numbers.randomNumber
FROM ( SELECT
  UNNEST($1) as id,
  UNNEST($2) as randomNumber
) new_numbers
WHERE w.id = new_numbers.id
)"};
};

} // namespace updates

int main(int argc, char *argv[]) {
  auto component_list =
      userver::components::MinimalServerComponentList()
          .Append<userver::components::Secdist>()
          .Append<userver::components::TestsuiteSupport>()
          .Append<plaintext::Handler>()
          .Append<json::Handler>()
          .Append<userver::components::Postgres>("hello-world-db")
          .Append<single_query::Handler>()
          .Append<multiple_queries::Handler>()
          .Append<updates::Handler>();

  return userver::utils::DaemonMain(argc, argv, component_list);
}
