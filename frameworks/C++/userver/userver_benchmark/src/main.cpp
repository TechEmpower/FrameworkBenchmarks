#include <userver/clients/http/component.hpp>
#include <userver/components/minimal_server_component_list.hpp>
#include <userver/server/handlers/ping.hpp>
#include <userver/server/handlers/tests_control.hpp>
#include <userver/testsuite/testsuite_support.hpp>
#include <userver/utils/daemon_run.hpp>
#include <userver/utils/rand.hpp>

#include <userver/storages/postgres/postgres.hpp>
#include <userver/storages/secdist/component.hpp>

namespace plaintext {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "plaintext-handler";

  using HttpHandlerBase::HttpHandlerBase;

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const override {
    request.GetHttpResponse().SetContentType("text/plain");
    return "Hello, World!";
  }
};

}  // namespace plaintext

namespace json {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "json-handler";

  using HttpHandlerJsonBase::HttpHandlerJsonBase;

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest&,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const override {
    userver::formats::json::ValueBuilder builder{
        userver::formats::json::Type::kObject};
    builder["message"] = "Hello, World!";

    return builder.ExtractValue();
  }
};

}  // namespace json

namespace single_query {

struct TableRow final {
  int id;
  int random_number;
};

constexpr int kMaxWorldRows = 10000;

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "single-query-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context)
      : userver::server::handlers::HttpHandlerJsonBase{config, context},
        pg_{context
                .FindComponent<userver::components::Postgres>("hello-world-db")
                .GetCluster()} {}

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest&,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const override {
    const auto row =
        pg_->Execute(userver::storages::postgres::ClusterHostType::kMaster,
                     "SELECT id, randomNumber from World where id = $1",
                     userver::utils::RandRange(1, kMaxWorldRows + 1))
            .AsSingleRow<TableRow>(userver::storages::postgres::kRowTag);

    userver::formats::json::ValueBuilder builder{
        userver::formats::json::Type::kObject};
    builder["id"] = row.id;
    builder["randomNumber"] = row.random_number;
    return builder.ExtractValue();
  }

 private:
  userver::storages::postgres::ClusterPtr pg_;
};

}  // namespace single_query

int main(int argc, char* argv[]) {
  auto component_list =
      userver::components::MinimalServerComponentList()
          .Append<userver::components::Secdist>()
          .Append<userver::components::TestsuiteSupport>()
          .Append<plaintext::Handler>()
          .Append<json::Handler>()
          .Append<userver::components::Postgres>("hello-world-db")
          .Append<single_query::Handler>();

  return userver::utils::DaemonMain(argc, argv, component_list);
}
