#include <userver/clients/http/component.hpp>
#include <userver/components/minimal_server_component_list.hpp>
#include <userver/server/handlers/ping.hpp>
#include <userver/server/handlers/tests_control.hpp>
#include <userver/testsuite/testsuite_support.hpp>
#include <userver/utils/daemon_run.hpp>

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

int main(int argc, char* argv[]) {
  auto component_list = userver::components::MinimalServerComponentList()
                            .Append<plaintext::Handler>()
                            .Append<json::Handler>();

  return userver::utils::DaemonMain(argc, argv, component_list);
}
