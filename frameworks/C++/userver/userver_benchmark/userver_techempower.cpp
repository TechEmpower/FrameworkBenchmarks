#include <userver/components/minimal_server_component_list.hpp>
#include <userver/testsuite/testsuite_support.hpp>
#include <userver/utils/daemon_run.hpp>

#include <userver/clients/dns/component.hpp>

#include <userver/storages/postgres/component.hpp>
#include <userver/storages/secdist/component.hpp>
#include <userver/storages/secdist/provider_component.hpp>
#include <userver/tracing/manager_component.hpp>

#include "controllers/cached_queries/handler.hpp"
#include "controllers/fortunes/handler.hpp"
#include "controllers/json/handler.hpp"
#include "controllers/multiple_queries/handler.hpp"
#include "controllers/plaintext/handler.hpp"
#include "controllers/single_query/handler.hpp"
#include "controllers/updates/handler.hpp"

#include "bare/simple_router.hpp"
#include "bare/simple_server.hpp"

namespace userver_techempower {

class NoopTracingManager final
    : public userver::tracing::TracingManagerComponentBase {
 public:
  static constexpr std::string_view kName{"noop-tracing-manager"};
  using userver::tracing::TracingManagerComponentBase::
      TracingManagerComponentBase;

 protected:
  bool TryFillSpanBuilderFromRequest(
      const userver::server::http::HttpRequest&,
      userver::tracing::SpanBuilder&) const final {
    return true;
  }

  void FillRequestWithTracingContext(
      const userver::tracing::Span&,
      userver::clients::http::RequestTracingEditor) const final {}

  void FillResponseWithTracingContext(
      const userver::tracing::Span&,
      userver::server::http::HttpResponse&) const final {}
};

int Main(int argc, char* argv[]) {
  auto component_list =
      userver::components::MinimalServerComponentList()
          // some required infra
          .Append<userver::clients::dns::Component>()
          .Append<userver::components::Secdist>()
          .Append<userver::components::DefaultSecdistProvider>()
          .Append<userver::components::TestsuiteSupport>()
          .Append<userver::components::Postgres>("hello-world-db")
          // actual handlers
          .Append<plaintext::Handler>()
          .Append<json::Handler>()
          .Append<single_query::Handler>()
          .Append<multiple_queries::Handler>()
          .Append<updates::Handler>()
          .Append<cached_queries::WorldCacheComponent>()  // cache component
          .Append<cached_queries::Handler>()
          .Append<fortunes::Handler>()
          // tracing tweaks
          .Append<NoopTracingManager>()
          // bare
          .Append<bare::SimpleRouter>()
          .Append<bare::SimpleServer>();

  return userver::utils::DaemonMain(argc, argv, component_list);
}

}  // namespace userver_techempower

int main(int argc, char* argv[]) {
  return userver_techempower::Main(argc, argv);
}
