#include <userver/components/minimal_server_component_list.hpp>
#include <userver/testsuite/testsuite_support.hpp>
#include <userver/utils/daemon_run.hpp>

#include <userver/clients/dns/component.hpp>

#include <userver/storages/postgres/component.hpp>
#include <userver/storages/secdist/component.hpp>
#include <userver/storages/secdist/provider_component.hpp>

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

int Main(int argc, char* argv[]) {
  auto component_list =
      userver::components::MinimalServerComponentList()
          .Append<userver::clients::dns::Component>()
          .Append<userver::components::Secdist>()
          .Append<userver::components::DefaultSecdistProvider>()
          .Append<userver::components::TestsuiteSupport>()
          .Append<userver::components::Postgres>("hello-world-db")
          .Append<plaintext::Handler>()
          .Append<json::Handler>()
          .Append<single_query::Handler>()
          .Append<multiple_queries::Handler>()
          .Append<updates::Handler>()
          .Append<cached_queries::WorldCacheComponent>()
          .Append<cached_queries::Handler>()
          .Append<fortunes::Handler>()
          // bare
          .Append<bare::SimpleRouter>()
          .Append<bare::SimpleServer>();

  return userver::utils::DaemonMain(argc, argv, component_list);
}

}  // namespace userver_techempower

int main(int argc, char* argv[]) {
  return userver_techempower::Main(argc, argv);
}
