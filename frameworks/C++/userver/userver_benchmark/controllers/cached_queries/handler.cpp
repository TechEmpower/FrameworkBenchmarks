#include "handler.hpp"

#include <userver/formats/serialize/common_containers.hpp>

#include <boost/container/small_vector.hpp>

namespace userver_techempower::cached_queries {

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerJsonBase{config, context},
      cache_{context.FindComponent<WorldCacheComponent>()},
      query_arg_name_{"count"} {}

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

  const auto cache_ptr = cache_.Get();
  const auto& cache = *cache_ptr;
  std::generate(result.begin(), result.end(),
                [&cache] { return cache.at(db_helpers::GenerateRandomId()); });

  return userver::formats::json::ValueBuilder{result}.ExtractValue();
}

}  // namespace userver_techempower::cached_queries
