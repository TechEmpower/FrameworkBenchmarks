#include "handler.hpp"

#include <boost/container/small_vector.hpp>

#include <userver/formats/serialize/common_containers.hpp>
#include <userver/http/common_headers.hpp>

namespace userver_techempower::cached_queries {

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerBase{config, context},
      cache_{context.FindComponent<WorldCacheComponent>()},
      query_arg_name_{"count"} {}

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
  boost::container::small_vector<db_helpers::WorldTableRow, 500> result(
      queries);

  const auto cache_ptr = cache_.Get();
  const auto& cache = *cache_ptr;
  std::generate(result.begin(), result.end(),
                [&cache] { return cache.at(db_helpers::GenerateRandomId()); });

  userver::formats::json::StringBuilder sb{};
  WriteToStream(result, sb);
  return sb.GetString();
}

}  // namespace userver_techempower::cached_queries
