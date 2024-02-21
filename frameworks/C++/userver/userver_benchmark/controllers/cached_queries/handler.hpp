#pragma once

#include <userver/server/handlers/http_handler_base.hpp>

#include "world_cache_component.hpp"

namespace userver_techempower::cached_queries {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "cached-queries-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const final;

  std::string GetResponse(int queries) const;

 private:
  const WorldCacheComponent& cache_;

  const std::string query_arg_name_;
};

}  // namespace userver_techempower::cached_queries
