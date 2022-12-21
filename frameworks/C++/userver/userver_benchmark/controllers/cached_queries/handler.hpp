#pragma once

#include <userver/server/handlers/http_handler_json_base.hpp>

#include "world_cache_component.hpp"

namespace userver_techempower::cached_queries {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "cached-queries-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest& request,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const final;

  userver::formats::json::Value GetResponse(int queries) const;

 private:
  const WorldCacheComponent& cache_;

  const std::string query_arg_name_;
};

}  // namespace userver_techempower::cached_queries
