#pragma once

#include <userver/server/handlers/http_handler_json_base.hpp>
#include <userver/storages/postgres/postgres_fwd.hpp>

namespace userver_techempower::multiple_queries {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "multiple-queries-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest& request,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const final;

  userver::formats::json::Value GetResponse(int queries) const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;

  const std::string query_arg_name_;
};

}  // namespace userver_techempower::multiple_queries
