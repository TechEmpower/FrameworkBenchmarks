#pragma once

#include <userver/server/handlers/http_handler_json_base.hpp>

#include <userver/storages/postgres/postgres_fwd.hpp>

namespace userver_techempower::single_query {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "single-query-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest&,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const final;

  userver::formats::json::Value GetResponse() const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;
};

}  // namespace userver_techempower::single_query
