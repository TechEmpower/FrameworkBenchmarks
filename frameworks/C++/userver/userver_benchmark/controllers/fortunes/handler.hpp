#pragma once

#include <userver/server/handlers/http_handler_base.hpp>

#include <userver/storages/postgres/postgres_fwd.hpp>
#include <userver/storages/postgres/query.hpp>

namespace userver_techempower::fortunes {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "fortunes-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const final;

  std::string GetResponse() const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;
  const userver::storages::postgres::Query select_all_fortunes_query_;
};

}  // namespace userver_techempower::fortunes
