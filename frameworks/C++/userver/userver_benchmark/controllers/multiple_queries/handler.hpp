#pragma once

#include "../../common/db_helpers.hpp"

#include <userver/server/handlers/http_handler_base.hpp>
#include <userver/storages/postgres/postgres_fwd.hpp>

namespace userver_techempower::multiple_queries {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "multiple-queries-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const final;

  std::string GetResponse(int queries) const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;

  const std::string query_arg_name_;

  db_helpers::DatabasePoolSemaphore semaphore_;
};

}  // namespace userver_techempower::multiple_queries
