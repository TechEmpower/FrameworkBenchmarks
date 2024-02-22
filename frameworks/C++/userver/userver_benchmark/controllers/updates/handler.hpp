#pragma once

#include "../../common/db_helpers.hpp"

#include <userver/server/handlers/http_handler_base.hpp>
#include <userver/storages/postgres/postgres_fwd.hpp>
#include <userver/storages/postgres/query.hpp>

namespace userver_techempower::updates {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "updates-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const final;

  std::string GetResponse(int queries) const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;

  const std::string query_arg_name_;
  const userver::storages::postgres::Query update_query_;

  db_helpers::DatabasePoolSemaphore semaphore_;
};

}  // namespace userver_techempower::updates
