#pragma once

#include "../../common/db_helpers.hpp"

#include <userver/server/handlers/http_handler_base.hpp>

#include <userver/storages/postgres/postgres_fwd.hpp>

namespace userver_techempower::single_query {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "single-query-handler";

  Handler(const userver::components::ComponentConfig& config,
          const userver::components::ComponentContext& context);

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest&,
      userver::server::request::RequestContext&) const final;

  std::string GetResponse() const;

 private:
  const userver::storages::postgres::ClusterPtr pg_;

  db_helpers::DatabasePoolSemaphore semaphore_;
};

}  // namespace userver_techempower::single_query
