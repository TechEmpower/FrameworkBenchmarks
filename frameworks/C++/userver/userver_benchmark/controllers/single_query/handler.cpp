#include "handler.hpp"

#include "../../common/db_helpers.hpp"

#include <userver/components/component_context.hpp>
#include <userver/storages/postgres/postgres.hpp>

namespace userver_techempower::single_query {

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerJsonBase{config, context},
      pg_{context
              .FindComponent<userver::components::Postgres>(
                  db_helpers::kDbComponentName)
              .GetCluster()} {}

userver::formats::json::Value Handler::HandleRequestJsonThrow(
    const userver::server::http::HttpRequest&,
    const userver::formats::json::Value&,
    userver::server::request::RequestContext&) const {
  return GetResponse();
}

userver::formats::json::Value Handler::GetResponse() const {
  const auto row =
      pg_->Execute(db_helpers::kClusterHostType, db_helpers::kSelectRowQuery,
                   db_helpers::GenerateRandomId())
          .AsSingleRow<db_helpers::WorldTableRow>(
              userver::storages::postgres::kRowTag);

  return db_helpers::Serialize(row, {});
}

}  // namespace userver_techempower::single_query
