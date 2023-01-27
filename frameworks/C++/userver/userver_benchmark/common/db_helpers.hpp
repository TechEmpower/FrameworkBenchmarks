#pragma once

#include <userver/formats/json/value.hpp>
#include <userver/server/http/http_request.hpp>
#include <userver/storages/postgres/cluster_types.hpp>
#include <userver/storages/postgres/query.hpp>

namespace userver_techempower::db_helpers {

constexpr int kMaxWorldRows = 10000;
const userver::storages::postgres::Query kSelectRowQuery{
    "SELECT id, randomNumber FROM World WHERE id = $1"};
constexpr auto kClusterHostType =
    userver::storages::postgres::ClusterHostType::kMaster;
constexpr std::string_view kDbComponentName = "hello-world-db";

struct WorldTableRow final {
  int id;
  int random_number;
};

int GenerateRandomId();
int GenerateRandomValue();

userver::formats::json::Value Serialize(
    const WorldTableRow& value,
    userver::formats::serialize::To<userver::formats::json::Value>);

int ParseParamFromQuery(const userver::server::http::HttpRequest& request,
                        const std::string& name);

int ParseParamFromQuery(std::string_view url, std::string_view name);

}  // namespace userver_techempower::db_helpers
