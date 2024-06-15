#pragma once

#include <userver/engine/semaphore.hpp>
#include <userver/formats/json/string_builder.hpp>
#include <userver/formats/json/value.hpp>
#include <userver/formats/serialize/write_to_stream.hpp>
#include <userver/server/http/http_request.hpp>
#include <userver/storages/postgres/cluster_types.hpp>
#include <userver/storages/postgres/options.hpp>
#include <userver/storages/postgres/query.hpp>

namespace userver_techempower::db_helpers {

userver::storages::postgres::Query CreateNonLoggingQuery(std::string statement);

constexpr int kMaxWorldRows = 10000;

const userver::storages::postgres::Query kSelectRowQuery =
    CreateNonLoggingQuery("SELECT id, randomNumber FROM World WHERE id = $1");

constexpr auto kClusterHostType =
    userver::storages::postgres::ClusterHostType::kMaster;

constexpr userver::storages::postgres::CommandControl kDefaultPgCC{
    std::chrono::seconds{7}, std::chrono::seconds{7}};

constexpr std::string_view kDbComponentName = "hello-world-db";

struct WorldTableRow final {
  int id;
  int random_number;
};

void WriteToStream(const WorldTableRow& row,
                   userver::formats::json::StringBuilder& sb);

int GenerateRandomId();
int GenerateRandomValue();

int ParseParamFromQuery(const userver::server::http::HttpRequest& request,
                        const std::string& name);

int ParseParamFromQuery(std::string_view url, std::string_view name);

class DatabasePoolSemaphore final {
 public:
  explicit DatabasePoolSemaphore(std::size_t initial_count);

  userver::engine::SemaphoreLock Acquire() const;

 private:
  mutable userver::engine::Semaphore semaphore_;
};

}  // namespace userver_techempower::db_helpers
