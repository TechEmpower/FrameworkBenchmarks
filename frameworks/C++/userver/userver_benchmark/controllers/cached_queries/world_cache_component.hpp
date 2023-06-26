#pragma once

#include <vector>

#include "../../common/db_helpers.hpp"

#include <userver/cache/base_postgres_cache.hpp>

namespace userver_techempower::cached_queries {

class WorldCache final {
 public:
  WorldCache();

  std::size_t size() const;
  void insert_or_assign(int key, db_helpers::WorldTableRow&& row);

  const db_helpers::WorldTableRow& at(size_t ind) const;

 private:
  std::vector<db_helpers::WorldTableRow> data_;
};

struct WorldCachePolicy final {
  static constexpr std::string_view kName = "world-pg-cache";
  using ValueType = db_helpers::WorldTableRow;
  using CacheContainer = WorldCache;
  static constexpr auto kKeyMember = &db_helpers::WorldTableRow::id;
  static constexpr const char* kQuery = "SELECT id, randomNumber FROM World";
  static constexpr const char* kUpdatedField = "";
  static constexpr auto kClusterHostType = db_helpers::kClusterHostType;
};

using WorldCacheComponent = userver::components::PostgreCache<WorldCachePolicy>;

}  // namespace userver_techempower::cached_queries
