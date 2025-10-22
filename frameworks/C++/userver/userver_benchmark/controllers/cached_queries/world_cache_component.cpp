#include "world_cache_component.hpp"

namespace userver_techempower::cached_queries {

WorldCache::WorldCache() { data_.reserve(db_helpers::kMaxWorldRows + 1); }

std::size_t WorldCache::size() const { return data_.size(); }

void WorldCache::insert_or_assign(int key, db_helpers::WorldTableRow&& row) {
  if (size() <= static_cast<std::size_t>(key)) {
    data_.resize(key + 1);
  }

  data_[key] = row;
}

const db_helpers::WorldTableRow& WorldCache::at(size_t ind) const {
  return data_[ind];
}

}  // namespace userver_techempower::cached_queries
