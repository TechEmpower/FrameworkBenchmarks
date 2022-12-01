#include "db_helpers.hpp"

#include <userver/formats/json/inline.hpp>
#include <userver/utils/rand.hpp>

namespace userver_techempower::db_helpers {

int GenerateRandomId() {
  return userver::utils::RandRange(1, kMaxWorldRows + 1);
}

int GenerateRandomValue() {
  return userver::utils::RandRange(1, kMaxWorldRows + 1);
}

userver::formats::json::Value Serialize(
    const WorldTableRow& value,
    userver::formats::serialize::To<userver::formats::json::Value>) {
  return userver::formats::json::MakeObject("id", value.id, "randomNumber",
                                            value.random_number);
}

int ParseParamFromQuery(const userver::server::http::HttpRequest& request,
                        const std::string& name) {
  const auto& arg_str = request.GetArg(name);
  if (arg_str.empty()) {
    return 1;
  }

  try {
    int value = std::stoi(arg_str);
    return std::min(500, std::max(1, value));
  } catch (const std::invalid_argument&) {
    return 1;
  }
}

}  // namespace userver_techempower::db_helpers
