#include "db_helpers.hpp"

#include <charconv>
#include <cctype>

#include <userver/formats/json/inline.hpp>
#include <userver/utils/rand.hpp>

namespace userver_techempower::db_helpers {

namespace {

int ParseFromQueryVal(std::string_view query_val) {
  if (query_val.empty()) {
    return 1;
  }

  int parse_result{};
  const auto [ptr, err] = std::from_chars(query_val.data(), query_val.data() + query_val.size(), parse_result);
  if (err != std::errc{} || ptr != query_val.data() + query_val.size()) {
    return 1;
  }

  return std::min(500, std::max(1, parse_result));
}

}

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
  return ParseFromQueryVal(arg_str);
}

int ParseParamFromQuery(std::string_view url, std::string_view name) {
  auto pos = url.find(name);
  if (pos == std::string_view::npos) {
    return 1;
  }
  pos += name.size() + 1; // +1 for '='

  std::size_t len = 0;
  while (pos + len < url.size() && std::isdigit(url[pos + len])) {
    ++len;
  }

  return ParseFromQueryVal(url.substr(pos, len));
}

}  // namespace userver_techempower::db_helpers
