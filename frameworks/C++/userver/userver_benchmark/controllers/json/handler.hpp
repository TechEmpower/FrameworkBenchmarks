#pragma once

#include <userver/server/handlers/http_handler_json_base.hpp>

namespace userver_techempower::json {

class Handler final : public userver::server::handlers::HttpHandlerJsonBase {
 public:
  static constexpr std::string_view kName = "json-handler";

  using HttpHandlerJsonBase::HttpHandlerJsonBase;

  userver::formats::json::Value HandleRequestJsonThrow(
      const userver::server::http::HttpRequest&,
      const userver::formats::json::Value&,
      userver::server::request::RequestContext&) const final;

  static userver::formats::json::Value GetResponse();
};

}  // namespace userver_techempower::json
