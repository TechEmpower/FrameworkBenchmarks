#pragma once

#include <userver/server/handlers/http_handler_base.hpp>

namespace userver_techempower::plaintext {

class Handler final : public userver::server::handlers::HttpHandlerBase {
 public:
  static constexpr std::string_view kName = "plaintext-handler";

  using HttpHandlerBase::HttpHandlerBase;

  std::string HandleRequestThrow(
      const userver::server::http::HttpRequest& request,
      userver::server::request::RequestContext&) const final;

  static std::string GetResponse();
};

}  // namespace userver_techempower::plaintext
