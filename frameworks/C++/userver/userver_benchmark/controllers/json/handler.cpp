#include "handler.hpp"

namespace userver_techempower::json {

userver::formats::json::Value Handler::HandleRequestJsonThrow(
    const userver::server::http::HttpRequest&,
    const userver::formats::json::Value&,
    userver::server::request::RequestContext&) const {
  return GetResponse();
}

userver::formats::json::Value Handler::GetResponse() {
  return userver::formats::json::MakeObject("message", "Hello, World!");
}

}  // namespace userver_techempower::json
