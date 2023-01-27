#include "handler.hpp"

namespace userver_techempower::plaintext {

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  request.GetHttpResponse().SetContentType("text/plain");
  return GetResponse();
}

std::string Handler::GetResponse() {
  return "Hello, World!";
}

}  // namespace userver_techempower::plaintext
