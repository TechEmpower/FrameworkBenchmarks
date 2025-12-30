#include "handler.hpp"

#include <userver/http/common_headers.hpp>

namespace userver_techempower::plaintext {

const std::string kContentTypeTextPlain{"text/plain"};

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  request.GetHttpResponse().SetHeader(userver::http::headers::kContentType,
                                      kContentTypeTextPlain);
  return GetResponse();
}

std::string Handler::GetResponse() { return "Hello, World!"; }

}  // namespace userver_techempower::plaintext
