#include "handler.hpp"

#include <userver/formats/json/string_builder.hpp>
#include <userver/http/common_headers.hpp>

namespace userver_techempower::json {

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  request.GetHttpResponse().SetHeader(userver::http::headers::kContentType,
                                      "application/json");
  return GetResponse();
}

std::string Handler::GetResponse() {
  const auto json =
      userver::formats::json::MakeObject("message", "Hello, World!");

  userver::formats::json::StringBuilder sb{};
  sb.WriteValue(json);
  return sb.GetString();
}

}  // namespace userver_techempower::json
