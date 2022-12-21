#include "primitive_http_server.hpp"

#include <userver/components/component_context.hpp>
#include <userver/engine/io/socket.hpp>

#include "../controllers/json/handler.hpp"
#include "../controllers/plaintext/handler.hpp"
#include "../controllers/single_query/handler.hpp"

#include "primitive_http_connection.hpp"

namespace userver_techempower::bare {

constexpr std::string_view kPlainTextUrlPrefix{"/plaintext"};
constexpr std::string_view kJsontUrlPrefix{"/json"};
constexpr std::string_view kSingleQueryUrlPrefix{"/db"};
/*constexpr std::string_view kPlainTextUrlPrefix{"/plaintext"};
constexpr std::string_view kPlainTextUrlPrefix{"/plaintext"};*/

PrimitiveHttpServer::PrimitiveHttpServer(
    const userver::components::ComponentConfig& config,
    const userver::components::ComponentContext& context)
    : userver::components::TcpAcceptorBase(config, context),
      single_query_{context.FindComponent<single_query::Handler>()} {}

PrimitiveHttpServer::~PrimitiveHttpServer() = default;

void PrimitiveHttpServer::ProcessSocket(userver::engine::io::Socket&& socket) {
  const auto fd = socket.Fd();
  connections_[fd] =
      std::make_unique<PrimitiveHttpConnection>(*this, std::move(socket));
}

PrimitiveHttpServer::Response PrimitiveHttpServer::HandleRequest(
    std::string_view url) const {
  if (url.find(kPlainTextUrlPrefix) == 0) {
    return {plaintext::Handler::GetResponse(), "text/plain"};
  }

  if (url.find(kJsontUrlPrefix) == 0) {
    return {userver::formats::json::ToString(json::Handler::GetResponse()),
            "application/json"};
  }

  if (url.find(kSingleQueryUrlPrefix) == 0) {
    return {userver::formats::json::ToString(single_query_.GetResponse()),
            "application/json"};
  }

  return {};
}

}  // namespace userver_techempower::bare