#include "simple_server.hpp"

#include <userver/components/component_context.hpp>
#include <userver/engine/io/socket.hpp>

#include "simple_connection.hpp"
#include "simple_router.hpp"

namespace userver_techempower::bare {

SimpleServer::SimpleServer(const userver::components::ComponentConfig& config,
                           const userver::components::ComponentContext& context)
    : userver::components::TcpAcceptorBase(config, context),
      router_{context.FindComponent<SimpleRouter>()} {}

SimpleServer::~SimpleServer() = default;

void SimpleServer::ProcessSocket(userver::engine::io::Socket&& socket) {
  const auto fd = socket.Fd();
  connections_[fd] =
      std::make_unique<SimpleConnection>(*this, std::move(socket));
}

SimpleResponse SimpleServer::HandleRequest(std::string_view url) const {
  return router_.RouteRequest(url);
}

}  // namespace userver_techempower::bare

