#pragma once

#include <array>
#include <memory>

#include <userver/components/tcp_acceptor_base.hpp>

#include "simple_response.hpp"

namespace userver_techempower::bare {

class SimpleConnection;
class SimpleRouter;

class SimpleServer final : public userver::components::TcpAcceptorBase {
 public:
  static constexpr std::string_view kName{"simple-server"};

  SimpleServer(const userver::components::ComponentConfig& config,
               const userver::components::ComponentContext& context);
  ~SimpleServer() final;

 private:
  void ProcessSocket(userver::engine::io::Socket&& socket) final;

  friend class SimpleConnection;
  [[nodiscard]] SimpleResponse HandleRequest(std::string_view url) const;

  const SimpleRouter& router_;

  static constexpr std::size_t kMaxFd = 65536;
  std::array<std::unique_ptr<SimpleConnection>, kMaxFd> connections_;
};
}  // namespace userver_techempower::bare
