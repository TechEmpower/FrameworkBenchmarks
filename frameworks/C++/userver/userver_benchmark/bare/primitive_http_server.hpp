#pragma once

#include <array>
#include <memory>

#include <userver/components/tcp_acceptor_base.hpp>

namespace userver_techempower::bare {

class PrimitiveHttpConnection;

class PrimitiveHttpServer final : public userver::components::TcpAcceptorBase {
 public:
  static constexpr std::string_view kName{"primitive-http-server"};

  PrimitiveHttpServer(const userver::components::ComponentConfig& config,
                      const userver::components::ComponentContext& context);
  ~PrimitiveHttpServer() final;

 private:
  void ProcessSocket(userver::engine::io::Socket&& socket) final;

  struct Response final {
    std::string body;
    std::string content_type;
  };

  friend class PrimitiveHttpConnection;
  [[nodiscard]] Response HandleRequest(std::string_view url) const;

  static constexpr std::size_t kMaxFd = 65536;
  std::array<std::unique_ptr<PrimitiveHttpConnection>, kMaxFd> connections_;
};

}  // namespace userver_techempower::bare
