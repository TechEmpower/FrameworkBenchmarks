#include <folly/MPMCQueue.h>
#include <folly/io/IOBuf.h>
#include <folly/net/NetworkSocket.h>
#include <folly/system/ThreadId.h>
#include <folly/system/ThreadName.h>

#include <proxygen/lib/http/HTTPMessage.h>
#include <proxygen/lib/http/session/HTTPTransaction.h>

#include <sib/network/s_proxygen_server.hpp>
#include <sib/sib.hpp>

using namespace sib::network::http;

// Constants
const size_t s_compute_pool_size() {
  constexpr size_t min_size = 512;
  constexpr size_t max_size = 4096;
  constexpr size_t per_thread = 128;
  size_t total = per_thread * std::max(1u, std::thread::hardware_concurrency());
  return std::min(std::max(total, min_size), max_size);
}

const size_t HANDLER_POOL_SIZE = s_compute_pool_size();

constexpr auto MAX_BUFFER_SIZE = 4 * 1024 * 1024; // 4MB

// forward declaration
struct handler;

// Thread-local handler Pool
thread_local static folly::MPMCQueue<handler*> t_handler_pool(HANDLER_POOL_SIZE);

struct handler : public proxygen::HTTPTransaction::Handler {
  handler() = default;

  void onHeadersComplete(std::unique_ptr<proxygen::HTTPMessage> /*req*/) noexcept override {
    proxygen::HTTPMessage response;
    response.setStatusCode(200);
    response.setStatusMessage("OK");
    response.setIsChunked(false);
    response.setWantsKeepalive(true);

    constexpr std::string_view SERVER_NAME = "SIB";
    constexpr std::string_view CONTENT_TYPE = "text/plain";
    constexpr std::string_view PAYLOAD_LEN = "13";
    constexpr std::string_view PAYLOAD = "Hello, World!";
    auto buffer = folly::IOBuf::wrapBuffer(PAYLOAD.data(), PAYLOAD.size());

    auto& headers = response.getHeaders();
    headers.add(proxygen::HTTPHeaderCode::HTTP_HEADER_SERVER, SERVER_NAME);
    headers.add(proxygen::HTTPHeaderCode::HTTP_HEADER_CONTENT_TYPE, CONTENT_TYPE.data());
    headers.add(proxygen::HTTPHeaderCode::HTTP_HEADER_CONTENT_LENGTH, PAYLOAD_LEN.data());

    txn_->sendHeaders(response);
    txn_->sendBody(std::move(buffer));
    txn_->sendEOM();
  }

  void onBody(std::unique_ptr<folly::IOBuf>) noexcept override {}
  void onTrailers(std::unique_ptr<proxygen::HTTPHeaders>) noexcept override {}
  void onUpgrade(proxygen::UpgradeProtocol) noexcept override {}
  void onError(const proxygen::HTTPException&) noexcept override {}
  void onEgressPaused() noexcept override {}
  void onEgressResumed() noexcept override {}

  void setTransaction(proxygen::HTTPTransaction* txn) noexcept override { txn_ = txn; }

  void detachTransaction() noexcept override {
    txn_ = nullptr;
    if (!t_handler_pool.write(this)) {
      delete this; // Failed to return to pool, delete instead
    }
  }

  void onEOM() noexcept override {}

 private:
  proxygen::HTTPTransaction* txn_{nullptr};
};

auto create_socket_opt() -> folly::SocketOptionMap {
  using ApplyPos = folly::SocketOptionKey::ApplyPos;
  folly::SocketOptionMap socket_opt{};
  // Enable SO_REUSEADDR, this is important for the server to be able to restart quickly
  socket_opt.emplace(
    folly::SocketOptionKey{SOL_SOCKET, SO_REUSEADDR, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{1});

  // Disable Nagle (reduce latency)
  socket_opt.emplace(
    folly::SocketOptionKey{IPPROTO_TCP, TCP_NODELAY, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{1});
  socket_opt.emplace(
    folly::SocketOptionKey{IPPROTO_TCP, TCP_NODELAY, ApplyPos::POST_BIND},
    folly::SocketOptionValue{1});
  socket_opt.emplace(
    folly::SocketOptionKey{
      IPPROTO_TCP, TCP_NOTSENT_LOWAT, folly::SocketOptionKey::ApplyPos::POST_BIND},
    folly::SocketOptionValue{1});

  // Increase socket buffers (avoid drops under load)
  socket_opt.emplace(
    folly::SocketOptionKey{SOL_SOCKET, SO_RCVBUF, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{MAX_BUFFER_SIZE});
  socket_opt.emplace(
    folly::SocketOptionKey{SOL_SOCKET, SO_SNDBUF, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{MAX_BUFFER_SIZE});

// Linux only TCP fast open for reducing handshake overhead
#ifdef __linux__
  socket_opt.emplace(
    folly::SocketOptionKey{SOL_SOCKET, SO_REUSEPORT, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{1});
  socket_opt.emplace(
    folly::SocketOptionKey{IPPROTO_TCP, TCP_FASTOPEN, ApplyPos::PRE_BIND},
    folly::SocketOptionValue{1000} // Queue length for TFO
  );
#else
  socket_opt.emplace(
    folly::SocketOptionKey{IPPROTO_TCP, TCP_NOOPT, folly::SocketOptionKey::ApplyPos::PRE_BIND},
    folly::SocketOptionValue{1});
#endif

  return socket_opt;
}

// === Main ===
int main(int argc, char** argv) {
  std::span<char*> argvSpan(argv, argc);
  std::ignore = sib::init(argc, argvSpan);

  proxygen::HTTPServerOptions opts;
  opts.threads = 0;
  opts.shutdownOn = {SIGINT};
  opts.idleTimeout = std::chrono::milliseconds(30000);
  opts.enableContentCompression = false;
  opts.h2cEnabled = false;
  opts.listenBacklog = 65535;
  opts.maxConcurrentIncomingStreams = 10000;
  opts.initialReceiveWindow = 128 * 1024;
  opts.receiveStreamWindowSize = 128 * 1024;
  opts.receiveSessionWindowSize = MAX_BUFFER_SIZE;
  opts.useZeroCopy = true;
  opts.enableExHeaders = false;

  std::vector<proxygen::HTTPServer::IPConfig> ipConfigs = {
    {folly::SocketAddress("0.0.0.0", 8080), proxygen::HTTPServer::Protocol::HTTP, nullptr}};
  ipConfigs[0].enableTCPFastOpen = true;
  ipConfigs[0].acceptorSocketOptions = create_socket_opt();

  s_h_server httpServer(std::move(opts));
  httpServer.set_domains({"localhost"})
    .set_alpn_protocols({"http/1.1"})
    .set_ips(std::move(ipConfigs));

  auto server = s_proxygen_server::make()->set_h(std::move(httpServer));

  server->run_forever(
    []([[maybe_unused]] proxygen::HTTPMessage* req) -> proxygen::HTTPTransactionHandler* {
      static thread_local bool pool_initialized = false;
      if (!pool_initialized) {
        pool_initialized = true;
        auto fill_count = HANDLER_POOL_SIZE /
          std::max(1u, static_cast<unsigned>(std::thread::hardware_concurrency()));
        for (auto i = 0; i < fill_count; ++i) {
          t_handler_pool.write(new handler());
        }
      }

      handler* h = nullptr;
      if (!t_handler_pool.read(h)) {
        h = new handler();
      }
      return h;
    });
}