#include "simple_connection.hpp"

#include <array>

#include <cctz/time_zone.h>
#include <llhttp.h>
#include <boost/container/small_vector.hpp>

#include "simple_server.hpp"

#include <userver/engine/async.hpp>
#include <userver/utils/datetime/wall_coarse_clock.hpp>
#include <userver/utils/scope_guard.hpp>
#include <userver/utils/small_string.hpp>

namespace userver_techempower::bare {

namespace {

struct HttpParser final {
  llhttp_t parser{};
  llhttp_settings_t parser_settings{};

  std::function<void(std::string_view)> on_request_cb{};

  userver::utils::SmallString<50> url;

  explicit HttpParser(std::function<void(std::string_view)> on_request_cb)
      : on_request_cb{std::move(on_request_cb)} {
    llhttp_settings_init(&parser_settings);
    parser_settings.on_url = HttpOnUrl;
    parser_settings.on_message_begin = HttpOnMessageBegin;
    parser_settings.on_message_complete = HttpOnMessageComplete;

    llhttp_init(&parser, HTTP_REQUEST, &parser_settings);
    parser.data = this;
  }

  auto Execute(const char* data, std::size_t length) {
    return llhttp_execute(&parser, data, length);
  }

  static int HttpOnUrl(llhttp_t* parser, const char* data, std::size_t length) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->url.append(std::string_view{data, length});
    return 0;
  }

  static int HttpOnMessageBegin(llhttp_t* parser) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->url.clear();
    return 0;
  }

  static int HttpOnMessageComplete(llhttp_t* parser) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->on_request_cb(static_cast<std::string_view>(self->url));
    return 0;
  }
};

class ResponseBuffers final {
 public:
  using HeadersString = userver::utils::SmallString<200>;

  HeadersString& Next(userver::engine::io::Socket& socket, std::string&& body) {
    if (Size() == kMaxResponses) {
      Send(socket);
    }

    auto& response = responses_.emplace_back();
    response.body = std::move(body);
    return response.headers;
  }

  void Send(userver::engine::io::Socket& socket) {
    if (Size() == 0) {
      return;
    }

    boost::container::small_vector<struct ::iovec, kMaxResponses * 2> io_vector(
        Size() * 2);

    std::size_t index = 0;
    std::size_t total_size = 0;
    for (auto& response : responses_) {
      io_vector[index++] = {response.headers.data(), response.headers.size()};
      io_vector[index++] = {response.body.data(), response.body.size()};
      total_size += response.headers.size() + response.body.size();
    }

    if (socket.SendAll(io_vector.data(), io_vector.size(), {}) != total_size) {
      throw std::runtime_error{"Socket closed by remote"};
    }

    responses_.clear();
  }

 private:
  static constexpr std::size_t kMaxResponses = 16;

  [[nodiscard]] std::size_t Size() const { return responses_.size(); }

  struct Response final {
    HeadersString headers;
    std::string body;
  };

  boost::container::small_vector<Response, kMaxResponses> responses_;
};

constexpr std::string_view kCommonHeaders{"HTTP/1.1 200 OK\r\nServer: us\r\n"};
constexpr std::string_view kHeadersEnd{"\r\n\r\n"};

std::string MakeHttpDate(std::chrono::system_clock::time_point date) {
  static const std::string kFormatString = "%a, %d %b %Y %H:%M:%S %Z";
  static const auto tz = cctz::utc_time_zone();

  return cctz::format(kFormatString, date, tz);
}

std::string_view GetCachedDate() {
  constexpr size_t kMaxDateHeaderLength = 128;

  static thread_local std::chrono::seconds::rep last_second = 0;
  static thread_local char last_time_string[kMaxDateHeaderLength]{};
  static thread_local std::string_view result_view{};

  const auto now = userver::utils::datetime::WallCoarseClock::now();
  const auto now_seconds =
      std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch())
          .count();
  if (now_seconds != last_second) {
    last_second = now_seconds;

    const auto time_str = MakeHttpDate(now);

    std::memcpy(last_time_string, time_str.c_str(), time_str.size());
    result_view = std::string_view{last_time_string, time_str.size()};
  }

  return result_view;
}

}  // namespace

SimpleConnection::SimpleConnection(SimpleServer& server,
                                   userver::engine::io::Socket&& socket)
    : server_{server},
      socket_{std::move(socket)},
      processing_task_{userver::engine::AsyncNoSpan([this] { Process(); })} {}

SimpleConnection::~SimpleConnection() { processing_task_.SyncCancel(); }

void SimpleConnection::Process() {
  constexpr std::size_t kBufferSize = 4096;
  std::array<char, kBufferSize> buffer{};

  userver::utils::ScopeGuard close_guard{[this] { socket_.Close(); }};

  ResponseBuffers buffers{};
  const auto handle_request = [this, &buffers](std::string_view url) {
    auto response = server_.HandleRequest(url);
    const auto content_length_str = std::to_string(response.body.size());
    auto& headers = buffers.Next(socket_, std::move(response.body));

    headers.append(kCommonHeaders);
    headers.append("Content-Type: ");
    headers.append(response.content_type);

    headers.append("\r\nContent-Length: ");
    headers.append(content_length_str);

    headers.append("\r\nDate: ");
    headers.append(GetCachedDate());

    headers.append(kHeadersEnd);
  };
  HttpParser parser{handle_request};

  std::size_t last_bytes_read = 0;
  while (true) {
    bool is_readable = true;
    if (last_bytes_read < kBufferSize) {
      is_readable = socket_.WaitReadable({});
    }

    last_bytes_read =
        is_readable ? socket_.RecvSome(buffer.data(), kBufferSize, {}) : 0;
    if (last_bytes_read == 0) {
      break;
    }

    if (parser.Execute(buffer.data(), last_bytes_read) != HPE_OK) {
      break;
    }

    buffers.Send(socket_);
  }
}

}  // namespace userver_techempower::bare
