#include "simple_connection.hpp"

#include <array>

#include <cctz/time_zone.h>
#include <http_parser.h>
#include <boost/container/small_vector.hpp>

#include "simple_server.hpp"

#include <userver/engine/async.hpp>
#include <userver/utils/datetime/wall_coarse_clock.hpp>
#include <userver/utils/scope_guard.hpp>

namespace userver_techempower::bare {

namespace {

template <std::size_t N>
class SmallString final {
 public:
  SmallString() = default;

  void Append(const char* data, std::size_t length) {
    const auto old_size = Size();
    data_.resize(old_size + length);
    std::memcpy(Data() + old_size, data, length);
  }

  void Append(std::string_view sw) { Append(sw.data(), sw.size()); }

  [[nodiscard]] std::string_view AsSw() const { return {Data(), Size()}; }

  [[nodiscard]] char* Data() { return data_.data(); }
  [[nodiscard]] const char* Data() const { return data_.data(); }

  [[nodiscard]] std::size_t Size() const { return data_.size(); }

  void Clear() { data_.resize(0); }

 private:
  boost::container::small_vector<char, N> data_;
};

struct HttpParser final {
  http_parser parser{};
  http_parser_settings parser_settings{};

  std::function<void(std::string_view)> on_request_cb{};

  SmallString<50> url;

  explicit HttpParser(std::function<void(std::string_view)> on_request_cb)
      : on_request_cb{std::move(on_request_cb)} {
    http_parser_init(&parser, HTTP_REQUEST);
    parser.data = this;

    http_parser_settings_init(&parser_settings);
    parser_settings.on_url = HttpOnUrl;
    parser_settings.on_message_begin = HttpOnMessageBegin;
    parser_settings.on_message_complete = HttpOnMessageComplete;
  }

  void Execute(const char* data, std::size_t length) {
    http_parser_execute(&parser, &parser_settings, data, length);
  }

  static int HttpOnUrl(http_parser* parser, const char* data,
                       std::size_t length) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->url.Append(data, length);
    return 0;
  }

  static int HttpOnMessageBegin(http_parser* parser) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->url.Clear();
    return 0;
  }

  static int HttpOnMessageComplete(http_parser* parser) {
    auto* self = static_cast<HttpParser*>(parser->data);
    self->on_request_cb(self->url.AsSw());
    return 0;
  }
};

class ResponseBuffers final {
 public:
  using HeadersString = SmallString<200>;

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

    boost::container::small_vector<userver::engine::io::IoData,
                                   kMaxResponses * 2>
        iovec(Size() * 2);

    std::size_t index = 0;
    std::size_t total_size = 0;
    for (const auto& response : responses_) {
      iovec[index++] = {response.headers.Data(), response.headers.Size()};
      iovec[index++] = {response.body.data(), response.body.size()};
      total_size += response.headers.Size() + response.body.size();
    }

    if (socket.SendAll(iovec.data(), iovec.size(), {}) != total_size) {
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

    headers.Append(kCommonHeaders);
    headers.Append("Content-Type: ");
    headers.Append(response.content_type);

    headers.Append("\r\nContent-Length: ");
    headers.Append(content_length_str);

    headers.Append("\r\nDate: ");
    headers.Append(GetCachedDate());

    headers.Append(kHeadersEnd);
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

    parser.Execute(buffer.data(), last_bytes_read);
    if (parser.parser.http_errno != 0) {
      break;
    }

    buffers.Send(socket_);
  }
}

}  // namespace userver_techempower::bare
