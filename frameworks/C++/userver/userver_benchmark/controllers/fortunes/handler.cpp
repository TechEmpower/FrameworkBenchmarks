#include "handler.hpp"

#include <vector>

#include "../../common/db_helpers.hpp"

#include <userver/components/component_context.hpp>
#include <userver/storages/postgres/postgres.hpp>

namespace userver_techempower::fortunes {

namespace {

struct Fortune final {
  int id;
  std::string message;
};

constexpr std::string_view kResultingHtmlHeader{
    "<!DOCTYPE "
    "html><html><head><title>Fortunes</title></head><body><table><tr><th>id</"
    "th><th>message</th></tr>"};
constexpr std::string_view kResultingHtmlFooter{"</table></body></html>"};

constexpr std::string_view kNewRowStart{"<tr><td>"};
constexpr std::string_view kColumnsSeparator{"</td><td>"};
constexpr std::string_view kNewRowEnd{"</td></tr>"};

constexpr std::string_view kEscapedQuote{"&quot;"};
constexpr std::string_view kEscapedAmpersand{"&amp;"};
constexpr std::string_view kEscapedLessThanSign{"&lt;"};
constexpr std::string_view kEscapedMoreThanSign{"&gt;"};

void AppendFortune(std::string& result, const Fortune& fortune) {
  {
    auto old_size = result.size();
    const auto fortune_id = std::to_string(fortune.id);

    const auto first_step_size =
        kNewRowStart.size() + fortune_id.size() + kColumnsSeparator.size();

    result.resize(old_size + first_step_size);
    char* append_position = result.data() + old_size;

    // this is just faster than std::string::append if we know the resulting
    // size upfront, because there are a lot of not inlined calls otherwise
    const auto append = [&append_position](std::string_view what) {
      std::memcpy(append_position, what.data(), what.size());
      append_position += what.size();
    };
    append(kNewRowStart);
    append(fortune_id);
    append(kColumnsSeparator);
  }

  {
    std::string_view message{fortune.message};

    const auto do_append = [&result](std::string_view unescaped,
                                     std::string_view escaped) {
      const auto old_size = result.size();
      const auto added_size = unescaped.size() + escaped.size();

      result.resize(result.size() + added_size);
      char* append_position = result.data() + old_size;
      if (!unescaped.empty()) {
        std::memcpy(append_position, unescaped.data(), unescaped.size());
        append_position += unescaped.size();
      }
      std::memcpy(append_position, escaped.data(), escaped.size());
    };

    std::size_t unescaped_len = 0;
    const auto append = [&unescaped_len, &message,
                         &do_append](std::string_view escaped) {
      do_append(message.substr(0, unescaped_len), escaped);
      message = message.substr(std::exchange(unescaped_len, 0) + 1);
    };

    while (unescaped_len < message.size()) {
      const auto c = message[unescaped_len];
      switch (c) {
        case '"': {
          append(kEscapedQuote);
          break;
        }
        case '&': {
          append(kEscapedAmpersand);
          break;
        }
        case '<': {
          append(kEscapedLessThanSign);
          break;
        }
        case '>': {
          append(kEscapedMoreThanSign);
          break;
        }
        default:
          ++unescaped_len;
      }
    }
    result.append(message);
  }

  { result.append(kNewRowEnd); }
}

std::string FormatFortunes(const std::vector<Fortune>& fortunes) {
  std::string result{};
  // Wild guess, seems reasonable. Could be the exact value needed, but that
  // looks kinda cheating.
  result.reserve(2048);

  result.append(kResultingHtmlHeader);
  for (const auto& fortune : fortunes) {
    AppendFortune(result, fortune);
  }
  result.append(kResultingHtmlFooter);

  return result;
}

}  // namespace

Handler::Handler(const userver::components::ComponentConfig& config,
                 const userver::components::ComponentContext& context)
    : userver::server::handlers::HttpHandlerBase{config, context},
      pg_{context
              .FindComponent<userver::components::Postgres>(
                  db_helpers::kDbComponentName)
              .GetCluster()},
      select_all_fortunes_query_{"SELECT id, message FROM Fortune"} {}

std::string Handler::HandleRequestThrow(
    const userver::server::http::HttpRequest& request,
    userver::server::request::RequestContext&) const {
  request.GetHttpResponse().SetContentType("text/html; charset=utf-8");
  return GetResponse();
}

std::string Handler::GetResponse() const {
  auto fortunes =
      pg_->Execute(db_helpers::kClusterHostType, select_all_fortunes_query_)
          .AsContainer<std::vector<Fortune>>(
              userver::storages::postgres::kRowTag);

  fortunes.push_back({0, "Additional fortune added at request time."});

  std::sort(fortunes.begin(), fortunes.end(),
            [](const auto& lhs, const auto& rhs) {
              return lhs.message < rhs.message;
            });

  return FormatFortunes(fortunes);
}

}  // namespace userver_techempower::fortunes
