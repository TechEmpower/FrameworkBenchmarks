#pragma once

#include <userver/components/loggable_component_base.hpp>

#include "simple_response.hpp"

namespace userver_techempower {

namespace single_query {
class Handler;
}
namespace multiple_queries {
class Handler;
}
namespace updates {
class Handler;
}
namespace cached_queries {
class Handler;
}
namespace fortunes {
class Handler;
}

namespace bare {

class SimpleRouter final : public userver::components::LoggableComponentBase {
 public:
  static constexpr std::string_view kName{"simple-router"};

  SimpleRouter(const userver::components::ComponentConfig& config,
               const userver::components::ComponentContext& context);
  ~SimpleRouter() final;

  [[nodiscard]] SimpleResponse RouteRequest(std::string_view url) const;

 private:
  const single_query::Handler& single_query_;
  const multiple_queries::Handler& multiple_queries_;
  const updates::Handler& updates_;
  const cached_queries::Handler& cached_queries_;
  const fortunes::Handler& fortunes_;
};

}  // namespace bare
}  // namespace userver_techempower
