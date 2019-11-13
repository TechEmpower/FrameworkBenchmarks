
#include "Utils.hpp"

#include <chrono>

oatpp::String Utils::renderTime() {

  static thread_local time_t lastSecond = 0;
  static thread_local oatpp::String rederedTime;

  auto time = std::chrono::system_clock::now().time_since_epoch();
  time_t seconds = (time_t)std::chrono::duration_cast<std::chrono::seconds>(time).count();

  if(seconds != lastSecond) {

    lastSecond = seconds;

    struct tm now;
    gmtime_r(&seconds, &now);
    char buffer[50];
    auto size = std::strftime(buffer, sizeof(buffer), "%a, %d %b %Y %T GMT", &now);

    rederedTime = oatpp::String(buffer, size, true);

  }

  return rederedTime;

}