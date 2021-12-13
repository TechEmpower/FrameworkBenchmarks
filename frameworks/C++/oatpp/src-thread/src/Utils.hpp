
#ifndef OATPP_ASYNC_TEST_UTILS_HPP
#define OATPP_ASYNC_TEST_UTILS_HPP

#include "oatpp/core/Types.hpp"


class Utils {
public:

  /**
   * Currently oatpp has no default implementation for server time rendering.
   * Implementing custom server time rendering for test purposes.
   * @return
   */
  static oatpp::String renderTime();

};


#endif //OATPP_ASYNC_TEST_UTILS_HPP
