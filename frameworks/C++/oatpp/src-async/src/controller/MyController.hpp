
#ifndef MyController_hpp
#define MyController_hpp

#include "dto/DTOs.hpp"

#include "oatpp/web/server/api/ApiController.hpp"
#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/macro/component.hpp"

#include <chrono>

/**
 * Sample Api Controller.
 */
class MyController : public oatpp::web::server::api::ApiController {
private:
  typedef MyController __ControllerType;
public:
  /**
   * Constructor with object mapper.
   * @param objectMapper - default object mapper used to serialize/deserialize DTOs.
   */
  MyController(OATPP_COMPONENT(std::shared_ptr<ObjectMapper>, objectMapper))
    : oatpp::web::server::api::ApiController(objectMapper)
  {}
public:
  
/**
 *  Begin ENDPOINTs generation ('ApiController' codegen)
 */
#include OATPP_CODEGEN_BEGIN(ApiController)

  static oatpp::String renderTime() {

    static thread_local time_t lastSecond = 0;
    static thread_local oatpp::String rederedTime;
    static int count = 0;

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

  ENDPOINT_ASYNC("GET", "/plaintext", Plaintext) {

    ENDPOINT_ASYNC_INIT(Plaintext)

    Action act() override {
      const auto& response = controller->createResponse(Status::CODE_200, oatpp::String("Hello, World!", 13, false));
      response->putHeader(Header::CONTENT_TYPE, oatpp::data::share::StringKeyLabel(nullptr, (p_char8)"text/plain", 10));
      response->putHeader("Date", renderTime());
      return _return(response);
    }

  };

  ENDPOINT_ASYNC("GET", "/json", Json) {
    
    ENDPOINT_ASYNC_INIT(Json)
    
    Action act() override {
      auto dto = MessageDto::createShared();
      dto->message = oatpp::String("Hello, World!", 13, false);
      const auto& response = controller->createDtoResponse(Status::CODE_200, dto);
      response->putHeader("Date", renderTime());
      return _return(response);
    }
    
  };
  
/**
 *  Finish ENDPOINTs generation ('ApiController' codegen)
 */
#include OATPP_CODEGEN_END(ApiController)
  
};

#endif /* MyController_hpp */
