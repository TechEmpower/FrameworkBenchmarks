
#ifndef MyController_hpp
#define MyController_hpp

#include "dto/DTOs.hpp"

#include "Utils.hpp"

#include "oatpp/web/server/api/ApiController.hpp"
#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/macro/component.hpp"

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

  ENDPOINT_ASYNC("GET", "/plaintext", Plaintext) {

    ENDPOINT_ASYNC_INIT(Plaintext)

    Action act() override {
      const auto& response = controller->createResponse(Status::CODE_200, oatpp::String("Hello, World!", 13, false));
      response->putHeader(Header::CONTENT_TYPE, oatpp::data::share::StringKeyLabel(nullptr, (p_char8)"text/plain", 10));
      response->putHeader("Date", Utils::renderTime());
      return _return(response);
    }

  };

  ENDPOINT_ASYNC("GET", "/json", Json) {
    
    ENDPOINT_ASYNC_INIT(Json)
    
    Action act() override {
      auto dto = MessageDto::createShared();
      dto->message = oatpp::String("Hello, World!", 13, false);
      const auto& response = controller->createDtoResponse(Status::CODE_200, dto);
      response->putHeader("Date", Utils::renderTime());
      return _return(response);
    }
    
  };
  
/**
 *  Finish ENDPOINTs generation ('ApiController' codegen)
 */
#include OATPP_CODEGEN_END(ApiController)
  
};

#endif /* MyController_hpp */
