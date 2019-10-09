
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

  ENDPOINT("GET", "/json", Json) {
    auto dto = MessageDto::createShared();
    dto->message = oatpp::String("Hello, World!", 13, false);
    const auto& response = createDtoResponse(Status::CODE_200, dto);
    response->putHeader("Date", Utils::renderTime());
    return response;
  }
  
/**
 *  Finish ENDPOINTs generation ('ApiController' codegen)
 */
#include OATPP_CODEGEN_END(ApiController)
  
};

#endif /* MyController_hpp */
