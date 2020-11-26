
#ifndef DTOs_hpp
#define DTOs_hpp

#include "oatpp/core/Types.hpp"
#include "oatpp/core/macro/codegen.hpp"

#include OATPP_CODEGEN_BEGIN(DTO)

class MessageDto : public oatpp::DTO {

  DTO_INIT(MessageDto, DTO)

  DTO_FIELD(String, message);

};

#include OATPP_CODEGEN_END(DTO)

#endif /* DTOs_hpp */
