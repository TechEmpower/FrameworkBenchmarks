
#ifndef DTOs_hpp
#define DTOs_hpp

#include "oatpp/core/data/mapping/type/Object.hpp"
#include "oatpp/core/macro/codegen.hpp"

#include OATPP_CODEGEN_BEGIN(DTO)

class MessageDto : public oatpp::data::mapping::type::Object {

  DTO_INIT(MessageDto, Object)

  DTO_FIELD(String, message);

};

#include OATPP_CODEGEN_END(DTO)

#endif /* DTOs_hpp */
