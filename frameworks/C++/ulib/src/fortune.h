// fortune.h

#ifndef FORTUNE_H
#define FORTUNE_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>

class Fortune {
public:
   // Check for memory error
   U_MEMORY_TEST

   // Allocator e Deallocator
   U_MEMORY_ALLOCATOR
   U_MEMORY_DEALLOCATOR

   uint32_t id;
   UString message;

   Fortune()
      {
      U_TRACE_REGISTER_OBJECT(5, Fortune, "")

      // coverity[uninit_ctor]
#  ifdef U_COVERITY_FALSE_POSITIVE
      id = 0;
#  endif
      }

   Fortune(uint32_t _id, const UString& _message) : id(_id), message(_message)
      {
      U_TRACE_REGISTER_OBJECT(5, Fortune, "%u,%V", _id, _message.rep)
      }

   Fortune(const Fortune& f) : id(f.id), message((void*)U_STRING_TO_PARAM(f.message))
      {
      U_TRACE_REGISTER_OBJECT(5, Fortune, "%p", &f)

      U_MEMORY_TEST_COPY(f)
      }

   ~Fortune()
      {
      U_TRACE_UNREGISTER_OBJECT(5, Fortune)
      }

   // SERVICE

   bool operator<(const Fortune& other) const { return cmp_obj(&message, &other.message); }

   static int cmp_obj(const void* a, const void* b)
      {
      U_TRACE(5, "Fortune::cmp_obj(%p,%p)", a, b)

#  ifdef U_STDCPP_ENABLE
      /**
       * The comparison function must follow a strict-weak-ordering
       *
       * 1) For all x, it is not the case that x < x (irreflexivity)
       * 2) For all x, y, if x < y then it is not the case that y < x (asymmetry)
       * 3) For all x, y, and z, if x < y and y < z then x < z (transitivity)
       * 4) For all x, y, and z, if x is incomparable with y, and y is incomparable with z, then x is incomparable with z (transitivity of incomparability)
       */

      return (((const Fortune*)a)->message.compare(((const Fortune*)b)->message) < 0);
#  else
      return (*(const Fortune**)a)->message.compare((*(const Fortune**)b)->message);
#  endif
      }

   // JSON

   void toJSON(UString& json)
      {
      U_TRACE(0, "Fortune::toJSON(%V)", json.rep)

      json.toJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.toJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   void fromJSON(UValue& json)
      {
      U_TRACE(0, "Fortune::fromJSON(%p)", &json)

      json.fromJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.fromJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   // ORM

   void bindParam(UOrmStatement* stmt)
      {
      U_TRACE(0, "Fortune::bindParam(%p)", stmt)

      stmt->bindParam(U_ORM_TYPE_HANDLER(id,      unsigned int));
      stmt->bindParam(U_ORM_TYPE_HANDLER(message, UString));
      }

   void bindResult(UOrmStatement* stmt)
      {
      U_TRACE(0, "Fortune::bindResult(%p)", stmt)

      stmt->bindResult(U_ORM_TYPE_HANDLER(id,      unsigned int));
      stmt->bindResult(U_ORM_TYPE_HANDLER(message, UString));
      }

#ifdef DEBUG
   const char* dump(bool breset) const
      {
      *UObjectIO::os << "id               " << id              << '\n'
                     << "message (UString " << (void*)&message << ')';

      if (breset)
         {
         UObjectIO::output();

         return UObjectIO::buffer_output;
         }

      return U_NULLPTR;
      }
#endif

private:
   U_DISALLOW_ASSIGN(Fortune)
};
#endif
