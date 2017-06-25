// world.h

#ifndef WORLD_H
#define WORLD_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>

class World {
public:
   // Check for memory error
   U_MEMORY_TEST

   // Allocator e Deallocator
   U_MEMORY_ALLOCATOR
   U_MEMORY_DEALLOCATOR

   uint32_t id, randomNumber;

   World()
      {
      U_TRACE_REGISTER_OBJECT(5, World, "")

      // coverity[uninit_ctor]
#  ifdef U_COVERITY_FALSE_POSITIVE
      id = randomNumber = 0;
#  endif
      }

   World(uint32_t _id, uint32_t _randomNumber) : id(_id), randomNumber(_randomNumber)
      {
      U_TRACE_REGISTER_OBJECT(5, World, "%u,%u", _id, _randomNumber)
      }

   ~World()
      {
      U_TRACE_UNREGISTER_OBJECT(5, World)
      }

   // JSON

   void toJSON(UString& json)
      {
      U_TRACE(0, "World::toJSON(%V)", json.rep)

      json.toJSON(U_JSON_METHOD_HANDLER(id,           unsigned int));
      json.toJSON(U_JSON_METHOD_HANDLER(randomNumber, unsigned int));
      }

   void fromJSON(UValue& json)
      {
      U_TRACE(0, "World::fromJSON(%p)", &json)

      json.fromJSON(U_JSON_METHOD_HANDLER(id,           unsigned int));
      json.fromJSON(U_JSON_METHOD_HANDLER(randomNumber, unsigned int));
      }

   // ORM

   void bindParam(UOrmStatement* stmt)
      {
      U_TRACE(0, "World::bindParam(%p)", stmt)

      stmt->bindParam(U_ORM_TYPE_HANDLER(id,           unsigned int));
      stmt->bindParam(U_ORM_TYPE_HANDLER(randomNumber, unsigned int));
      }

   void bindResult(UOrmStatement* stmt)
      {
      U_TRACE(0, "World::bindResult(%p)", stmt)

      stmt->bindResult(U_ORM_TYPE_HANDLER(id,           unsigned int));
      stmt->bindResult(U_ORM_TYPE_HANDLER(randomNumber, unsigned int));
      }

#ifdef DEBUG
   const char* dump(bool breset) const
      {
      *UObjectIO::os << "id           " << id            << '\n'
                     << "randomNumber " << randomNumber;

      if (breset)
         {
         UObjectIO::output();

         return UObjectIO::buffer_output;
         }

      return U_NULLPTR;
      }
#endif

private:
   U_DISALLOW_ASSIGN(World)
};
#endif
