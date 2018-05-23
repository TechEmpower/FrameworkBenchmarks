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
      U_TRACE_CTOR(5, World, "")

      // coverity[uninit_ctor]
#  ifdef U_COVERITY_FALSE_POSITIVE
      id = randomNumber = 0;
#  endif
      }

   World(uint32_t _id, uint32_t _randomNumber) : id(_id), randomNumber(_randomNumber)
      {
      U_TRACE_CTOR(5, World, "%u,%u", _id, _randomNumber)
      }

   World(const World& w) : id(w.id), randomNumber(w.randomNumber)
      {
      U_TRACE_CTOR(5, World, "%p", &w)

      U_MEMORY_TEST_COPY(w)
      }

   ~World()
      {
      U_TRACE_DTOR(5, World)
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

   // SERVICE

   bool operator<(const World& other) const { return cmp_obj(&id, &other.id); }

   static int cmp_obj(const void* a, const void* b)
      {
      U_TRACE(5, "World::cmp_obj(%p,%p)", a, b)

#  ifdef U_STDCPP_ENABLE
      /**
       * The comparison function must follow a strict-weak-ordering
       *
       * 1) For all x, it is not the case that x < x (irreflexivity)
       * 2) For all x, y, if x < y then it is not the case that y < x (asymmetry)
       * 3) For all x, y, and z, if x < y and y < z then x < z (transitivity)
       * 4) For all x, y, and z, if x is incomparable with y, and y is incomparable with z, then x is incomparable with z (transitivity of incomparability)
       */

      return (((const World*)a)->id < (((const World*)b)->id));
#  else
      return (*(const World**)a)->id < ((*(const World**)b)->id);
#  endif
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
