// world.h

#ifndef WORLD_H
#define WORLD_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>
#include <ulib/utility/uhttp.h>
#include <ulib/orm/orm_driver.h>
#include <ulib/net/server/client_image.h>

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

   static char* pwbuffer;
   static uint32_t rnum, rnumber[500];

   static World*        pworld_query;
   static UOrmSession*    psql_query;
   static UOrmStatement* pstmt_query;

   static void initResult(uint32_t num_queries)
      {
      U_TRACE(0, "World::initResult(%u)", num_queries)

      (void) UClientImage_Base::wbuffer->reserve(36U * num_queries);

      pwbuffer = UClientImage_Base::wbuffer->pend();

      *pwbuffer++ = '[';
      }

   static void endResult()
      {
      U_TRACE_NO_PARAM(0, "World::endResult()")

      U_INTERNAL_ASSERT_POINTER(pwbuffer)

      *(pwbuffer-1) = ']';

      UClientImage_Base::wbuffer->size_adjust(pwbuffer);
      }

   static void endOneResult()
      {
      U_TRACE_NO_PARAM(0, "World::endOneResult()")

      U_INTERNAL_ASSERT_POINTER(pwbuffer)

      *pwbuffer++ = '}';

      UClientImage_Base::wbuffer->size_adjust(pwbuffer);
      }

   static void handlerOneResult(uint32_t uid, uint32_t random)
      {
      U_TRACE(0, "World::handlerOneResult(%u,%u)", uid, random)

      U_INTERNAL_ASSERT_POINTER(pwbuffer)

      u_put_unalignedp32(pwbuffer,   U_MULTICHAR_CONSTANT32('{','"','i','d'));
      u_put_unalignedp16(pwbuffer+4, U_MULTICHAR_CONSTANT16('"',':'));

      pwbuffer = u_num2str32(uid, pwbuffer+6);

      u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
      u_put_unalignedp64(pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));

      pwbuffer = u_num2str32(random, pwbuffer+16);
      }

   static void handlerResult(uint32_t uid, uint32_t random)
      {
      U_TRACE(0, "World::handlerResult(%u,%u)", uid, random)

      handlerOneResult(uid, random);

      u_put_unalignedp16(pwbuffer, U_MULTICHAR_CONSTANT16('}',','));
                         pwbuffer += 2;
      }

   static void handlerResult(uint32_t i)
      {
      U_TRACE(0, "World::handlerResult(%u)", i)

      U_INTERNAL_ASSERT_POINTER(pworld_query)

      U_INTERNAL_DUMP("pworld_query->randomNumber = %u", pworld_query->randomNumber)
      }

   static void handlerResultSql(uint32_t i)
      {
      U_TRACE(0, "World::handlerResultSql(%u)", i)

      U_INTERNAL_ASSERT_POINTER(pworld_query)

      handlerResult(rnumber[i], pworld_query->randomNumber);
      }

   static void doUpdateNoSql(vPFu handlerUpdateNoSql)
      {
      U_TRACE(0, "World::doUpdateNoSql(%p)", handlerUpdateNoSql)

      uint32_t num_queries = UHTTP::getFormFirstNumericValue(1, 500);

      initResult(num_queries);

      for (uint32_t i = 0; i < num_queries; ++i)
         {
         handlerUpdateNoSql(i);

         handlerResult(rnumber[i], rnum);
         }

      endResult();
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(0, "World::handlerFork()")

      if (rnumber[0] == 0) for (uint32_t i = 0; i < 500; ++i) rnumber[i] = u_get_num_random(10000-1);
      }

   static void handlerForkSql()
      {
      U_TRACE_NO_PARAM(0, "World::handlerForkSql()")

      if (psql_query == U_NULLPTR)
         {
         U_NEW(UOrmSession, psql_query, UOrmSession(U_CONSTANT_TO_PARAM("hello_world")));

         if (psql_query->isReady() == false)
            {
            U_WARNING("World::handlerForkSql(): we cound't connect to db");

            U_DELETE(psql_query)

            psql_query = U_NULLPTR;

            return;
            }

         U_NEW(UOrmStatement, pstmt_query, UOrmStatement(*psql_query, U_CONSTANT_TO_PARAM("SELECT randomNumber FROM World WHERE id = ?")));

         U_NEW(World, pworld_query, World);

         pstmt_query->use( pworld_query->id);
         pstmt_query->into(pworld_query->randomNumber);

         handlerFork();
         }
      }

#ifdef DEBUG
   static void handlerEndSql()
      {
      U_TRACE_NO_PARAM(0, "World::handlerEndSql()")

      if (pstmt_query)
         {
         U_DELETE( pstmt_query)
         U_DELETE(pworld_query)
         U_DELETE(  psql_query)

         pstmt_query = U_NULLPTR;
         }
      }

   const char* dump(bool breset) const;
#endif

private:
   U_DISALLOW_ASSIGN(World)
};
#endif
