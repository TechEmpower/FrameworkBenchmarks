// world.h

#ifndef WORLD_H
#define WORLD_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>
#include <ulib/utility/uhttp.h>
#include <ulib/orm/orm_driver.h>
#include <ulib/net/server/client_image.h>

#ifdef U_STATIC_ORM_DRIVER_PGSQL
#  include <ulib/orm/driver/orm_driver_pgsql.h>
#endif

class World {
public:
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
      U_TRACE(5, "World::toJSON(%V)", json.rep)

      json.toJSON(U_JSON_METHOD_HANDLER(id,           unsigned int));
      json.toJSON(U_JSON_METHOD_HANDLER(randomNumber, unsigned int));
      }

   void fromJSON(UValue& json)
      {
      U_TRACE(5, "World::fromJSON(%p)", &json)

      json.fromJSON(U_JSON_METHOD_HANDLER(id,           unsigned int));
      json.fromJSON(U_JSON_METHOD_HANDLER(randomNumber, unsigned int));
      }

   // ORM

   void bindParam(UOrmStatement* stmt)
      {
      U_TRACE(5, "World::bindParam(%p)", stmt)

      stmt->bindParam(U_ORM_TYPE_HANDLER(id,           unsigned int));
      stmt->bindParam(U_ORM_TYPE_HANDLER(randomNumber, unsigned int));
      }

   void bindResult(UOrmStatement* stmt)
      {
      U_TRACE(5, "World::bindResult(%p)", stmt)

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
   static char wbuffer[18000];
   static uint32_t rnum, rnumber[500];

   static World*        pworld_query;
   static UOrmSession*    psql_query;
   static UOrmStatement* pstmt_query;

#ifdef U_STATIC_ORM_DRIVER_PGSQL
   static PGconn* conn;
   static UOrmDriverPgSql* pdrv;
   static UPgSqlStatement* pstmt;
   static char num2str[sizeof(unsigned int)];

   static bool initPipeline()
      {
      U_TRACE(5, "World::initPipeline()")

      if (pdrv)
         {
         (void) U_SYSCALL(PQsetnonblocking, "%p,%u", conn, 1);
         (void) U_SYSCALL(PQenterBatchMode, "%p",    conn);

         U_RETURN(true);
         }

      U_RETURN(false);
      }

   static PGresult* execPrepared()
      {
      U_TRACE_NO_PARAM(5, "World::execPrepared()")

      U_INTERNAL_ASSERT_MAJOR(rnumber[0], 0)

      *(unsigned int*)num2str = htonl(rnumber[0]);

      PGresult* res = (PGresult*) U_SYSCALL(PQexecPrepared, "%p,%S,%u,%p,%p,%p,%u", conn, pstmt->stmtName, 1, pstmt->paramValues, pstmt->paramLengths, pstmt->paramFormats, 1);

      U_RETURN_POINTER(res, PGresult);
      }

   static void sendQueryPrepared(uint32_t i)
      {
      U_TRACE(5, "World::sendQueryPrepared(%u)", i)

      U_INTERNAL_ASSERT_MAJOR(rnumber[i], 0)

      *(unsigned int*)num2str = htonl(rnumber[i]);

      (void) U_SYSCALL(PQsendQueryPrepared, "%p,%S,%u,%p,%p,%p,%u", conn, pstmt->stmtName, 1, pstmt->paramValues, pstmt->paramLengths, pstmt->paramFormats, 1);
      }
#endif

   static void initResult()
      {
      U_TRACE(5, "World::initResult()")

      u_put_unalignedp64(wbuffer,    U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
      u_put_unalignedp64(wbuffer+8,  U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
      u_put_unalignedp64(wbuffer+16, U_MULTICHAR_CONSTANT64('1','3','3','3','1','\r','\n','C'));
      u_put_unalignedp64(wbuffer+24, U_MULTICHAR_CONSTANT64('o','n','t','e','n','t','-','T'));
      u_put_unalignedp64(wbuffer+32, U_MULTICHAR_CONSTANT64('y','p','e',':',' ','a','p','p'));
      u_put_unalignedp64(wbuffer+40, U_MULTICHAR_CONSTANT64('l','i','c','a','t','i','o','n'));
      u_put_unalignedp64(wbuffer+48, U_MULTICHAR_CONSTANT64('/','j','s','o','n','\r','\n','\r'));
      u_put_unalignedp16(wbuffer+56, U_MULTICHAR_CONSTANT16('\n','['));

      pwbuffer = wbuffer + U_CONSTANT_SIZE("Content-Length: 13331\r\nContent-Type: application/json\r\n\r\n[");
      }

   static void endResult()
      {
      U_TRACE_NO_PARAM(5, "World::endResult()")

      *(pwbuffer-1) = ']';

      uint32_t      len = pwbuffer-wbuffer,
               body_len = len - U_CONSTANT_SIZE("Content-Length: 13331\r\nContent-Type: application/json\r\n\r\n");

      pwbuffer = u_num2str32(body_len, wbuffer + U_CONSTANT_SIZE("Content-Length: "));

      while (*pwbuffer != '\r') *pwbuffer++ = ' ';

      UClientImage_Base::wbuffer->setConstant(wbuffer, len);
      }

   static void initOneResult()
      {
      U_TRACE(5, "World::initOneResult()")

      u_put_unalignedp64(wbuffer,    U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
      u_put_unalignedp64(wbuffer+8,  U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
      u_put_unalignedp32(wbuffer+16, U_MULTICHAR_CONSTANT32('3','1','\r','\n'));
      u_put_unalignedp64(wbuffer+20, U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
      u_put_unalignedp64(wbuffer+28, U_MULTICHAR_CONSTANT64('T','y','p','e',':',' ','a','p'));
      u_put_unalignedp64(wbuffer+36, U_MULTICHAR_CONSTANT64('p','l','i','c','a','t','i','o'));
      u_put_unalignedp64(wbuffer+44, U_MULTICHAR_CONSTANT64('n','/','j','s','o','n','\r','\n'));
      u_put_unalignedp32(wbuffer+52, U_MULTICHAR_CONSTANT32('\r','\n','{','\0'));

      pwbuffer = wbuffer + U_CONSTANT_SIZE("Content-Length: 31\r\nContent-Type: application/json\r\n\r\n{");
      }

   static void endOneResult()
      {
      U_TRACE_NO_PARAM(5, "World::endOneResult()")

      *pwbuffer = '}';

      uint32_t      len = pwbuffer-wbuffer+1,
               body_len = len - U_CONSTANT_SIZE("Content-Length: 31\r\nContent-Type: application/json\r\n\r\n");

      (void) u_num2str32(body_len, wbuffer+U_CONSTANT_SIZE("Content-Length: "));

      UClientImage_Base::wbuffer->setConstant(wbuffer, len);
      }

   static void handlerOneResult(uint32_t uid, uint32_t random)
      {
      U_TRACE(5, "World::handlerOneResult(%u,%u)", uid, random)

      u_put_unalignedp32(pwbuffer, U_MULTICHAR_CONSTANT32('"','i','d','"'));

      pwbuffer[4] = ':';

      pwbuffer = u_num2str32(uid, pwbuffer+5);

      u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
      u_put_unalignedp64(pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));

      pwbuffer = u_num2str32(random, pwbuffer+16);
      }

   static void handlerResult(uint32_t uid, uint32_t random)
      {
      U_TRACE(5, "World::handlerResult(%u,%u)", uid, random)

      u_put_unalignedp32(pwbuffer,   U_MULTICHAR_CONSTANT32('{','"','i','d'));
      u_put_unalignedp16(pwbuffer+4, U_MULTICHAR_CONSTANT16('"',':'));

      pwbuffer = u_num2str32(uid, pwbuffer+6);

      u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
      u_put_unalignedp64(pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));

      pwbuffer = u_num2str32(random, pwbuffer+16);

      u_put_unalignedp16(pwbuffer, U_MULTICHAR_CONSTANT16('}',','));
                         pwbuffer += 2;
      }

   static void handlerResult(uint32_t i)
      {
      U_TRACE(5, "World::handlerResult(%u)", i)

      U_INTERNAL_ASSERT_POINTER(pworld_query)

      U_INTERNAL_DUMP("pworld_query->randomNumber = %u", pworld_query->randomNumber)
      }

   static void handlerResultSql(uint32_t i)
      {
      U_TRACE(5, "World::handlerResultSql(%u)", i)

      U_INTERNAL_ASSERT_POINTER(pworld_query)

      handlerResult(rnumber[i], pworld_query->randomNumber);
      }

   static void doUpdateNoSql(vPFu handlerUpdateNoSql)
      {
      U_TRACE(5, "World::doUpdateNoSql(%p)", handlerUpdateNoSql)

      initResult();

      for (uint32_t i = 0, n = UHTTP::getFormFirstNumericValue(1, 500); i < n; ++i)
         {
         handlerUpdateNoSql(i);

         handlerResult(rnumber[i], rnum);
         }

      endResult();
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(5, "World::handlerFork()")

      if (rnumber[0] == 0) for (uint32_t i = 0; i < 500; ++i) rnumber[i] = u_get_num_random_range1(10000);
      }

   static void handlerForkSql()
      {
      U_TRACE_NO_PARAM(5, "World::handlerForkSql()")

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

#     ifdef U_STATIC_ORM_DRIVER_PGSQL
         if (UOrmDriver::isPGSQL())
            {
            conn  = (PGconn*)(pdrv = (UOrmDriverPgSql*)psql_query->getDriver())->UOrmDriver::connection;
            pstmt = (UPgSqlStatement*)pstmt_query->getStatement();

            (void) pstmt->setBindParam(pdrv);

            pstmt->paramValues[0]  = num2str;
            pstmt->paramLengths[0] = sizeof(unsigned int);
            }
#     endif

         handlerFork();
         }
      }

private:
   U_DISALLOW_ASSIGN(World)
};
#endif
