// worldNoSql.h

#ifndef WORLD_NO_SQL_H
#define WORLD_NO_SQL_H 1

#include "world.h"

#include <ulib/net/client/redis.h>
#include <ulib/net/client/elasticsearch.h>

#ifdef USE_MONGODB
#  include <ulib/net/client/mongodb.h>
#endif

class WorldNoSql {
public:

   static UString* str_rnumber;

   static void handlerOneResult(uint32_t uid)
      {
      U_TRACE(0, "WorldNoSql::handlerOneResult(%u)", uid)

      U_INTERNAL_ASSERT_POINTER(str_rnumber)
      U_INTERNAL_ASSERT_POINTER(World::pwbuffer)

      u_put_unalignedp32(World::pwbuffer, U_MULTICHAR_CONSTANT32('"','i','d','"'));

      World::pwbuffer[4] = ':';

      World::pwbuffer = u_num2str32(uid, World::pwbuffer+5);

      u_put_unalignedp64(World::pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
      u_put_unalignedp64(World::pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));
                         World::pwbuffer += 16;

      uint32_t sz = str_rnumber->size();

      (void) memcpy(World::pwbuffer, str_rnumber->data(), sz);
                    World::pwbuffer +=                    sz;

      str_rnumber->clear();
      }

   static void handlerResult(uint32_t uid)
      {
      U_TRACE(0, "WorldNoSql::handlerResult(%u)", uid)

      U_INTERNAL_ASSERT_POINTER(str_rnumber)
      U_INTERNAL_ASSERT_POINTER(World::pwbuffer)

      u_put_unalignedp32(World::pwbuffer,   U_MULTICHAR_CONSTANT32('{','"','i','d'));
      u_put_unalignedp16(World::pwbuffer+4, U_MULTICHAR_CONSTANT16('"',':'));

      World::pwbuffer = u_num2str32(uid, World::pwbuffer+6);

      u_put_unalignedp64(World::pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
      u_put_unalignedp64(World::pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));
                         World::pwbuffer += 16;

      uint32_t sz = str_rnumber->size();

      (void) memcpy(World::pwbuffer, str_rnumber->data(), sz);
                    World::pwbuffer +=                    sz;

      str_rnumber->clear();

      u_put_unalignedp16(World::pwbuffer, U_MULTICHAR_CONSTANT16('}',','));
                         World::pwbuffer += 2;
      }

   static void doOneQuery(vPFu handlerQuery)
      {
      U_TRACE(0, "WorldNoSql::doOneQuery(%p)", handlerQuery)

      World::initOneResult();

      handlerQuery(World::rnumber[0]);

      handlerOneResult(World::rnumber[0]);

      World::endOneResult();
      }

   static void doQuery(vPFu handlerQuery)
      {
      U_TRACE(0, "WorldNoSql::doQuery(%p)", handlerQuery)

      World::initResult();

      for (uint32_t i = 0, n = UHTTP::getFormFirstNumericValue(1, 500); i < n; ++i)
         {
         handlerQuery(World::rnumber[i]);

         handlerResult(World::rnumber[i]);
         }

      World::endResult();
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerFork()")

      if (str_rnumber == U_NULLPTR) U_NEW_STRING(str_rnumber, UString);

      World::handlerFork();

      U_INTERNAL_ASSERT_POINTER(str_rnumber)
      }

#ifdef USE_MONGODB
   static bson_t* query;
   static UMongoDBClient* mc;
#endif

   static void handlerQueryMongoDB(uint32_t uid)
      {
      U_TRACE(0, "WorldNoSql::handlerQueryMongoDB(%u)", uid)

      U_INTERNAL_ASSERT_POINTER(str_rnumber)

#  ifdef USE_MONGODB
      (void) mc->findOne(uid, query);
      (void) U_JFIND(mc->vitem[0], "randomNumber", *str_rnumber);

      uint32_t pos = str_rnumber->find_first_of('.');

      if (pos != U_NOT_FOUND) str_rnumber->size_adjust_constant(pos);
#  endif
      }

   static void handlerUpdateMongoDB(uint32_t i)
      {
      U_TRACE(0, "WorldNoSql::handlerUpdateMongoDB(%u)", i)

#  ifdef USE_MONGODB
      (void) mc->findOne(World::rnumber[i], query);
      (void) mc->update( World::rnumber[i], "randomNumber", World::rnum = u_get_num_random_range1(10000));
#  endif
      }

   static void handlerForkMongoDB()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerForkMongoDB()")

#  ifdef USE_MONGODB
      if (mc == U_NULLPTR)
         {
         U_NEW(UMongoDBClient, mc, UMongoDBClient);

         if (mc->connect(U_NULLPTR, 0) == false)
            {
            U_WARNING("WorldNoSql::handlerForkMongoDB(): connection failed");

            U_DELETE(mc)

            mc = U_NULLPTR;

            return;
            }

         if (mc->selectCollection("hello_world", "world") == false)
            {
            U_WARNING("WorldNoSql::handlerForkMongoDB(): selectCollection() failed");

            U_DELETE(mc)

            mc = U_NULLPTR;

            return;
            }

         query = (bson_t*) U_SYSCALL_NO_PARAM(bson_new);  

         handlerFork();
         }
#  endif
      }

   static char rc_buffer[128];
   static UREDISClient_Base* rc;

   static void handlerQueryREDIS(uint32_t uid)
      {
      U_TRACE(0, "WorldNoSql::handlerQueryREDIS(%u)", uid)

      U_INTERNAL_ASSERT_POINTER(str_rnumber)

      char* ptr = rc_buffer+U_CONSTANT_SIZE("world:");

      (void) rc->get(ptr, U_CONSTANT_SIZE("world:")+u_num2str32(uid, ptr)-ptr);

      *str_rnumber = rc->vitem[0];
      }

   static void handlerUpdateREDIS(uint32_t i)
      {
      U_TRACE(0, "WorldNoSql::handlerUpdateREDIS(%u)", i)

      char* start = rc_buffer+U_CONSTANT_SIZE("world:");
      char* ptr = u_num2str32(World::rnumber[i], start);

      (void) rc->get(start, ptr-start);

      *ptr = ' ';
       ptr = u_num2str32(World::rnum = u_get_num_random_range1(10000), ptr+1);

      (void) rc->mset(start, ptr-start);
      }

   static void handlerForkREDIS()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerForkREDIS()")

      if (rc == U_NULLPTR)
         {
         U_NEW(UREDISClient<UTCPSocket>, rc, UREDISClient<UTCPSocket>);

         if (rc->connect() == false)
            {
            U_WARNING("WorldNoSql::handlerForkREDIS(): %V", rc->UClient_Base::getResponse().rep);

            U_DELETE(rc)

            rc = U_NULLPTR;

            return;
            }

         U_MEMCPY(rc_buffer, "world:", U_CONSTANT_SIZE("world:"));

         handlerFork();
         }
      }

   static char* pbuffer1;
   static char* pbuffer2;
   static char es_buffer1[128];
   static char es_buffer2[128];
   static UElasticSearchClient* es;

#  define U_QLEN U_CONSTANT_SIZE("{\"query\":{\"match\":{\"_id\":\"")

   static void handlerQueryElasticSearch(uint32_t uid)
      {
      U_TRACE(0, "WorldNoSql::handlerQueryElasticSearch(%u)", uid)

      U_INTERNAL_ASSERT_POINTER(str_rnumber)

      (void) es->sendPOST(U_CONSTANT_TO_PARAM("/tfb/world/_search"), es_buffer1, U_QLEN+
                                                         u__snprintf(es_buffer1+ U_QLEN,
                                                              sizeof(es_buffer1)-U_QLEN, U_CONSTANT_TO_PARAM("%u\"}}}"), uid));

      (void) U_JFIND(es->getContent(), "randomNumber", *str_rnumber);
      }

   static void handlerUpdateElasticSearch(uint32_t i)
      {
      U_TRACE(0, "WorldNoSql::handlerUpdateElasticSearch(%u)", i)

      uint32_t len1 = u__snprintf(pbuffer1, 100, U_CONSTANT_TO_PARAM("%u/_update"), World::rnumber[i]),
               len2 = u__snprintf(pbuffer2, 100, U_CONSTANT_TO_PARAM("%u\"}}"), World::rnum = u_get_num_random_range1(10000));

      (void) es->sendPOST(es_buffer1, len1+U_CONSTANT_SIZE("/tfb/world/"), es_buffer2, len2+U_CONSTANT_SIZE("{\"doc\":{\"_id\":\""));
      }

   static void handlerForkElasticSearch()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerForkElasticSearch()")

      if (es == U_NULLPTR)
         {
         U_NEW(UElasticSearchClient, es, UElasticSearchClient);

         if (es->connect() == false)
            {
            U_WARNING("WorldNoSql::handlerForkElasticSearch(): connection disabled or failed");

            U_DELETE(es)

            es = U_NULLPTR;

            return;
            }

         U_MEMCPY(es_buffer1, "{\"query\":{\"match\":{\"_id\":\"", U_QLEN);

         handlerFork();
         }
      }

#ifdef DEBUG
   static void handlerEnd()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerEnd()")

      if (str_rnumber)
         {
         U_DELETE(str_rnumber)

         str_rnumber = U_NULLPTR;
         }
      }

   static void handlerEndMongoDB()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerEndMongoDB()")

#  ifdef USE_MONGODB
      if (query)
         {
         U_DELETE(mc)

         U_SYSCALL_VOID(bson_destroy, "%p", query);

         query = U_NULLPTR;

         handlerEnd();
         }
#  endif
      }

   static void handlerEndREDIS()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerEndREDIS()")

      if (rc)
         {
         U_DELETE(rc)

         rc = U_NULLPTR;

         handlerEnd();
         }
      }

   static void handlerEndElasticSearch()
      {
      U_TRACE_NO_PARAM(0, "WorldNoSql::handlerEndElasticSearch()")

      if (es)
         {
         U_DELETE(es)

         es = U_NULLPTR;

         handlerEnd();
         }
      }
#endif

private:
   U_DISALLOW_ASSIGN(WorldNoSql)
};
#endif
