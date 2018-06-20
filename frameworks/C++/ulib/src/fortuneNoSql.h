// fortuneNoSql.h

#ifndef FORTUNE_NO_SQL_H
#define FORTUNE_NO_SQL_H 1

#include "fortune.h"

#include <ulib/net/client/redis.h>
#include <ulib/net/client/elasticsearch.h>

#ifdef USE_MONGODB
#  include <ulib/net/client/mongodb.h>
#endif

class FortuneNoSql {
public:

#ifdef USE_MONGODB
   static UMongoDBClient* mc;
#endif

   static void handlerQueryMongoDB()
      {
      U_TRACE(0, "FortuneNoSql::handlerQueryMongoDB()")

#  ifdef USE_MONGODB
      (void) mc->findAll();

      for (uint32_t i = 0, n = mc->vitem.size(); i < n; ++i)
         {
         Fortune* item;
         UString result;

         (void) U_JFIND(mc->vitem[i], "message", result);

         U_NEW(Fortune, item, Fortune(i+1, result));

         Fortune::pvfortune->push(item);
         }
#  endif
      }

   static void handlerForkMongoDB()
      {
      U_TRACE_NO_PARAM(0, "FortuneNoSql::handlerForkMongoDB()")

#  ifdef USE_MONGODB
      if (mc == U_NULLPTR)
         {
         U_NEW(UMongoDBClient, mc, UMongoDBClient);

         if (mc->connect(U_NULLPTR, 0) == false)
            {
            U_WARNING("FortuneNoSql::handlerForkMongoDB(): connection failed");

            U_DELETE(mc)

            mc = U_NULLPTR;

            return;
            }

         if (mc->selectCollection("hello_world", "fortune") == false)
            {
            U_WARNING("FortuneNoSql::handlerForkMongoDB(): selectCollection() failed");

            U_DELETE(mc)

            mc = U_NULLPTR;

            return;
            }

         Fortune::handlerFork();
         }
#  endif
      }

   static UREDISClient_Base* rc;

   static void handlerQueryREDIS()
      {
      U_TRACE(0, "FortuneNoSql::handlerQueryREDIS()")

      (void) rc->lrange(U_CONSTANT_TO_PARAM("fortunes 0 -1"));

      for (uint32_t i = 0, n = rc->vitem.size(); i < n; ++i)
         {
         Fortune* item;

         U_NEW(Fortune, item, Fortune(i+1, rc->vitem[i]));

         Fortune::pvfortune->push(item);
         }
      }

   static void handlerForkREDIS()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerForkREDIS()")

      if (rc == U_NULLPTR)
         {
         U_NEW(UREDISClient<UTCPSocket>, rc, UREDISClient<UTCPSocket>);

         if (rc->connect() == false)
            {
            U_WARNING("FortuneNoSql::handlerForkREDIS(): %V", rc->UClient_Base::getResponse().rep);

            U_DELETE(rc)

            rc = U_NULLPTR;

            return;
            }

         Fortune::handlerFork();
         }
      }

#ifdef DEBUG
   static void handlerEndMongoDB()
      {
      U_TRACE_NO_PARAM(0, "FortuneNoSql::handlerEndMongoDB()")

#  ifdef USE_MONGODB
      if (mc)
         {
         Fortune::handlerEnd();

         U_DELETE(mc)

         mc = U_NULLPTR;
         }
#  endif
      }

   static void handlerEndREDIS()
      {
      U_TRACE_NO_PARAM(0, "FortuneNoSql::handlerEndREDIS()")

      if (rc)
         {
         Fortune::handlerEnd();

         U_DELETE(rc)

         rc = U_NULLPTR;
         }
      }
#endif

private:
   U_DISALLOW_ASSIGN(FortuneNoSql)
};
#endif
