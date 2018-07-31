// fortuneNoSql.h

#ifndef FORTUNE_NO_SQL_H
#define FORTUNE_NO_SQL_H 1

#include "fortune.h"

#include <ulib/net/client/redis.h>
#include <ulib/net/client/elasticsearch.h>

#ifdef USE_MONGODB
#  include <ulib/net/client/mongodb.h>
#endif

class U_EXPORT FortuneNoSql {
public:

#ifdef USE_MONGODB
	static UMongoDBClient* mc;
#endif

	static void handlerQueryMongoDB()
		{
		U_TRACE(5, "FortuneNoSql::handlerQueryMongoDB()")

		U_INTERNAL_ASSERT_POINTER(Fortune::pmessage)

#	ifdef USE_MONGODB
		(void) mc->findAll();

		for (uint32_t i = 0, n = mc->vitem.size(); i < n; ++i)
			{
			(void) U_JFIND(mc->vitem[i], "message", *Fortune::pmessage);

			Fortune::replace(i, i+1);

			Fortune::pmessage->clear();
			}
#	endif
		}

	static void handlerForkMongoDB()
		{
		U_TRACE_NO_PARAM(5, "FortuneNoSql::handlerForkMongoDB()")

#	ifdef USE_MONGODB
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
#	endif
		}

	static UREDISClient_Base* rc;

	static void handlerQueryREDIS()
		{
		U_TRACE(5, "FortuneNoSql::handlerQueryREDIS()")

		(void) rc->lrange(U_CONSTANT_TO_PARAM("fortunes 0 -1"));

		for (uint32_t i = 0, n = rc->vitem.size(); i < n; ++i) Fortune::replace(i, rc->vitem[i]);
		}

	static void handlerForkREDIS()
		{
		U_TRACE_NO_PARAM(5, "Fortune::handlerForkREDIS()")

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

private:
	U_DISALLOW_ASSIGN(FortuneNoSql)
};
#endif
