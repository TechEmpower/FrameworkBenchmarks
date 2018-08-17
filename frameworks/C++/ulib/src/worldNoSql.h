// worldNoSql.h

#ifndef WORLD_NO_SQL_H
#define WORLD_NO_SQL_H 1

#include "world.h"

#include <ulib/net/client/redis.h>
#include <ulib/net/client/elasticsearch.h>

#ifdef USE_MONGODB
#	include <ulib/net/client/mongodb.h>
#endif

class U_EXPORT WorldNoSql {
public:

	static UString* str_rnumber;

	static void doOneQuery(vPFu handlerQuery)
		{
		U_TRACE(5, "WorldNoSql::doOneQuery(%p)", handlerQuery)

		handlerQuery(World::rnumber[0]);

		uint32_t sz = str_rnumber->size();

		World::initOneResult();

		(void) memcpy(World::pwbuffer+16, str_rnumber->data(), sz);

		World::ptr = World::pwbuffer+16+sz;

		World::endOneResult();

		str_rnumber->clear();
		}

	static void handlerResult(uint32_t i)
		{
		U_TRACE(5, "WorldNoSql::handlerResult(%u)", i)

		U_INTERNAL_ASSERT_POINTER(str_rnumber)
		U_INTERNAL_ASSERT_POINTER(World::pwbuffer)

		if (i) World::addResult(i);

		uint32_t sz = str_rnumber->size();

		(void) memcpy(World::ptr+16, str_rnumber->data(), sz);
						  World::ptr += 16+						  sz;

		u_put_unalignedp16(World::ptr, U_MULTICHAR_CONSTANT16('}',','));
								 World::ptr += 2;

		str_rnumber->clear();
		}

	static void doQuery(vPFu handlerQuery)
		{
		U_TRACE(5, "WorldNoSql::doQuery(%p)", handlerQuery)

		World::initResult();

		for (uint32_t i = 0, n = UHTTP::getFormFirstNumericValue(1, 500); i < n; ++i)
			{
			handlerQuery(World::rnumber[i]);

			handlerResult(i);
			}

		World::endResult();
		}

	static void handlerFork()
		{
		U_TRACE_NO_PARAM(5, "WorldNoSql::handlerFork()")

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
		U_TRACE(5, "WorldNoSql::handlerQueryMongoDB(%u)", uid)

		U_INTERNAL_ASSERT_POINTER(str_rnumber)

#	ifdef USE_MONGODB
		(void) mc->findOne(uid, query);
		(void) U_JFIND(mc->vitem[0], "randomNumber", *str_rnumber);

		uint32_t pos = str_rnumber->find_first_of('.');

		if (pos != U_NOT_FOUND) str_rnumber->size_adjust_constant(pos);
#	endif
		}

	static void handlerUpdateMongoDB(uint32_t i)
		{
		U_TRACE(5, "WorldNoSql::handlerUpdateMongoDB(%u)", i)

#	ifdef USE_MONGODB
		(void) mc->findOne(World::rnumber[i], query);
		(void) mc->update( World::rnumber[i], "randomNumber", World::rnum = u_get_num_random_range1(10000));
#	endif
		}

	static void handlerForkMongoDB()
		{
		U_TRACE_NO_PARAM(5, "WorldNoSql::handlerForkMongoDB()")

#	ifdef USE_MONGODB
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
#	endif
		}

	static char rc_buffer[128];
	static UREDISClient_Base* rc;

	static void handlerQueryREDIS(uint32_t uid)
		{
		U_TRACE(5, "WorldNoSql::handlerQueryREDIS(%u)", uid)

		U_INTERNAL_ASSERT_POINTER(str_rnumber)

		char* ptr = rc_buffer+U_CONSTANT_SIZE("world:");

		(void) rc->get(ptr, U_CONSTANT_SIZE("world:")+u_num2str32(uid, ptr)-ptr);

		*str_rnumber = rc->vitem[0];
		}

	static void handlerUpdateREDIS(uint32_t i)
		{
		U_TRACE(5, "WorldNoSql::handlerUpdateREDIS(%u)", i)

		char* start = rc_buffer+U_CONSTANT_SIZE("world:");
		char* ptr = u_num2str32(World::rnumber[i], start);

		(void) rc->get(start, ptr-start);

		*ptr = ' ';
		 ptr = u_num2str32(World::rnum = u_get_num_random_range1(10000), ptr+1);

		(void) rc->mset(start, ptr-start);
		}

	static void handlerForkREDIS()
		{
		U_TRACE_NO_PARAM(5, "WorldNoSql::handlerForkREDIS()")

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

#	define U_QLEN U_CONSTANT_SIZE("{\"query\":{\"match\":{\"_id\":\"")

	static void handlerQueryElasticSearch(uint32_t uid)
		{
		U_TRACE(5, "WorldNoSql::handlerQueryElasticSearch(%u)", uid)

		U_INTERNAL_ASSERT_POINTER(str_rnumber)

		(void) es->sendPOST(U_CONSTANT_TO_PARAM("/tfb/world/_search"), es_buffer1, U_QLEN+
																			u__snprintf(es_buffer1+ U_QLEN,
																				  sizeof(es_buffer1)-U_QLEN, U_CONSTANT_TO_PARAM("%u\"}}}"), uid));

		(void) U_JFIND(es->getContent(), "randomNumber", *str_rnumber);
		}

	static void handlerUpdateElasticSearch(uint32_t i)
		{
		U_TRACE(5, "WorldNoSql::handlerUpdateElasticSearch(%u)", i)

		uint32_t len1 = u__snprintf(pbuffer1, 100, U_CONSTANT_TO_PARAM("%u/_update"), World::rnumber[i]),
					len2 = u__snprintf(pbuffer2, 100, U_CONSTANT_TO_PARAM("%u\"}}"), World::rnum = u_get_num_random_range1(10000));

		(void) es->sendPOST(es_buffer1, len1+U_CONSTANT_SIZE("/tfb/world/"), es_buffer2, len2+U_CONSTANT_SIZE("{\"doc\":{\"_id\":\""));
		}

	static void handlerForkElasticSearch()
		{
		U_TRACE_NO_PARAM(5, "WorldNoSql::handlerForkElasticSearch()")

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

private:
	U_DISALLOW_ASSIGN(WorldNoSql)
};
#endif
