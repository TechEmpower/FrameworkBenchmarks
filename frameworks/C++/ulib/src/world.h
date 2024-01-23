// world.h

#ifndef WORLD_H
#define WORLD_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>
#include <ulib/utility/uhttp.h>
#include <ulib/orm/orm_driver.h>
#include <ulib/net/server/client_image.h>

#ifdef U_STATIC_ORM_DRIVER_PGSQL
#	include <ulib/event/event_db.h>
#  include <ulib/orm/driver/orm_driver_pgsql.h>
#endif

class U_EXPORT World {
public:
	uint32_t id, randomNumber;

	World()
		{
		U_TRACE_CTOR(5, World, "")

		// coverity[uninit_ctor]
#	ifdef U_COVERITY_FALSE_POSITIVE
		id = randomNumber = 0;
#	endif
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

		json.toJSON(U_JSON_METHOD_HANDLER(id,				unsigned int));
		json.toJSON(U_JSON_METHOD_HANDLER(randomNumber,	unsigned int));
		}

	void fromJSON(UValue& json)
		{
		U_TRACE(5, "World::fromJSON(%p)", &json)

		json.fromJSON(U_JSON_METHOD_HANDLER(id,			  unsigned int));
		json.fromJSON(U_JSON_METHOD_HANDLER(randomNumber, unsigned int));
		}

	// ORM

	void bindParam(UOrmStatement* stmt)
		{
		U_TRACE(5, "World::bindParam(%p)", stmt)

		stmt->bindParam(U_ORM_TYPE_HANDLER(id,				 unsigned int));
		stmt->bindParam(U_ORM_TYPE_HANDLER(randomNumber, unsigned int));
		}

	void bindResult(UOrmStatement* stmt)
		{
		U_TRACE(5, "World::bindResult(%p)", stmt)

		stmt->bindResult(U_ORM_TYPE_HANDLER(id,			  unsigned int));
		stmt->bindResult(U_ORM_TYPE_HANDLER(randomNumber, unsigned int));
		}

	// SERVICE

	bool operator<(const World& other) const { return cmp_obj(&id, &other.id); }

	static int cmp_obj(const void* a, const void* b)
		{
		U_TRACE(5, "World::cmp_obj(%p,%p)", a, b)

#	ifdef U_STDCPP_ENABLE
		/**
		 * The comparison function must follow a strict-weak-ordering
		 *
		 * 1) For all x, it is not the case that x < x (irreflexivity)
		 * 2) For all x, y, if x < y then it is not the case that y < x (asymmetry)
		 * 3) For all x, y, and z, if x < y and y < z then x < z (transitivity)
		 * 4) For all x, y, and z, if x is incomparable with y, and y is incomparable with z, then x is incomparable with z (transitivity of incomparability)
		 */

		return (((const World*)a)->id < (((const World*)b)->id));
#	else
		return (*(const World**)a)->id < ((*(const World**)b)->id);
#	endif
		}

	static char* ptr;
	static char* pwbuffer;
	static char wbuffer[18000];
	static uint32_t rnum, rnumber[501];

	static World*        pworld_query;
	static UOrmSession*	  psql_query;
	static UOrmStatement* pstmt_query;

#ifdef U_STATIC_ORM_DRIVER_PGSQL
	static PGconn* conn;
	static UPgSqlStatement* pstmt;
	static char num2str[sizeof(unsigned int)];

	static void _sendQueryPrepared()
		{
		U_TRACE_NO_PARAM(5, "World::_sendQueryPrepared()")

		(void) U_SYSCALL(PQsendQueryPrepared, "%p,%S,%u,%p,%p,%p,%u", conn, pstmt->stmtName, 1, pstmt->paramValues, pstmt->paramLengths, pstmt->paramFormats, 1);
		}

	static void sendQueryPrepared()
		{
		U_TRACE_NO_PARAM(5, "World::sendQueryPrepared()")

		U_INTERNAL_ASSERT_MAJOR(rnumber[0], 0)

		*(unsigned int*)num2str = htonl(rnumber[0]);

		_sendQueryPrepared();
		}

	static void sendQueryPrepared(uint32_t i)
		{
		U_TRACE(5, "World::sendQueryPrepared(%u)", i)

		U_INTERNAL_ASSERT_MAJOR(rnumber[i], 0)

		*(unsigned int*)num2str = htonl(rnumber[i]);

		_sendQueryPrepared();
		}
#endif

	static void initOneResult()
		{
		U_TRACE_NO_PARAM(5, "World::initOneResult()")

		U_INTERNAL_DUMP("wbuffer = %#.10S", wbuffer)

		if (u_get_unalignedp64(wbuffer+52) != U_MULTICHAR_CONSTANT64('\r','\n','{','"','i','d','"',':'))
			{
			u_put_unalignedp64(wbuffer,	 U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
			u_put_unalignedp64(wbuffer+8,  U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
			u_put_unalignedp32(wbuffer+16, U_MULTICHAR_CONSTANT32('3','1','\r','\n'));
			u_put_unalignedp64(wbuffer+20, U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
			u_put_unalignedp64(wbuffer+28, U_MULTICHAR_CONSTANT64('T','y','p','e',':',' ','a','p'));
			u_put_unalignedp64(wbuffer+36, U_MULTICHAR_CONSTANT64('p','l','i','c','a','t','i','o'));
			u_put_unalignedp64(wbuffer+44, U_MULTICHAR_CONSTANT64('n','/','j','s','o','n','\r','\n'));
			u_put_unalignedp64(wbuffer+52, U_MULTICHAR_CONSTANT64('\r','\n','{','"','i','d','"',':'));

			pwbuffer = u_num2str32(rnumber[0], wbuffer + U_CONSTANT_SIZE("Content-Length: 31\r\nContent-Type: application/json\r\n\r\n{\"id\":"));

			u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
			u_put_unalignedp64(pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));
			}

		U_INTERNAL_ASSERT_EQUALS(u_get_unalignedp64(wbuffer),	U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'))
		}

	static void endOneResult()
		{
		U_TRACE_NO_PARAM(5, "World::endOneResult()")

		*ptr = '}';

		uint32_t		  len = ptr-wbuffer+1,
					body_len = len - U_CONSTANT_SIZE("Content-Length: 31\r\nContent-Type: application/json\r\n\r\n");

		U_NUM2STR16(wbuffer+U_CONSTANT_SIZE("Content-Length: "), body_len);

		UClientImage_Base::wbuffer->setConstant(wbuffer, len);
		}

	static void handlerOneResult(uint32_t random)
		{
		U_TRACE(5, "World::handlerOneResult(%u)", random)

		initOneResult();

		ptr = u_num2str32(random, pwbuffer+16);

		endOneResult();
		}

	static void initResult()
		{
		U_TRACE_NO_PARAM(5, "World::initResult()")

		U_INTERNAL_DUMP("wbuffer = %#.10S", wbuffer)

		if (u_get_unalignedp64(wbuffer+56) != U_MULTICHAR_CONSTANT64('\n','[','{','"','i','d','"',':'))
			{
			u_put_unalignedp64(wbuffer,	 U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
			u_put_unalignedp64(wbuffer+8,  U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
			u_put_unalignedp64(wbuffer+16, U_MULTICHAR_CONSTANT64('1','3','3','3','1','\r','\n','C'));
			u_put_unalignedp64(wbuffer+24, U_MULTICHAR_CONSTANT64('o','n','t','e','n','t','-','T'));
			u_put_unalignedp64(wbuffer+32, U_MULTICHAR_CONSTANT64('y','p','e',':',' ','a','p','p'));
			u_put_unalignedp64(wbuffer+40, U_MULTICHAR_CONSTANT64('l','i','c','a','t','i','o','n'));
			u_put_unalignedp64(wbuffer+48, U_MULTICHAR_CONSTANT64('/','j','s','o','n','\r','\n','\r'));
			u_put_unalignedp64(wbuffer+56, U_MULTICHAR_CONSTANT64('\n','[','{','"','i','d','"',':'));

			pwbuffer = u_num2str32(rnumber[0], wbuffer + U_CONSTANT_SIZE("Content-Length: 13331\r\nContent-Type: application/json\r\n\r\n[{\"id\":"));

			u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
			u_put_unalignedp64(pwbuffer+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));
			}

		U_INTERNAL_ASSERT_EQUALS(u_get_unalignedp64(wbuffer),	U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'))

		ptr = pwbuffer;
		}

	static void endResult()
		{
		U_TRACE_NO_PARAM(5, "World::endResult()")

		*(ptr-1) = ']';

		uint32_t		  len = ptr-wbuffer,
					body_len = len - U_CONSTANT_SIZE("Content-Length: 13331\r\nContent-Type: application/json\r\n\r\n");

		ptr = u_num2str32(body_len, wbuffer + U_CONSTANT_SIZE("Content-Length: "));

		while (*ptr != '\r') *ptr++ = ' ';

		UClientImage_Base::wbuffer->setConstant(wbuffer, len);
		}

	static void addResult(uint32_t i)
		{
		U_TRACE(5, "World::addResult(%u)", i)

		U_INTERNAL_ASSERT_MAJOR(i, 0)

		u_put_unalignedp32(ptr,   U_MULTICHAR_CONSTANT32('{','"','i','d'));
		u_put_unalignedp16(ptr+4, U_MULTICHAR_CONSTANT16('"',':'));

		ptr = u_num2str32(rnumber[i], ptr+6);

		u_put_unalignedp64(ptr,   U_MULTICHAR_CONSTANT64(',','"','r','a','n','d','o','m'));
		u_put_unalignedp64(ptr+8, U_MULTICHAR_CONSTANT64('N','u','m','b','e','r','"',':'));
		}

	static void addRandom(uint32_t random)
		{
		U_TRACE(5, "World::addRandom(%u)", random)

		ptr = u_num2str32(random, ptr+16);

		u_put_unalignedp16(ptr, U_MULTICHAR_CONSTANT16('}',','));
								 ptr += 2;
		}

	static void handlerResult(uint32_t i, uint32_t random)
		{
		U_TRACE(5, "World::handlerResult(%u,%u)", i, random)

		if (i) addResult(i);

		addRandom(random);
		}

	static void doUpdateNoSql(vPFu handlerUpdateNoSql)
		{
		U_TRACE(5, "World::doUpdateNoSql(%p)", handlerUpdateNoSql)

		initResult();

		for (uint32_t i = 0, n = UHTTP::getFormFirstNumericValue(1, 500); i < n; ++i)
			{
			handlerUpdateNoSql(i);

			handlerResult(i, rnum);
			}

		endResult();
		}

	static void handlerInitSql()
		{
		U_TRACE_NO_PARAM(5, "World::handlerInitSql()")

#	ifdef U_STATIC_ORM_DRIVER_PGSQL
		U_INTERNAL_DUMP("UServer_Base::handler_db1 = %p", UServer_Base::handler_db1)

		if (UServer_Base::handler_db1 == U_NULLPTR)
			{
			U_NEW(UEventDB, UServer_Base::handler_db1, UEventDB);
			}
#	endif
		}

	static void handlerFork()
		{
		U_TRACE_NO_PARAM(5, "World::handlerFork()")

		if (rnumber[0] == 0) for (uint32_t i = 0; i <= 500; ++i) rnumber[i] = u_get_num_random_range1(10000);
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

			U_NEW(UOrmStatement, pstmt_query, UOrmStatement(*psql_query, U_CONSTANT_TO_PARAM("SELECT randomNumber, id FROM World WHERE id = ?")));

			U_NEW(World, pworld_query, World);

			pstmt_query->use( pworld_query->id);
			pstmt_query->into(pworld_query->randomNumber);

#		ifdef U_STATIC_ORM_DRIVER_PGSQL
			if (UOrmDriver::isPGSQL())
				{
				UOrmDriverPgSql* pdrv = (UOrmDriverPgSql*)psql_query->getDriver();

				 conn = (PGconn*)pdrv->UOrmDriver::connection;
				pstmt = (UPgSqlStatement*)pstmt_query->getStatement();

				(void) pstmt->setBindParam(pdrv);

				pstmt->paramValues[0]  = num2str;
				pstmt->paramLengths[0] = sizeof(unsigned int);

				UServer_Base::handler_db1->setConnection(conn);
				}
#		endif

			handlerFork();
			}
		}

private:
	U_DISALLOW_ASSIGN(World)
};
#endif
