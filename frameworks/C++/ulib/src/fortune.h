// fortune.h

#ifndef FORTUNE_H
#define FORTUNE_H 1

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>
#include <ulib/utility/uhttp.h>
#include <ulib/orm/orm_driver.h>
#include <ulib/utility/xml_escape.h>
#include <ulib/net/server/client_image.h>

#ifdef U_STATIC_ORM_DRIVER_PGSQL
#	include <ulib/event/event_db.h>
#  include <ulib/orm/driver/orm_driver_pgsql.h>
#endif

class U_EXPORT Fortune {
public:
	uint32_t id;
	UString message;

	Fortune(uint32_t _id) : id(_id), message(101U)
		{
		U_TRACE_CTOR(5, Fortune, "%u", _id)
		}

	Fortune(uint32_t _id, const UString& _message) : id(_id), message(_message)
		{
		U_TRACE_CTOR(5, Fortune, "%u,%V", _id, _message.rep)
		}

	Fortune(const Fortune& f) : id(f.id), message(f.message)
		{
		U_TRACE_CTOR(5, Fortune, "%p", &f)
		}

	~Fortune()
		{
		U_TRACE_DTOR(5, Fortune)
		}

	// SERVICE

	bool operator<(const Fortune& other) const { return cmp_obj(&message, &other.message); }

	static int cmp_obj(const void* a, const void* b)
		{
		U_TRACE(5, "Fortune::cmp_obj(%p,%p)", a, b)

#	ifdef U_STDCPP_ENABLE
		/**
		 * The comparison function must follow a strict-weak-ordering
		 *
		 * 1) For all x, it is not the case that x < x (irreflexivity)
		 * 2) For all x, y, if x < y then it is not the case that y < x (asymmetry)
		 * 3) For all x, y, and z, if x < y and y < z then x < z (transitivity)
		 * 4) For all x, y, and z, if x is incomparable with y, and y is incomparable with z, then x is incomparable with z (transitivity of incomparability)
		 */

		return (((const Fortune*)a)->message.compare(((const Fortune*)b)->message) < 0);
#	else
		return (*(const Fortune**)a)->message.compare((*(const Fortune**)b)->message);
#	endif
		}

	// JSON

	void toJSON(UString& json)
		{
		U_TRACE(5, "Fortune::toJSON(%V)", json.rep)

		json.toJSON(U_JSON_METHOD_HANDLER(id,		 unsigned int));
		json.toJSON(U_JSON_METHOD_HANDLER(message, UString));
		}

	void fromJSON(UValue& json)
		{
		U_TRACE(5, "Fortune::fromJSON(%p)", &json)

		json.fromJSON(U_JSON_METHOD_HANDLER(id,		unsigned int));
		json.fromJSON(U_JSON_METHOD_HANDLER(message, UString));
		}

	// ORM

	void bindParam(UOrmStatement* stmt)
		{
		U_TRACE(5, "Fortune::bindParam(%p)", stmt)

		stmt->bindParam(U_ORM_TYPE_HANDLER(id,		  unsigned int));
		stmt->bindParam(U_ORM_TYPE_HANDLER(message, UString));
		}

	void bindResult(UOrmStatement* stmt)
		{
		U_TRACE(5, "Fortune::bindResult(%p)", stmt)

		stmt->bindResult(U_ORM_TYPE_HANDLER(id,		unsigned int));
		stmt->bindResult(U_ORM_TYPE_HANDLER(message, UString));
		}

	static uint32_t uid;
	static char* pwbuffer;
	static UString* pmessage;
	static UVector<Fortune*>* pvfortune;

	static UOrmSession*	  psql_fortune;
	static UOrmStatement* pstmt_fortune;

#ifdef U_STATIC_ORM_DRIVER_PGSQL
	static PGconn* conn;
	static UPgSqlStatement* pstmt;

	static void sendQueryPrepared()
		{
		U_TRACE_NO_PARAM(5, "Fortune::sendQueryPrepared()")

		(void) U_SYSCALL(PQsendQueryPrepared, "%p,%S,%u,%p,%p,%p,%u", conn, pstmt->stmtName, 0, 0, 0, 0, 1);
		}
#endif

	static void replace(uint32_t i, uint32_t _id, const char* msg, uint32_t len)
		{
		U_TRACE(5, "Fortune::replace(%u,%u,%.*S,%u)", i, _id, len, msg, len)

		U_INTERNAL_ASSERT_POINTER(pvfortune)

		Fortune* elem = pvfortune->at(i);

		elem->id = _id;

		UXMLEscape::encode(msg, len, elem->message);
		}

	static void replace(uint32_t i)													 { replace(i, uid, U_STRING_TO_PARAM(*pmessage)); }
	static void replace(uint32_t i, uint32_t _id)								 { replace(i, _id, U_STRING_TO_PARAM(*pmessage)); }
	static void replace(uint32_t i,					 const UString& message) { replace(i, i+1, U_STRING_TO_PARAM(message)); }
	static void replace(uint32_t i, uint32_t _id, const UString& message) { replace(i, _id, U_STRING_TO_PARAM(message)); }

	static void initQuery()
		{
		U_TRACE_NO_PARAM(5, "::initQuery()")

		char* ptr = UClientImage_Base::wbuffer->data();

		U_INTERNAL_DUMP("wbuffer(%u) = %#.10S", UClientImage_Base::wbuffer->size(), ptr)

		if (u_get_unalignedp64(ptr+48) != U_MULTICHAR_CONSTANT64('h','a','r','s','e','t','=','U'))
			{
			u_put_unalignedp64(ptr,	    U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
			u_put_unalignedp64(ptr+8,   U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
			u_put_unalignedp64(ptr+16,  U_MULTICHAR_CONSTANT64('x','x','x','x','x','x','x','x'));
			u_put_unalignedp64(ptr+24,  U_MULTICHAR_CONSTANT64('\r','\n','C','o','n','t','e','n'));
			u_put_unalignedp64(ptr+32,  U_MULTICHAR_CONSTANT64('t','-','T','y','p','e',':',' '));
			u_put_unalignedp64(ptr+40,  U_MULTICHAR_CONSTANT64('t','e','x','t','/','h','t','m'));
			u_put_unalignedp64(ptr+48,  U_MULTICHAR_CONSTANT64('l',';',' ','c','h','a','r','s'));
			u_put_unalignedp64(ptr+56,  U_MULTICHAR_CONSTANT64('e','t','=','U','T','F','-','8'));
			u_put_unalignedp64(ptr+64,  U_MULTICHAR_CONSTANT64('\r','\n','\r','\n','<','!','d','o'));
			u_put_unalignedp64(ptr+72,  U_MULTICHAR_CONSTANT64('c','t','y','p','e',' ','h','t'));
			u_put_unalignedp64(ptr+80,  U_MULTICHAR_CONSTANT64('m','l','>','<','h','t','m','l'));
			u_put_unalignedp64(ptr+88,  U_MULTICHAR_CONSTANT64('>','<','h','e','a','d','>','<'));
			u_put_unalignedp64(ptr+96,  U_MULTICHAR_CONSTANT64('t','i','t','l','e','>','F','o'));
			u_put_unalignedp64(ptr+104, U_MULTICHAR_CONSTANT64('r','t','u','n','e','s','<','/'));
			u_put_unalignedp64(ptr+112, U_MULTICHAR_CONSTANT64('t','i','t','l','e','>','<','/'));
			u_put_unalignedp64(ptr+120, U_MULTICHAR_CONSTANT64('h','e','a','d','>','<','b','o'));
			u_put_unalignedp64(ptr+128, U_MULTICHAR_CONSTANT64('d','y','>','<','t','a','b','l'));
			u_put_unalignedp64(ptr+136, U_MULTICHAR_CONSTANT64('e','>','<','t','r','>','<','t'));
			u_put_unalignedp64(ptr+144, U_MULTICHAR_CONSTANT64('h','>','i','d','<','/','t','h'));
			u_put_unalignedp64(ptr+152, U_MULTICHAR_CONSTANT64('>','<','t','h','>','m','e','s'));
			u_put_unalignedp64(ptr+160, U_MULTICHAR_CONSTANT64('s','a','g','e','<','/','t','h'));
			u_put_unalignedp64(ptr+168, U_MULTICHAR_CONSTANT64('>','<','/','t','r','>','\0','\0'));

			pwbuffer	= ptr + U_CONSTANT_SIZE("Content-Length: xxxxxxxx\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"
														"<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");
			}

		U_INTERNAL_ASSERT_EQUALS(u_get_unalignedp64(UClientImage_Base::wbuffer->data()), U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'))
		}

	static void endQuery()
		{
		U_TRACE_NO_PARAM(5, "::endQuery()")

		U_INTERNAL_ASSERT_POINTER(pvfortune)

		Fortune* elem = pvfortune->last();

		elem->id = 0;
		elem->message.rep->replace(U_CONSTANT_TO_PARAM("Additional fortune added at request time."));

		pvfortune->sort(Fortune::cmp_obj);

		char* ptr = pwbuffer;
		uint32_t content_length = U_CONSTANT_SIZE("<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

		for (uint32_t sz, i = 0, n = pvfortune->size(); i < n; ++i)
			{
			elem = pvfortune->at(i);

			u_put_unalignedp64(ptr, U_MULTICHAR_CONSTANT64('<','t','r','>','<','t','d','>'));

			ptr = u_num2str32(elem->id, ptr+8);

			u_put_unalignedp64(ptr, U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','t','d'));
									 ptr += 8;

			*ptr++ = '>';

			(void) memcpy(ptr, elem->message.data(), sz = elem->message.size());
							  ptr += sz;

			u_put_unalignedp64(ptr,   U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','/','t'));
			u_put_unalignedp16(ptr+8, U_MULTICHAR_CONSTANT16('r','>'));
									 ptr += 8+2;
			}

		u_put_unalignedp64(ptr,    U_MULTICHAR_CONSTANT64('<','/','t','a','b','l','e','>'));
		u_put_unalignedp64(ptr+8,  U_MULTICHAR_CONSTANT64('<','/','b','o','d','y','>','<'));
		u_put_unalignedp64(ptr+16, U_MULTICHAR_CONSTANT64('/','h','t','m','l','>','\0','\0'));

		content_length += (ptr - pwbuffer) + U_CONSTANT_SIZE("</table></body></html>");

		ptr = pwbuffer	- U_CONSTANT_SIZE("xxxxxxxx\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"
													"<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

		u_put_unalignedp64(ptr, U_MULTICHAR_CONSTANT64(' ',' ',' ',' ',' ',' ',' ',' '));

		(void) u_num2str32(content_length, ptr);

		UClientImage_Base::wbuffer->size_adjust_constant(U_CONSTANT_SIZE("Content-Length: xxxxxxxx\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n") + content_length);
		}

	static void doQuery(vPF handlerQuery)
		{
		U_TRACE(5, "Fortune::doQuery(%p)", handlerQuery)

			initQuery();
		handlerQuery();
			 endQuery();
		}

	static void handlerInitSql()
		{
		U_TRACE_NO_PARAM(5, "Fortune::handlerInitSql()")

#	ifdef U_STATIC_ORM_DRIVER_PGSQL
		U_INTERNAL_DUMP("UServer_Base::handler_db2 = %p", UServer_Base::handler_db2)

		if (UServer_Base::handler_db2 == U_NULLPTR) UServer_Base::handler_db2 = new UEventDB();
#	endif
		}

	static void handlerFork()
		{
		U_TRACE_NO_PARAM(5, "Fortune::handlerFork()")

		pmessage = new UString(101U);

		pvfortune = new UVector<Fortune*>();

		Fortune* elem;

		for (uint32_t i = 0; i < 13; ++i)
			{
			elem = new Fortune(i+1);

			pvfortune->push(elem);
			}
		}

	static void handlerForkSql()
		{
		U_TRACE_NO_PARAM(5, "Fortune::handlerForkSql()")

		if (psql_fortune == U_NULLPTR)
			{
			psql_fortune = new UOrmSession(U_CONSTANT_TO_PARAM("fortune"));

			if (psql_fortune->isReady() == false)
				{
				U_WARNING("Fortune::handlerForkSql(): we cound't connect to db");

				U_DELETE(psql_fortune)

				psql_fortune = U_NULLPTR;

				return;
				}

			pstmt_fortune = new UOrmStatement(*psql_fortune, U_CONSTANT_TO_PARAM("SELECT id, message FROM Fortune"));

			handlerFork();

			pstmt_fortune->into(uid, *pmessage);

#		ifdef U_STATIC_ORM_DRIVER_PGSQL
			if (UOrmDriver::isPGSQL())
				{
				UOrmDriverPgSql* pdrv = (UOrmDriverPgSql*)psql_fortune->getDriver();

				 conn = (PGconn*)pdrv->UOrmDriver::connection;
				pstmt = (UPgSqlStatement*)pstmt_fortune->getStatement();

				pstmt->prepareStatement(pdrv);

				UServer_Base::handler_db2->setConnection(conn);
				}
#		endif
			}
		}

private:
	U_DISALLOW_ASSIGN(Fortune)
};
#endif
