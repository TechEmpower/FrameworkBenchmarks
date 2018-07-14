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
#  include <ulib/orm/driver/orm_driver_pgsql.h>
#endif

class Fortune {
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

      U_MEMORY_TEST_COPY(f)
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

#  ifdef U_STDCPP_ENABLE
      /**
       * The comparison function must follow a strict-weak-ordering
       *
       * 1) For all x, it is not the case that x < x (irreflexivity)
       * 2) For all x, y, if x < y then it is not the case that y < x (asymmetry)
       * 3) For all x, y, and z, if x < y and y < z then x < z (transitivity)
       * 4) For all x, y, and z, if x is incomparable with y, and y is incomparable with z, then x is incomparable with z (transitivity of incomparability)
       */

      return (((const Fortune*)a)->message.compare(((const Fortune*)b)->message) < 0);
#  else
      return (*(const Fortune**)a)->message.compare((*(const Fortune**)b)->message);
#  endif
      }

   // JSON

   void toJSON(UString& json)
      {
      U_TRACE(5, "Fortune::toJSON(%V)", json.rep)

      json.toJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.toJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   void fromJSON(UValue& json)
      {
      U_TRACE(5, "Fortune::fromJSON(%p)", &json)

      json.fromJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.fromJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   // ORM

   void bindParam(UOrmStatement* stmt)
      {
      U_TRACE(5, "Fortune::bindParam(%p)", stmt)

      stmt->bindParam(U_ORM_TYPE_HANDLER(id,      unsigned int));
      stmt->bindParam(U_ORM_TYPE_HANDLER(message, UString));
      }

   void bindResult(UOrmStatement* stmt)
      {
      U_TRACE(5, "Fortune::bindResult(%p)", stmt)

      stmt->bindResult(U_ORM_TYPE_HANDLER(id,      unsigned int));
      stmt->bindResult(U_ORM_TYPE_HANDLER(message, UString));
      }

   static uint32_t uid;
   static UString* pmessage;
   static UVector<Fortune*>* pvfortune;

   static UOrmSession*    psql_fortune;
   static UOrmStatement* pstmt_fortune;

   static void replace(uint32_t i, uint32_t _id, const char* msg, uint32_t len)
      {
      U_TRACE(5, "Fortune::replace(%u,%u,%.*S,%u)", i, _id, len, msg, len)

      U_INTERNAL_ASSERT_POINTER(pvfortune)

      Fortune* elem = pvfortune->at(i);

      elem->id = _id;

      UXMLEscape::encode(msg, len, elem->message);
      }

   static void replace(uint32_t i)                                       { replace(i, uid, U_STRING_TO_PARAM(*pmessage)); }
   static void replace(uint32_t i, uint32_t _id)                         { replace(i, _id, U_STRING_TO_PARAM(*pmessage)); }
   static void replace(uint32_t i,               const UString& message) { replace(i, i+1, U_STRING_TO_PARAM(message)); }
   static void replace(uint32_t i, uint32_t _id, const UString& message) { replace(i, _id, U_STRING_TO_PARAM(message)); }

   static void doQuery(vPF handlerQuery)
      {
      U_TRACE(5, "Fortune::doQuery(%p)", handlerQuery)

      U_INTERNAL_ASSERT_POINTER(pvfortune)

      char* pwbuffer = UClientImage_Base::wbuffer->data();

      u_put_unalignedp64(pwbuffer,     U_MULTICHAR_CONSTANT64('C','o','n','t','e','n','t','-'));
      u_put_unalignedp64(pwbuffer+8,   U_MULTICHAR_CONSTANT64('L','e','n','g','t','h',':',' '));
      u_put_unalignedp64(pwbuffer+16,  U_MULTICHAR_CONSTANT64('1','2','2','7','\r','\n','C','o'));
      u_put_unalignedp64(pwbuffer+24,  U_MULTICHAR_CONSTANT64('n','t','e','n','t','-','T','y'));
      u_put_unalignedp64(pwbuffer+32,  U_MULTICHAR_CONSTANT64('p','e',':',' ','t','e','x','t'));
      u_put_unalignedp64(pwbuffer+40,  U_MULTICHAR_CONSTANT64('/','h','t','m','l',';',' ','c'));
      u_put_unalignedp64(pwbuffer+48,  U_MULTICHAR_CONSTANT64('h','a','r','s','e','t','=','U'));
      u_put_unalignedp64(pwbuffer+56,  U_MULTICHAR_CONSTANT64('T','F','-','8','\r','\n','\r','\n'));
      u_put_unalignedp64(pwbuffer+64,  U_MULTICHAR_CONSTANT64('<','!','d','o','c','t','y','p'));
      u_put_unalignedp64(pwbuffer+72,  U_MULTICHAR_CONSTANT64('e',' ','h','t','m','l','>','<'));
      u_put_unalignedp64(pwbuffer+80,  U_MULTICHAR_CONSTANT64('h','t','m','l','>','<','h','e'));
      u_put_unalignedp64(pwbuffer+88,  U_MULTICHAR_CONSTANT64('a','d','>','<','t','i','t','l'));
      u_put_unalignedp64(pwbuffer+96,  U_MULTICHAR_CONSTANT64('e','>','F','o','r','t','u','n'));
      u_put_unalignedp64(pwbuffer+104, U_MULTICHAR_CONSTANT64('e','s','<','/','t','i','t','l'));
      u_put_unalignedp64(pwbuffer+112, U_MULTICHAR_CONSTANT64('e','>','<','/','h','e','a','d'));
      u_put_unalignedp64(pwbuffer+120, U_MULTICHAR_CONSTANT64('>','<','b','o','d','y','>','<'));
      u_put_unalignedp64(pwbuffer+128, U_MULTICHAR_CONSTANT64('t','a','b','l','e','>','<','t'));
      u_put_unalignedp64(pwbuffer+136, U_MULTICHAR_CONSTANT64('r','>','<','t','h','>','i','d'));
      u_put_unalignedp64(pwbuffer+144, U_MULTICHAR_CONSTANT64('<','/','t','h','>','<','t','h'));
      u_put_unalignedp64(pwbuffer+152, U_MULTICHAR_CONSTANT64('>','m','e','s','s','a','g','e'));
      u_put_unalignedp64(pwbuffer+160, U_MULTICHAR_CONSTANT64('<','/','t','h','>','<','/','t'));
      u_put_unalignedp16(pwbuffer+168, U_MULTICHAR_CONSTANT16('r','>'));

      pwbuffer += U_CONSTANT_SIZE("Content-Length: 1227\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"
                                  "<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

      handlerQuery();

      Fortune* elem = pvfortune->last();

      elem->id = 0;
      elem->message.rep->replace(U_CONSTANT_TO_PARAM("Additional fortune added at request time."));

      pvfortune->sort(Fortune::cmp_obj);

      for (uint32_t sz, i = 0, n = pvfortune->size(); i < n; ++i)
         {
         elem = pvfortune->at(i);

         u_put_unalignedp64(pwbuffer, U_MULTICHAR_CONSTANT64('<','t','r','>','<','t','d','>'));

         pwbuffer = u_num2str32(elem->id, pwbuffer+8);

         u_put_unalignedp64(pwbuffer, U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','t','d'));
                            pwbuffer += 8;

         *pwbuffer++ = '>';

         (void) memcpy(pwbuffer, elem->message.data(), sz = elem->message.size());
                       pwbuffer += sz;

         u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','/','t'));
         u_put_unalignedp16(pwbuffer+8, U_MULTICHAR_CONSTANT16('r','>'));
                            pwbuffer += 8+2;
         }

      u_put_unalignedp64(pwbuffer,    U_MULTICHAR_CONSTANT64('<','/','t','a','b','l','e','>'));
      u_put_unalignedp64(pwbuffer+8,  U_MULTICHAR_CONSTANT64('<','/','b','o','d','y','>','<'));
      u_put_unalignedp64(pwbuffer+16, U_MULTICHAR_CONSTANT64('/','h','t','m','l','>','\0','\0'));

      UClientImage_Base::wbuffer->size_adjust_constant(pwbuffer + U_CONSTANT_SIZE("</table></body></html>"));
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(5, "Fortune::handlerFork()")

      U_NEW_STRING(pmessage, UString(101U));

      U_NEW(UVector<Fortune*>, pvfortune, UVector<Fortune*>);

      Fortune* elem;

      for (uint32_t i = 0; i < 13; ++i)
         {
         U_NEW(Fortune, elem, Fortune(i+1));

         pvfortune->push(elem);
         }
      }

   static void handlerForkSql()
      {
      U_TRACE_NO_PARAM(5, "Fortune::handlerForkSql()")

      if (psql_fortune == U_NULLPTR)
         {
         U_NEW(UOrmSession, psql_fortune, UOrmSession(U_CONSTANT_TO_PARAM("fortune")));

         if (psql_fortune->isReady() == false)
            {
            U_WARNING("Fortune::handlerForkSql(): we cound't connect to db");

            U_DELETE(psql_fortune)

            psql_fortune = U_NULLPTR;

            return;
            }

         U_NEW(UOrmStatement, pstmt_fortune, UOrmStatement(*psql_fortune, U_CONSTANT_TO_PARAM("SELECT id, message FROM Fortune")));

         handlerFork();

         pstmt_fortune->into(uid, *pmessage);
         }
      }

private:
   U_DISALLOW_ASSIGN(Fortune)
};
#endif
