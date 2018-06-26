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
   // Check for memory error
   U_MEMORY_TEST

   // Allocator e Deallocator
   U_MEMORY_ALLOCATOR
   U_MEMORY_DEALLOCATOR

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
      U_TRACE(0, "Fortune::toJSON(%V)", json.rep)

      json.toJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.toJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   void fromJSON(UValue& json)
      {
      U_TRACE(0, "Fortune::fromJSON(%p)", &json)

      json.fromJSON(U_JSON_METHOD_HANDLER(id,      unsigned int));
      json.fromJSON(U_JSON_METHOD_HANDLER(message, UString));
      }

   // ORM

   void bindParam(UOrmStatement* stmt)
      {
      U_TRACE(0, "Fortune::bindParam(%p)", stmt)

      stmt->bindParam(U_ORM_TYPE_HANDLER(id,      unsigned int));
      stmt->bindParam(U_ORM_TYPE_HANDLER(message, UString));
      }

   void bindResult(UOrmStatement* stmt)
      {
      U_TRACE(0, "Fortune::bindResult(%p)", stmt)

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
      U_TRACE(0, "Fortune::replace(%u,%u,%.*S,%u)", i, _id, len, msg, len)

      U_INTERNAL_ASSERT_POINTER(pvfortune)

      Fortune* elem = pvfortune->at(1+i);

      elem->id = _id;

      UXMLEscape::encode(msg, len, elem->message);
      }

   static void replace(uint32_t i)                                       { replace(i, uid, U_STRING_TO_PARAM(*pmessage)); }
   static void replace(uint32_t i, uint32_t _id)                         { replace(i, _id, U_STRING_TO_PARAM(*pmessage)); }
   static void replace(uint32_t i,               const UString& message) { replace(i, i+1, U_STRING_TO_PARAM(message)); }
   static void replace(uint32_t i, uint32_t _id, const UString& message) { replace(i, _id, U_STRING_TO_PARAM(message)); }

   static void doQuery(vPF handlerQuery)
      {
      U_TRACE(0, "Fortune::doQuery(%p)", handlerQuery)

      U_INTERNAL_ASSERT_POINTER(pvfortune)

      char* pwbuffer = UClientImage_Base::wbuffer->pend();

      (void) memcpy(pwbuffer, U_CONSTANT_TO_PARAM("<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"));
                    pwbuffer   += U_CONSTANT_SIZE("<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

      Fortune* elem = pvfortune->at(0);

      elem->id = 0;
      elem->message.rep->replace(U_CONSTANT_TO_PARAM("Additional fortune added at request time."));

      handlerQuery();

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
                            pwbuffer += 10;
         }

      (void) memcpy(pwbuffer, U_CONSTANT_TO_PARAM("</table></body></html>"));

      UClientImage_Base::wbuffer->size_adjust(pwbuffer + U_CONSTANT_SIZE("</table></body></html>"));

      UHTTP::mime_index = U_html;
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerFork()")

      U_NEW_STRING(pmessage, UString(101U));

      U_NEW(UVector<Fortune*>, pvfortune, UVector<Fortune*>);

      Fortune* elem;

      for (uint32_t i = 0; i < 13; ++i)
         {
         U_NEW(Fortune, elem, Fortune(i));

         pvfortune->push(elem);
         }
      }

   static void handlerForkSql()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerForkSql()")

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

#ifdef DEBUG
   static void handlerEnd()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerEnd()")

      U_INTERNAL_ASSERT_POINTER(pmessage)
      U_INTERNAL_ASSERT_POINTER(pvfortune)

      U_DELETE(pmessage)
      U_DELETE(pvfortune)
      }

   static void handlerEndSql()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerEndSql()")

      if (pstmt_fortune)
         {
         handlerEnd();

         U_DELETE(psql_fortune)
         U_DELETE(pstmt_fortune)

         pstmt_fortune = U_NULLPTR;
         }
      }

   const char* dump(bool breset) const;
#endif

private:
   U_DISALLOW_ASSIGN(Fortune)
};
#endif
