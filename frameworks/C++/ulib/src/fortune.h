// fortune.h

#ifndef FORTUNE_H
#define FORTUNE_H 1

#include <ulib/base/coder/xml.h>

#include <ulib/orm/orm.h>
#include <ulib/json/value.h>
#include <ulib/utility/uhttp.h>
#include <ulib/orm/orm_driver.h>
#include <ulib/net/server/client_image.h>

class Fortune {
public:
   // Check for memory error
   U_MEMORY_TEST

   // Allocator e Deallocator
   U_MEMORY_ALLOCATOR
   U_MEMORY_DEALLOCATOR

   uint32_t id;
   UString message;

   Fortune()
      {
      U_TRACE_CTOR(5, Fortune, "")

      // coverity[uninit_ctor]
#  ifdef U_COVERITY_FALSE_POSITIVE
      id = 0;
#  endif
      }

   Fortune(uint32_t _id, const UString& _message) : id(_id), message(_message)
      {
      U_TRACE_CTOR(5, Fortune, "%u,%V", _id, _message.rep)
      }

   Fortune(const Fortune& f) : id(f.id), message((void*)U_STRING_TO_PARAM(f.message))
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

   static char* pwbuffer;
   static Fortune* pfortune;
   static Fortune* pfortune2add;
   static UVector<Fortune*>* pvfortune;

   static UOrmSession*    psql_fortune;
   static UOrmStatement* pstmt_fortune;

   static void doQuery(vPF handlerQuery)
      {
      U_TRACE(0, "Fortune::doQuery(%p)", handlerQuery)

      U_INTERNAL_ASSERT_POINTER(pfortune2add)

      pwbuffer = UClientImage_Base::wbuffer->pend();

      (void) memcpy(pwbuffer, U_CONSTANT_TO_PARAM("<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"));
                    pwbuffer   += U_CONSTANT_SIZE("<!doctype html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>");

      pvfortune->push(pfortune2add);

      handlerQuery();

      pvfortune->sort(Fortune::cmp_obj);

      Fortune* elem;

      for (uint32_t i = 0, n = pvfortune->size(); i < n; ++i)
         {
         elem = pvfortune->at(i);

         u_put_unalignedp64(pwbuffer, U_MULTICHAR_CONSTANT64('<','t','r','>','<','t','d','>'));

         pwbuffer = u_num2str32(elem->id, pwbuffer+8);

         u_put_unalignedp64(pwbuffer, U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','t','d'));
                            pwbuffer += 8;

         *pwbuffer++ = '>';

         pwbuffer += u_xml_encode((const unsigned char*)U_STRING_TO_PARAM(elem->message), (unsigned char*)pwbuffer);

         u_put_unalignedp64(pwbuffer,   U_MULTICHAR_CONSTANT64('<','/','t','d','>','<','/','t'));
         u_put_unalignedp16(pwbuffer+8, U_MULTICHAR_CONSTANT16('r','>'));
                            pwbuffer += 10;

         if (elem != pfortune2add) U_DELETE(elem)
         }

      pvfortune->setEmpty();

      (void) memcpy(pwbuffer, U_CONSTANT_TO_PARAM("</table></body></html>"));

      UClientImage_Base::wbuffer->size_adjust(pwbuffer + U_CONSTANT_SIZE("</table></body></html>"));

      UHTTP::mime_index = U_html;
      }

   static void handlerFork()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerFork()")

      U_NEW(UVector<Fortune*>, pvfortune, UVector<Fortune*>);

      U_NEW(Fortune, pfortune2add, Fortune(0, U_STRING_FROM_CONSTANT("Additional fortune added at request time.")));
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

         U_NEW(Fortune, pfortune, Fortune);

         pstmt_fortune->into(*pfortune);

         handlerFork();
         }
      }

#ifdef DEBUG
   static void handlerEnd()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerEnd()")

      U_INTERNAL_ASSERT_POINTER(pvfortune)
      U_INTERNAL_ASSERT_POINTER(pfortune2add)

      U_DELETE(pvfortune)
      U_DELETE(pfortune2add)
      }

   static void handlerEndSql()
      {
      U_TRACE_NO_PARAM(0, "Fortune::handlerEndSql()")

      if (pstmt_fortune)
         {
         handlerEnd();

         U_DELETE(pfortune)
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

// override the default...
template <> inline void u_destroy(const Fortune** ptr, uint32_t n) { U_TRACE(0,"u_destroy<Fortune*>(%p,%u)", ptr, n) }
#endif
