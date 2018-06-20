// fortune.cpp

#include "fortune.h"

char*              Fortune::pwbuffer;
Fortune*           Fortune::pfortune;
Fortune*           Fortune::pfortune2add;
UOrmSession*       Fortune::psql_fortune;
UOrmStatement*     Fortune::pstmt_fortune;
UVector<Fortune*>* Fortune::pvfortune;

#ifdef DEBUG
const char* Fortune::dump(bool breset) const
{
   *UObjectIO::os << "id               " << id              << '\n'
                  << "message (UString " << (void*)&message << ')';

   if (breset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return U_NULLPTR;
}
#endif
