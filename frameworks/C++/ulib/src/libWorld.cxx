// world.cpp

#include "world.h"

char*          World::pwbuffer;
World*         World::pworld_query;
uint32_t       World::rnum;
uint32_t       World::rnumber[500];
UOrmSession*   World::psql_query;
UOrmStatement* World::pstmt_query;

#ifdef DEBUG
const char* World::dump(bool breset) const
{
   *UObjectIO::os << "id           " << id            << '\n'
                  << "randomNumber " << randomNumber;

   if (breset)
      {
      UObjectIO::output();

      return UObjectIO::buffer_output;
      }

   return U_NULLPTR;
}
#endif
