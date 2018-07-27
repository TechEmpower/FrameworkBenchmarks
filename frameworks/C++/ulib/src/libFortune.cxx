// fortune.cpp

#include "fortune.h"

char*					 Fortune::pwbuffer;
uint32_t				 Fortune::uid;
UString*				 Fortune::pmessage;
UOrmSession*		 Fortune::psql_fortune;
UOrmStatement*		 Fortune::pstmt_fortune;
UVector<Fortune*>* Fortune::pvfortune;
#ifdef U_STATIC_ORM_DRIVER_PGSQL
PGconn*				 Fortune::conn;
UOrmDriverPgSql*	 Fortune::pdrv;
UPgSqlStatement*	 Fortune::pstmt;
#endif

#ifdef DEBUG
const char* Fortune::dump(bool breset) const
{
	*UObjectIO::os << "id               " << id				   << '\n'
						<< "message (UString " << (void*)&message << ')';

	if (breset)
		{
		UObjectIO::output();

		return UObjectIO::buffer_output;
		}

	return U_NULLPTR;
}
#endif
