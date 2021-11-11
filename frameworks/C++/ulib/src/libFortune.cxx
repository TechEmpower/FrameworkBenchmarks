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
UPgSqlStatement*	 Fortune::pstmt;
#endif
