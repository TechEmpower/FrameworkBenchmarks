// world.cpp

#include "world.h"

char				  World::wbuffer[18000];
char*				  World::ptr;
char*				  World::pwbuffer;
World*			  World::pworld_query;
uint32_t			  World::rnum;
uint32_t			  World::rnumber[501];
UOrmSession*	  World::psql_query;
UOrmStatement*	  World::pstmt_query;
#ifdef U_STATIC_ORM_DRIVER_PGSQL
char				  World::num2str[sizeof(unsigned int)];
PGconn*			  World::conn;
UPgSqlStatement* World::pstmt;
#endif
