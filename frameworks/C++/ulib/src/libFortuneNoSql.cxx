// fortuneNoSql.cpp

#include "fortuneNoSql.h"

#ifdef USE_MONGODB
UMongoDBClient*	 FortuneNoSql::mc;
#endif
UREDISClient_Base* FortuneNoSql::rc;
