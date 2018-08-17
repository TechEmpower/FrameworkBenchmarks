// worldNoSql.cpp

#include "worldNoSql.h"

#ifdef USE_MONGODB
bson_t*					 WorldNoSql::query;
UMongoDBClient*		 WorldNoSql::mc;
#endif
char						 WorldNoSql::rc_buffer[128];
char						 WorldNoSql::es_buffer1[128];
char						 WorldNoSql::es_buffer2[128];
char*						 WorldNoSql::pbuffer1;
char*						 WorldNoSql::pbuffer2;
UString*					 WorldNoSql::str_rnumber;
UREDISClient_Base*	 WorldNoSql::rc;
UElasticSearchClient* WorldNoSql::es;
