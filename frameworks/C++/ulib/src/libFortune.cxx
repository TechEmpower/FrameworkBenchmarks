// fortune.cpp

#include "fortune.h"

uint32_t           Fortune::uid;
UString*           Fortune::pmessage;
UOrmSession*       Fortune::psql_fortune;
UOrmStatement*     Fortune::pstmt_fortune;
UVector<Fortune*>* Fortune::pvfortune;
