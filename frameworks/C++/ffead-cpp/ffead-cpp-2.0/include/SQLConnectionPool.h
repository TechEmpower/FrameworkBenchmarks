/*
 * SQLConnectionPool.h
 *
 *  Created on: 14-May-2014
 *      Author: sumeetc
 */

#ifndef SQLCONNECTIONPOOL_H_
#define SQLCONNECTIONPOOL_H_
#include "Compatibility.h"
#ifdef HAVE_LIBODBC
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>
#endif
#include "ConnectionPooler.h"
#include "LoggerFactory.h"

class SQLConnectionPool: public ConnectionPooler {
	Logger logger;
	void initEnv();
	void* newConnection(const bool& isWrite, const ConnectionNode& node);
	void closeConnection(void* conn);
	void destroy();
public:
	SQLConnectionPool(const ConnectionProperties& props);
	virtual ~SQLConnectionPool();
};

#endif /* SQLCONNECTIONPOOL_H_ */
