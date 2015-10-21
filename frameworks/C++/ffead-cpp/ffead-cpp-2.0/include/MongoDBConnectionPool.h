/*
 * MongoDBConnectionPool.h
 *
 *  Created on: 02-Jun-2014
 *      Author: sumeetc
 */

#ifndef MONGODBCONNECTIONPOOL_H_
#define MONGODBCONNECTIONPOOL_H_
#define MONGO_HAVE_STDINT
#include "mongoc.h"
#include "ConnectionPooler.h"
#include "LoggerFactory.h"

class MongoDBConnectionPool: public ConnectionPooler {
	mongoc_uri_t* uri;
	Logger logger;
	void initEnv();
	void* newConnection(const bool& isWrite, const ConnectionNode& node);
	void closeConnection(void* conn);
	void destroy();
	bool isReplicaSet;
	bool isSharded;
	bool isUnixDomainSocket;
	bool isSSL;
	string replicaSetName;
public:
	MongoDBConnectionPool(const ConnectionProperties& props);
	~MongoDBConnectionPool();
};

#endif /* MONGODBCONNECTIONPOOL_H_ */
