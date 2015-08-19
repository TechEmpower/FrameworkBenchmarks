/*
 * ConnectionPooler.h
 *
 *  Created on: 08-May-2014
 *      Author: sumeetc
 */

#ifndef CONNECTIONPOOLER_H_
#define CONNECTIONPOOLER_H_
#include "Connection.h"
#include "Mutex.h"
#include <unistd.h>
#include <iostream>
#include <stdlib.h>
#include <stdio.h>

typedef vector<Connection*> connVec;

class ConnectionPooler {
	connVec readConnections;
	connVec writeConnections;
	Mutex mutex;
	bool initialized;
	void* _env;
	ConnectionProperties properties;
	Connection* checkoutInternal(connVec& conns, const bool& isWrite);
protected:
	void destroyPool();
	void createPool(const ConnectionProperties& properties);
	virtual void initEnv()=0;
	virtual void* newConnection(const bool& isWrite, const ConnectionNode& node)=0;
	virtual void closeConnection(void* conn)=0;
	virtual void destroy()=0;
	const connVec& getWriteConnections() const;
	void* getEnv();
	void setEnv(void* env);
public:
	ConnectionPooler();
	virtual ~ConnectionPooler();
	Connection* checkout();
	Connection* checkout(const bool& isWrite);
	void release(Connection* conn);
	bool isInitialized() const;
	const ConnectionProperties& getProperties() const;
};

#endif /* CONNECTIONPOOLER_H_ */
