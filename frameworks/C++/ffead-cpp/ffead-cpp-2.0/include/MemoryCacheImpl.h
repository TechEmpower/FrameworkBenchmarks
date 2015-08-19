/*
 * MemoryCacheImpl.h
 *
 *  Created on: 10-May-2014
 *      Author: sumeetc
 */

#ifndef MEMORYCACHEIMPL_H_
#define MEMORYCACHEIMPL_H_
#include "CacheInterface.h"
#include "ConnectionPooler.h"

class MemoryCacheImpl : public CacheInterface, public ConnectionPooler {
	map<string, string> internalMap;
	ConnectionProperties properties;
	bool setInternal(const string& key, const string& value, const int& expireSeconds, const int& setOrAddOrRep);
public:
	MemoryCacheImpl(const ConnectionProperties& properties);
	~MemoryCacheImpl();
	void init();

	bool set(const string& key, GenericObject& value, const int& expireSeconds);
	bool add(const string& key, GenericObject& value, const int& expireSeconds);
	bool replace(const string& key, GenericObject& value, const int& expireSeconds);

	string getValue(const string& key);

	bool remove(const string& key);
	long long increment(const string& key, const int& number= 1);
	long long decrement(const string& key, const int& number= 1);
	long double incrementFloat(const string& key, const double& number = 1.0);
	long double decrementFloat(const string& key, const double& number = 1.0);
	map<string, string> statistics();
	bool flushAll();

	void* executeCommand(const string& command, ...);

	void initEnv();
	void destroy();
	void* newConnection(const bool& isWrite, const ConnectionNode& node);
	void closeConnection(void* conn);
};

#endif /* MEMORYCACHEIMPL_H_ */
