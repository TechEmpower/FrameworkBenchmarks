/*
 * CacheManager.h
 *
 *  Created on: 12-Oct-2014
 *      Author: sumeetc
 */

#ifndef CACHEMANAGER_H_
#define CACHEMANAGER_H_
#include "MemoryCacheImpl.h"
#ifdef INC_CACHE_MEMCACHED
#include"MemcachedImpl.h"
#endif
#ifdef INC_CACHE_REDIS
#include "RedisCacheImpl.h"
#endif

class CacheManager {
	static map<string, CacheManager*> caches;
	static string defDsnName;
	ConnectionProperties props;
	ConnectionPooler* pool;
	static void initCache(const ConnectionProperties& props, const string& appName);
	static void destroy();
	CacheManager(const ConnectionProperties& props);
	friend class ConfigurationHandler;
public:
	virtual ~CacheManager();
	CacheInterface* getImpl(string name = "");
};

#endif /* CACHEMANAGER_H_ */
