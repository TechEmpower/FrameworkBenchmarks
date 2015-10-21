/*
 * DataSourceManager.h
 *
 *  Created on: 14-May-2014
 *      Author: sumeetc
 */

#ifndef DATASOURCEMANAGER_H_
#define DATASOURCEMANAGER_H_

#include "DataSourceInterface.h"
#ifdef INC_SDORM_SQL
#include "SQLConnectionPool.h"
#include "SQLDataSourceImpl.h"
#endif
#ifdef INC_SDORM_MONGO
#include "MongoDBConnectionPool.h"
#include "MongoDBDataSourceImpl.h"
#endif

class DataSourceManager;

class DataSourceManager {
	static map<string, DataSourceManager*> dsns;
	static string defDsnName;
	ConnectionProperties props;
	Mapping mapping;
	ConnectionPooler* pool;
	static void initDSN(const ConnectionProperties& props, const Mapping& mapping);
	DataSourceManager(const ConnectionProperties& props, const Mapping& mapping);
	static void destroy();
	friend class ConfigurationHandler;
public:
	virtual ~DataSourceManager();
	static DataSourceInterface* getImpl(string name = "");
};

#endif /* DATASOURCEMANAGER_H_ */
