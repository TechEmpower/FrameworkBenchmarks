/*
 * MongoDBDataSourceImpl.h
 *
 *  Created on: 02-Jun-2014
 *      Author: sumeetc
 */

#ifndef MONGODBDATASOURCEIMPL_H_
#define MONGODBDATASOURCEIMPL_H_

#include "TemplateEngine.h"
#include "mongoc.h"
#include "DataSourceInterface.h"

class QueryComponent {
	bool isAnd;
	bool undecided;
	vector<Condition> andClauses;
	vector<Condition> orClauses;
	vector<Condition> tempClauses;
	vector<QueryComponent*> andChildren;
	vector<QueryComponent*> orChildren;
	vector<QueryComponent*> tempChildren;
	QueryComponent* parent;
	bson_t* actualQuery;
	friend class MongoDBDataSourceImpl;
	virtual ~QueryComponent();
};

class MongoContext {
	mongoc_collection_t *collection;
	Connection* conn;
	friend class MongoDBDataSourceImpl;
	MongoContext();
	virtual ~MongoContext();
};

class MongoDBDataSourceImpl: public DataSourceInterface {
	Logger logger;
	static QueryComponent* getQueryComponent(const vector<Condition>& conds);
	static void populateQueryComponents(QueryComponent* sq);
	static bson_t* createSubMongoQuery(vector<Condition>& conds);
	static void appendGenericObject(bson_t* b, const string& name, GenericObject& o);
	static map<string, map<string, Condition> > toMap(vector<Condition>& conds);
	void getBSONObjectFromObject(const string& clasName, void* object, bson_t*, const bool& isIdBsonAppend= true);
	string initializeQueryParts(Query& cquery, bson_t** fields, bson_t** querySpec, string& operationName);
	string initializeDMLQueryParts(Query& cquery, bson_t** data, bson_t** query, string& operationName);
	string getQueryForRelationship(const string& column, const string& type, void* val);
	void getMapOfProperties(bson_t* data, map<string, GenericObject>* map);
	void* getObject(bson_t* data, uint8_t* buf, uint32_t len, const string& clasName);
	void storeProperty(const ClassInfo& clas, void* t, void* colV, const Field& fe);
	void* getResults(const string& tableNm, Query& cquery, bson_t* fields, bson_t* querySpec, const bool& isObj, const bool& isCountQuery);
	void* getResults(const string& tableNm, QueryBuilder& qb, bson_t* fields, bson_t* query, const bool& isObj);
	Connection* _conn();
	mongoc_collection_t* _collection(Connection*, const char*);
	void _release(Connection*, mongoc_collection_t*);
public:
	MongoDBDataSourceImpl(ConnectionPooler* pool, Mapping* mapping);
	~MongoDBDataSourceImpl();
	bool startTransaction();
	bool commit();
	bool rollback();
	void procedureCall(const string&);
	void empty(const string& clasName);
	long getNumRows(const string& clasName);
	bool executeUpdate(Query& query);
	vector<map<string, GenericObject> > execute(QueryBuilder& qb);
	vector<map<string, GenericObject> > execute(Query& query);
protected:
	bool executeInsert(Query& query, void* entity);
	bool isGetDbEntityForBulkInsert();
	void* getDbEntityForBulkInsert(void* entity, const string& clasName, string& error);
	bool executeInsertBulk(Query& query, vector<void*> entities, vector<void*> dbEntities);
	bool executeUpdateBulk(Query& query, vector<void*> entities, vector<void*> dbEntities);
	bool executeUpdate(Query& query, void* entity);
	bool remove(const string& clasName, GenericObject& id);

	void* executeQuery(Query& query, const bool& isObj);
	void* executeQuery(QueryBuilder& qb, const bool& isObj);

	void executePreTable(DataSourceEntityMapping& dsemp, GenericObject& idv);
	void executePostTable(DataSourceEntityMapping& dsemp, GenericObject& idv);
	void executeSequence(DataSourceEntityMapping& dsemp, GenericObject& idv);
	void executeIdentity(DataSourceEntityMapping& dsemp, GenericObject& idv);
	void executeCustom(DataSourceEntityMapping& dsemp, const string& customMethod, GenericObject& idv);

	void* getContext(void* details);
	void destroyContext(void*);
};

#endif /* MONGODBDATASOURCEIMPL_H_ */

