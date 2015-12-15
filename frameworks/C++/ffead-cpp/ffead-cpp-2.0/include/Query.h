/*
 * Query.h
 *
 *  Created on: 13-May-2014
 *      Author: sumeetc
 */

#ifndef QUERY_H_
#define QUERY_H_
#include "map"
#include "string"
#include "GenericObject.h"
#include "StringUtil.h"
using namespace std;
typedef map<string, GenericObject> Parameters;
typedef map<int, GenericObject> PosParameters;

class Query {
	/*The column bindings used in the where clause for the entity*/
	Parameters columnBindings;
	/*The property/column names to be queried for the entity*/
	Parameters propNameVaues;
	/*The property/column positions to be queried for the entity*/
	PosParameters propPosVaues;
	/*The actual Query string*/
	string query;
	/*The start and count values required for pagination*/
	int start, count;
	/*The class for criteria building*/
	string className;
	string tableName;
	vector<string> aliasedColumns;
	friend class SQLDataSourceImpl;
public:
	Query();
	Query(const string& query);
	Query(const string& query, const string& className);
	Query addParameter(const string&, const GenericObject&);
	Query addParameters(const Parameters& propNameVaues);
	Query addParameter(const int&, const GenericObject&);
	Query addParameters(const PosParameters& propPosVaues);
	Query addColumnBinding(const string&, const GenericObject&);
	Query addColumnBindings(const Parameters& columnBindings);
	Query paginate(const int&, const int&);
	bool isUpdate();
	virtual ~Query();
	Parameters& getColumnBindings();
	Parameters& getPropNameVaues();
	PosParameters& getPropPosVaues();
	void setQuery(const string& query);
	int getCount() const;
	int getStart() const;
	const string& getClassName() const;
	const string& getQuery() const;
	void setCount(const int& count);
	void setStart(const int& start);
	const string& getTableName() const;
	void setTableName(const string& tableName);
	void setClassName(const string& className);
};

#endif /* QUERY_H_ */
