/*
 * DataSourceMapping.h
 *
 *  Created on: 18-May-2014
 *      Author: sumeetc
 */

#ifndef DATASOURCEMAPPING_H_
#define DATASOURCEMAPPING_H_
#include "map"
#include "StringUtil.h"

using namespace std;

typedef map<string, string> strMap;
typedef map<string, strMap> smstrMap;

class DataSourceInternalRelation
{
	int type;//1->1-1 2->1-N 3->N-N
	string dfk;
	string dmappedBy;
	string clsName;
	string field;
	friend class ConfigurationHandler;
public:
	const string& getClsName() const;
	void setClsName(const string& clsName);
	const string& getField() const;
	void setField(const string& field);
	const string& getDfk() const;
	void setDfk(const string& dfk);
	const string& getDmappedBy() const;
	void setDmappedBy(const string& dmappedBy);
	int getType() const;
	void setType(const int& type);
};

class DataSourceEntityMapping {
	string className;
	string tableName;
	bool embedded;
	bool idGenerate;
	string idgendbEntityType;
	string idgendbEntityName;
	string idgentype;
	string idgenhiValueColumn;
	string idgenentityColumn;
	string idgencolumnName;
	int idgenlowValue;
	string idPropertyName;
	strMap propertyColumnMapping;
	strMap columnPropertyMapping;
	vector<DataSourceInternalRelation> relations;
	friend class Mapping;
	friend class ConfigurationHandler;
public:
	DataSourceEntityMapping();
	~DataSourceEntityMapping();
	const string& getClassName() const;
	void setClassName(const string& className);
	const string& getIdPropertyName() const;
	void setIdPropertyName(const string& idColumnName);
	bool isEmbedded() const;
	void setIsEmbedded(const bool& isEmbedded);
	const string& getTableName() const;
	void setTableName(const string& tableName);
	void addPropertyColumnMapping(const string& property, const string& column);
	void addRelation(const DataSourceInternalRelation& relation);
	string getColumnForProperty(const string& property);
	string getPropertyForColumn(const string& column);
	vector<DataSourceInternalRelation> getRelations();
	const strMap& getColumnPropertyMapping() const;
	const strMap& getPropertyColumnMapping() const;
	const string& getIdgencolumnName() const;
	const string& getIdgendbEntityName() const;
	const string& getIdgendbEntityType() const;
	const string& getIdgenentityColumn() const;
	const string& getIdgenhiValueColumn() const;
	const string& getIdgentype() const;
	bool isIdGenerate() const;
	int getIdgenlowValue() const;
	void setEmbedded(const bool& embedded);
	void setIdgencolumnName(const string& idgencolumnName);
	void setIdgendbEntityName(const string& idgendbEntityName);
	void setIdgendbEntityType(const string& idgendbEntityType);
	void setIdgenentityColumn(const string& idgenentityColumn);
	void setIdGenerate(const bool& idGenerate);
	void setIdgenhiValueColumn(const string& idgenhiValueColumn);
	void setIdgentype(const string& idgentype);
	void setIdgenlowValue(const int& idgenlowValue);
};

class Mapping
{
	DataSourceEntityMapping __c;
	string appName;
	strMap tableClassMapping;
	map<string, DataSourceEntityMapping> dseMap;
	friend class ConfigurationHandler;
public:
	void addDataSourceEntityMapping(const DataSourceEntityMapping& dsemp);
	DataSourceEntityMapping getDataSourceEntityMapping(const string& clas);
    string getPropertyForColumn(const string& tableName, const string& columnName);
    strMap getMappingForTable(const string& tableName);
    string getTableForClass(const string& claz);
    string getClassForTable(const string& table);
	const string& getAppName() const;
	void setAppName(const string& appName);
	map<string, DataSourceEntityMapping>& getDseMap();
};

#endif /* DATASOURCEMAPPING_H_ */
