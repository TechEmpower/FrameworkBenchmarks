/*
 * QueryBuilder.h
 *
 *  Created on: 11-May-2014
 *      Author: sumeetc
 */

#ifndef QUERYBUILDER_H_
#define QUERYBUILDER_H_
#include "Query.h"

class QueryBuilder;

class QueryClause;

class QueryClause {
	int val;
	static void init()
	{
		if(NONE.val!=1)
		{
			NONE.val = 1;
			LOGICAL_GRP_OPEN.val = 2;
			LOGICAL_GRP_CLOSE.val = 3;
			AND.val = 4;
			OR.val = 5;
		}
	}
	QueryClause()
	{
		init();
	}
	friend class Condition;
	friend class LogicalGroup;
	friend class QueryBuilder;
	friend class GroupClause;
	friend class JoinClause;
public:
	int get() const
	{
		return val;
	}
	static QueryClause NONE,
		LOGICAL_GRP_OPEN,
		LOGICAL_GRP_CLOSE,
		AND,
		OR;
	bool operator==(const QueryClause& other) const
	{
		init();
		if(this->val == other.val)
		{
			return true;
		}
		return false;
	}
	bool operator!=(const QueryClause& other) const
	{
		init();
		if(this->val != other.val)
		{
			return true;
		}
		return false;
	}
};

class QueryOperator;

class QueryOperator {
	int val;
	static void init()
	{
		if(NONE.val!=1)
		{
			NONE.val = 1;
			EQUALS.val = 2;
			NOT_EQUALS.val = 3;
			IN.val = 4;
			NOT_IN.val = 5;
			BETWEEN.val = 6;
			GREATER_THAN.val = 7;
			LESS_THAN.val = 8;
			GREATER_THAN_EQUALS.val = 9;
			LESS_THAN_EQUALS.val = 10;
		}
	}
	QueryOperator()
	{
		init();
	}
	friend class Condition;
	friend class LogicalGroup;
	friend class QueryBuilder;
	friend class GroupClause;
	friend class JoinClause;
public:
	int get() const
	{
		return val;
	}
	static QueryOperator NONE,
		EQUALS,
		NOT_EQUALS,
		IN,
		NOT_IN,
		BETWEEN,
		GREATER_THAN,
		LESS_THAN,
		GREATER_THAN_EQUALS,
		LESS_THAN_EQUALS;
	bool operator==(const QueryOperator& other) const
	{
		init();
		if(this->val == other.val)
		{
			return true;
		}
		return false;
	}
	bool operator!=(const QueryOperator& other) const
	{
		init();
		if(this->val != other.val)
		{
			return true;
		}
		return false;
	}
};


class JoinType;

class JoinType {
	int val;
	static void init()
	{
		if(INNER.val!=1)
		{
			INNER.val = 1;
			LEFT.val = 2;
			RIGHT.val = 3;
			FULL.val = 4;
			NATURAL.val = 5;
			CROSS.val = 6;
			LEFT_OUTER.val = 7;
			RIGHT_OUTER.val = 8;
			FULL_OUTER.val = 9;
		}
	}
	JoinType()
	{
		init();
	}
	friend class Condition;
	friend class LogicalGroup;
	friend class QueryBuilder;
	friend class GroupClause;
	friend class JoinClause;
public:
	int get() const
	{
		return val;
	}
	static JoinType INNER,
		LEFT,
		RIGHT,
		FULL,
		NATURAL,
		CROSS,
		LEFT_OUTER,
		RIGHT_OUTER,
		FULL_OUTER;
	bool operator==(const JoinType& other) const
	{
		init();
		if(this->val == other.val)
		{
			return true;
		}
		return false;
	}
	bool operator!=(const JoinType& other) const
	{
		init();
		if(this->val != other.val)
		{
			return true;
		}
		return false;
	}
};

class Condition {
	string lhs;
	QueryOperator oper;
	vector<GenericObject> rhsVec;
	QueryClause clause;
	bool managed;
	friend class LogicalGroup;
public:
	Condition();
	virtual ~Condition();
	QueryClause getClause() const;
	const string& getLhs() const;
	QueryOperator getOper() const;
	GenericObject& getRhs();
	GenericObject& getRhs(const int& index);
	int getRhsSize();
	bool isManaged() const;
};

class LogicalGroup {
	QueryBuilder* qb;
	vector<Condition> conds;
	QueryClause lastClause;
	bool condStarted;
	friend class QueryBuilder;
	LogicalGroup(QueryBuilder* qb);
public:
	LogicalGroup();
	virtual ~LogicalGroup();
	LogicalGroup& where(const string& lhs, const QueryOperator& oper, const GenericObject& rhs);
	LogicalGroup& where(const string& lhs, const QueryOperator& oper, const GenericObject& rhs, const GenericObject& rhs1);
	LogicalGroup& where(const string& lhs, const QueryOperator& oper, const vector<GenericObject>& rhsList);
	template <class T> LogicalGroup& where(const string& lhs, const QueryOperator& oper, const T& rhs);
	template <class T> LogicalGroup& where(const string& lhs, const QueryOperator& oper, const T& rhs, const T& rhs1);
	template <class T> LogicalGroup& where(const string& lhs, const QueryOperator& oper, const vector<T>& rhsList);
	LogicalGroup& logicalAnd();
	LogicalGroup& logicalOr();
	LogicalGroup& open();
	LogicalGroup& close();
	QueryBuilder& end();
	const vector<Condition>& getConds() const;
};

class JoinClause {
	QueryBuilder* qb;
	string tableName;
	string className;
	string alias;
	JoinType type;
	LogicalGroup condition;
	friend class QueryBuilder;
	JoinClause(QueryBuilder* qb);
public:
	JoinClause();
	virtual ~JoinClause();
	LogicalGroup& on(const string& lhs, const QueryOperator& oper, const GenericObject& rhs);
	QueryBuilder& end();
	const string& getAlias() const;
	const string& getClassName() const;
	const LogicalGroup& getCondition() const;
	const string& getTableName() const;
	JoinType getType() const;
};

class GroupClause {
	QueryBuilder* qb;
	vector<string> columns;
	LogicalGroup conditions;
	friend class QueryBuilder;
	GroupClause(QueryBuilder* qb);
public:
	GroupClause();
	virtual ~GroupClause();
	LogicalGroup& having();
	QueryBuilder& end();
	const vector<string>& getColumns() const;
	const LogicalGroup& getConditions() const;
};

class QueryBuilder {
	friend class Query;
	string tableName;
	string className;
	string alias;
	map<string, string> columns;
	bool allCols;
	bool unique;
	int start;
	int count;
	vector<JoinClause*> joinClauses;
	LogicalGroup conditions;
	vector<string> columnsAsc;
	vector<string> columnsDesc;
	vector<QueryBuilder> unions;
	vector<QueryBuilder> unionAlls;
	GroupClause group;
public:
	QueryBuilder();
	virtual ~QueryBuilder();

	QueryBuilder& allColumns();
	QueryBuilder& columnName(string name, string calias= "");
	QueryBuilder& columnNames(const map<string, string>& columns);
	QueryBuilder& delimitedColumnNames(const string& names);
	QueryBuilder& distinct();

	QueryBuilder& fromTable(const string& name, const string& alias);
	QueryBuilder& fromClass(const string& name, const string& alias);

	LogicalGroup& condition();

	GroupClause& groupBy(const string& column);
	GroupClause& groupBy(const vector<string>& columns);
	GroupClause& delimitedGroupByColumns(const string& name);

	QueryBuilder& orderByAsc(const string& column);
	QueryBuilder& orderByDesc(const string& column);

	QueryBuilder& paginate(const int&, const int&);

	JoinClause& joinTable(const JoinType& jt, const string& name, const string& alias);
	JoinClause& joinClass(const JoinType& jt, const string& name, const string& alias);

	QueryBuilder& unionQuery(const QueryBuilder& qb);
	QueryBuilder& unionAllQuery(const QueryBuilder& qb);
	const string& getAlias() const;
	bool isAllCols() const;
	const string& getClassName() const;
	const map<string, string>& getColumns() const;
	const vector<string>& getColumnsAsc() const;
	const vector<string>& getColumnsDesc() const;
	const LogicalGroup& getConditions() const;
	int getCount() const;
	const GroupClause& getGroup() const;
	const vector<JoinClause*>& getJoinClauses() const;
	int getStart() const;
	const string& getTableName() const;
	const vector<QueryBuilder>& getUnionAlls() const;
	const vector<QueryBuilder>& getUnions() const;
	bool isUnique() const;
};

template<class T>
inline LogicalGroup& LogicalGroup::where(const string& lhs, const QueryOperator& oper, const T& rhs) {
	Condition cond;
	cond.lhs = lhs;
	cond.oper = oper;
	cond.managed = true;
	GenericObject o;
	o << rhs;
	cond.rhsVec.push_back(o);
	cond.clause = QueryClause::NONE;
	if(condStarted && (lastClause!=QueryClause::AND && lastClause!=QueryClause::OR)) {
		logicalAnd();
	}
	condStarted = true;
	conds.push_back(cond);
	return *this;
}

template<class T>
inline LogicalGroup& LogicalGroup::where(const string& lhs, const QueryOperator& oper, const T& rhs, const T& rhs1) {
	Condition cond;
	cond.lhs = lhs;
	cond.oper = oper;
	cond.managed = true;
	T* t = new T;
	*t = rhs;
	GenericObject o;
	o << t;
	cond.rhsVec.push_back(o);
	t = new T;
	*t = rhs1;
	o << t;
	cond.rhsVec.push_back(o);
	cond.clause = QueryClause::NONE;
	if(condStarted && (lastClause!=QueryClause::AND && lastClause!=QueryClause::OR)) {
		logicalAnd();
	}
	condStarted = true;
	conds.push_back(cond);
	return *this;
}

template<class T>
inline LogicalGroup& LogicalGroup::where(const string& lhs, const QueryOperator& oper, const vector<T>& rhsList) {
	Condition cond;
	cond.lhs = lhs;
	cond.oper = oper;
	cond.managed = true;
	for (int var = 0; var < (int)rhsList.size(); ++var) {
		T* t = new T;
		*t = rhsList.at(var);
		GenericObject o;
		o << t;
		cond.rhsVec.push_back(o);
	}
	cond.clause = QueryClause::NONE;
	if(condStarted && (lastClause!=QueryClause::AND && lastClause!=QueryClause::OR)) {
		logicalAnd();
	}
	condStarted = true;
	conds.push_back(cond);
	return *this;
}

#endif /* QUERYBUILDER_H_ */
