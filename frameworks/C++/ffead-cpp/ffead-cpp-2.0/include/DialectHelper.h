/*
	Copyright 2010, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (const the& "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * DialectHelper.h
 *
 *  Created on: 25-Apr-2013
 *      Author: sumeetc
 */

#ifndef DIALECTHELPER_H_
#define DIALECTHELPER_H_
#include "string"
#include "map"
#include "TemplateEngine.h"
#include "CastUtil.h"
#include "DataSourceMapping.h"
using namespace std;

class DialectHelper {
	static bool init;
	static map<string, map<int, string> > dialectStrMap;
	static void loadDialectSQLStrings();
	static void loadOracleDialectStrings();
	static void loadMySQLDialectStrings();
	static void loadPostgresDialectStrings();
	static void loadSQLServerDialectStrings();
	static void loadDB22DialectStrings();
	static void getPaginationSQL(const string& dialect, string& query, const StringContext& params);
	DialectHelper();
public:
	static string ORACLE_DIALECT, MYSQLMYISAM_DIALECT, MYSQLINNODB_DIALECT, POSTGRES_DIALECT,
				  SQLSERVER_DIALECT, SQLSERVER12_DIALECT, TIMESTEN_DIALECT,
				  DB2_DIALECT, SQLLITE_DIALECT, ANY_DIALECT;
	static int PAGINATION_OFFSET_SQL, PAGINATION_NO_OFFSET_SQL, VALIDDB_FUNCTIONS, IDGEN_SEQUENCE_QUERY,
				IDGEN_TABLESEL_QUERY, IDGEN_TABLEUPD_QUERY, IS_TRANSACTION_SUPPORTED, IDGEN_IDENTITY_QUERY,
				IDGEN_TABLESELNOLOCK_QUERY, IDGEN_TABLESELNOLOCKMHL_QUERY, IDGEN_TABLESELMHL_QUERY,
				IDGEN_TABLEUPDMHL_QUERY, BULK_INSERT_QUERY, BULK_UPDATE_QUERY;
	static string getSQLString(const string& dialect, const int& type, string query, StringContext params);
	static string getSQLString(const string& dialect, const int& type, StringContext params);
	static string getProperty(const string& dialect, const int& type);
	static bool isTransactionSupported(const string& dialect);
	static string getIdGenerateQueryPre(const string& dialect, const DataSourceEntityMapping& dsemp);
	static string getIdGenerateQueryPost(const string& dialect, const DataSourceEntityMapping& dsemp);
	static void load();
	virtual ~DialectHelper();
};

#endif /* DIALECTHELPER_H_ */
