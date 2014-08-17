#define BENCHMARK_DB_HOST "localhost"
#define MYSQL_MAX_CONNECTIONS 3000

#include <stdexcept>
using namespace std;

#ifdef _mysql_h

MYSQL* doConnect(void*) {
	//printf("doConnect()\n");
	MYSQL* tmp=mysql_init(NULL);
	if(tmp==NULL) throw bad_alloc();
	if (mysql_real_connect(tmp, BENCHMARK_DB_HOST, 
					"benchmarkdbuser", "benchmarkdbpass", "hello_world", 0, NULL, 0) == NULL) {
		string s(mysql_error(tmp));
		mysql_close(tmp);
		throw runtime_error(s);
	}
	mysql_set_character_set(tmp, "utf8");
	mysql_options(tmp, MYSQL_SET_CHARSET_NAME, "utf8");
	return tmp;
}
void doDisconnect(void*,MYSQL* conn) {
	mysql_close(conn);
}
#endif
#ifdef LIBPQ_FE_H
PGconn* doConnect_pg(void*) {
	//printf("doConnect_pg()\n");
	const char* connString="host='" BENCHMARK_DB_HOST 
		"' user='benchmarkdbuser' password='benchmarkdbpass' dbname='hello_world' sslmode='allow'";
	PGconn* conn;
	conn = PQconnectdb(connString);
	if(conn==NULL) throw bad_alloc();
	if (PQstatus(conn) != CONNECTION_OK)
	{
		string err=PQerrorMessage(conn);
		PQfinish(conn);
		throw runtime_error(err);
	}
	return conn;
}
void doDisconnect_pg(void*,PGconn* conn) {
	PQfinish(conn);
}
#endif
