#define BENCHMARK_DB_HOST "localhost"

#include <stdexcept>
using namespace std;

MYSQL* doConnect(void*) {
	printf("doConnect()\n");
	MYSQL* tmp=mysql_init(NULL);
	if(tmp==NULL) throw bad_alloc();
	if (mysql_real_connect(tmp, BENCHMARK_DB_HOST, 
					"benchmarkdbuser", "benchmarkdbpass", "hello_world", 0, NULL, 0) == NULL) {
		string s(mysql_error(tmp));
		mysql_close(tmp);
		throw runtime_error(s);
	}
	return tmp;
}
void doDisconnect(void*,MYSQL* conn) {
	mysql_close(conn);
}
