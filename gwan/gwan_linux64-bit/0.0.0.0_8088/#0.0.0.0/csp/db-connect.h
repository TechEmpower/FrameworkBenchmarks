#define BENCHMARK_DB_HOST "127.0.0.1"
#define MYSQL_MAX_CONNECTIONS 3000

#include <stdexcept>
#include <string>
using namespace std;


// sudo mysql_upgrade -u root
// cat ../config/create.sql | mysql -u root
#ifdef _mysql_h
class DbClient {
  protected:
    MYSQL* conn;
  public:
    DbClient() {
      connect();
    }
    void connect() {
      conn=mysql_init(NULL);
      if(!conn) throw bad_alloc();
      if (mysql_real_connect(conn, BENCHMARK_DB_HOST, "benchmarkdbuser", "benchmarkdbpass", "hello_world", 0, 0, 0) == 0) {
        string s(mysql_error(conn));
        mysql_close(conn);
        throw runtime_error(s);
      }
      mysql_set_character_set(conn, "utf8");
      mysql_options(conn, MYSQL_SET_CHARSET_NAME, "utf8");
    }
    void disconnect() {
      if(!conn) return;
      mysql_close(conn);
      conn = 0;
    }
    void reconnect() {
      disconnect();
      connect();
    }
    virtual ~DbClient() {
      disconnect();
    }
    MYSQL* connection() {
      return conn;
    }
};
#endif

// cat ../config/create-postgres-database.sql | psql -U postgres
// cat ../config/ create-postgres.sql | psql -U postgres
#ifdef LIBPQ_FE_H
class DbClient {
  protected:
    PGconn* conn;
  public:
    DbClient() {
      connect();
    }
    void connect() {
      const char* conn_str="host='" BENCHMARK_DB_HOST "' user='benchmarkdbuser' password='benchmarkdbpass' dbname='hello_world' sslmode='allow'";
      conn = PQconnectdb(conn_str);
      if(!conn) throw bad_alloc();
      if (PQstatus(conn) != CONNECTION_OK) {
        string err=PQerrorMessage(conn);
        PQfinish(conn);
        throw runtime_error(err);
      }
    }
    void disconnect() {
      if(!conn) return;
      PQfinish(conn);
      conn = 0;
    }
    void reconnect() {
      disconnect();
      connect();
    }
    virtual ~DbClient() {
      disconnect();
    }
    PGconn* connection() {
      return conn;
    }
};
#endif
