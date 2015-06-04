// Test type 2: Single database query (MySQL version)
// http://localhost:8088/?my.cpp
#pragma link "mysqlclient"
#pragma link "json"
#include "gwan.h"
#include "gwan-helper.h"
#include <mysql/mysql.h>
#include "db-connect.h"
#include "json-obj.h"
#include <cstring>
#include <cstdlib>

class Benchmark {
  protected:
    Http res;

    DbClient db;

    int id, randomNumber;
    long unsigned int len1;
    MYSQL_STMT* stmt;
    MYSQL_BIND param[1], results[1];

  public:
    Benchmark(int argc, char** argv) : res(argc,argv) {
      // create prepared statement
      stmt = mysql_stmt_init(db.connection());
      static const char* sql="SELECT randomNumber FROM World WHERE id = ?";
      mysql_stmt_prepare(stmt,sql,strlen(sql));
    }
    int exec() {

      id = rand()%10000;

      // reset statement for reuse
      if(mysql_stmt_reset(stmt)) throw runtime_error(mysql_stmt_error(stmt));

      // create and use input parameter
      memset(param, 0, sizeof(param));
      param[0].buffer_type = MYSQL_TYPE_LONG;
      param[0].buffer = (char *)&id;
      param[0].buffer_length = sizeof(id);
      param[0].is_null = 0;
      param[0].length = NULL;
      if(mysql_stmt_bind_param(stmt,param)) throw runtime_error(mysql_stmt_error(stmt));

      // create and fill result buffer
      memset(results, 0, sizeof(results));
      results[0].buffer_type= MYSQL_TYPE_LONG;
      results[0].buffer = &randomNumber;
      results[0].buffer_length = sizeof(randomNumber);
      results[0].is_null = 0;
      results[0].length = &len1;
      if(mysql_stmt_bind_result(stmt, results)) throw runtime_error(mysql_stmt_error(stmt));

      // execute statement
      if(mysql_stmt_execute(stmt)) throw runtime_error(mysql_stmt_error(stmt));
      if(mysql_stmt_fetch(stmt)) randomNumber = 0;

      // output json
      JsonObj obj;
      obj.add("id",id);
      obj.add("randomNumber",randomNumber);
      res << obj.to_s();

      return res.end();
    }
    virtual ~Benchmark() {
      mysql_stmt_close(stmt);
    }
};



int main(int argc,char*argv[]) {
  try {
    Benchmark b(argc,argv);
    return b.exec();
  } catch(...) {
    return 500;
  }
}
