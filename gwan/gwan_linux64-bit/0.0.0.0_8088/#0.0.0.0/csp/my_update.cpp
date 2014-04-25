// Test type 5: Database updates (MySQL version)
// http://localhost:8088/?my_update.cpp&queries=250
#pragma link "mysqlclient"
#pragma link "json"
#include "gwan.h"
#include "gwan-helper.h"
#include <mysql/mysql.h>
#include "db-connect.h"
#include "json-obj.h"
#include <cstring>
#include <cstdlib>
#include <vector>

struct World {
    int id, randomNumber;
    World(int id=0, int randomNumber=0) : id(id), randomNumber(randomNumber) {}
};

typedef vector<World> vw;

class Benchmark {
  protected:
    Http res;

    DbClient db;

    int id, randomNumber, newNumber;
    long unsigned int len1;
    MYSQL_STMT* stmt, *ustmt;
    MYSQL_BIND param[1], results[1], param_update[2];

  public:
    Benchmark(int argc, char** argv) : res(argc,argv) {
      // response headers
      res.set_content_type("application/json");

      // create prepared statement
      stmt = mysql_stmt_init(db.connection());
      static const char* sql="SELECT randomNumber FROM World WHERE id = ?";
      mysql_stmt_prepare(stmt,sql,strlen(sql));

      ustmt = mysql_stmt_init(db.connection());
      static const char* usql="UPDATE World SET randomNumber = ? WHERE id = ?";
      mysql_stmt_prepare(ustmt,usql,strlen(usql));
    }
    int exec() {

      int n = res.get_i("queries=");
      if(n<1) n = 1;
      if(n>500) n = 500;

      vw array;

      while(n--) {

        id = rand()%10000;
        newNumber = rand()%10000+1;

        // reset statement for reuse
        if(mysql_stmt_reset(stmt)) throw runtime_error(mysql_stmt_error(stmt));
        if(mysql_stmt_reset(ustmt)) throw runtime_error(mysql_stmt_error(ustmt));

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

        // create and use update parameter
        memset(param_update, 0, sizeof(param_update));
        param_update[0].buffer_type = MYSQL_TYPE_LONG;
        param_update[0].buffer = (char *)&newNumber;
        param_update[0].buffer_length = sizeof(newNumber);
        param_update[0].is_null = 0;
        param_update[0].length = 0;
        param_update[1].buffer_type = MYSQL_TYPE_LONG;
        param_update[1].buffer = (char *)&id;
        param_update[1].buffer_length = sizeof(id);
        param_update[1].is_null = 0;
        param_update[1].length = 0;

        // execute statement
        if(mysql_stmt_execute(stmt)) throw runtime_error(mysql_stmt_error(stmt));
        if(mysql_stmt_bind_param(ustmt,param_update)) throw runtime_error(mysql_stmt_error(ustmt));
        if(mysql_stmt_fetch(stmt)) randomNumber = 0;

        // add to array (required by specification)
        array.push_back(World(id,newNumber));
      }

      // output json
      res << '[';
      for(unsigned z=0;z<array.size();++z) {
        if(z>0) res << ',';
        World &az = array[z];
        JsonObj obj;
        obj.add("id",az.id);
        obj.add("randomNumber",az.randomNumber);
        res << obj.to_s();
      }
      res << ']';

      return res.end();
    }
    virtual ~Benchmark() {
      mysql_stmt_close(stmt);
      mysql_stmt_close(ustmt);
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
