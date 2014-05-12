// Test type 7: Actual insert to database (MySQL version)
// http://localhost:8088/?my_insert.cpp
#pragma link "mysqlclient"
#pragma link "json"
#include "gwan.h"
#include "gwan-helper.h"
#include <mysql/mysql.h>
#include "db-connect-my.h"
#include "json-obj.h"
#include <cstring>
#include <cstdlib>

class Benchmark {
  protected:
    Http res;

    DbClient db;

    int id, randomNumber;
    long unsigned int len1;

  public:
    Benchmark(int argc, char** argv) : res(argc,argv) {      
      // USE hello_world;
      // CREATE TABLE Test (id INTEGER primary key auto_increment);
      // TRUNCATE TABLE Test;      
    }
    int exec() {

      // insert everytime there are request
      static const char* sqli="INSERT INTO Test() VALUES()";
      int fail = mysql_query(db.connection(),sqli);
      if(fail) throw runtime_error(mysql_error(db.connection()));

      MYSQL_RES *result = mysql_store_result(db.connection());
      if(result) throw runtime_error(mysql_error(db.connection()));

      id = mysql_insert_id(db.connection());
 
      mysql_free_result(result);
      
      // get last inserted (possible from another thread/process)
      static const char* sql = "SELECT id FROM Test ORDER BY id DESC LIMIT 0,1";
      fail = mysql_query(db.connection(),sql);
      if(fail) throw runtime_error(mysql_error(db.connection()));

      result = mysql_store_result(db.connection());
      if(!result) throw runtime_error(mysql_error(db.connection()));

      MYSQL_ROW row = mysql_fetch_row(result);
      randomNumber = atoi(row[0]);
 
      mysql_free_result(result);      

      // output json
      JsonObj obj;
      obj.add("id",id);
      obj.add("randomNumber",randomNumber);
      res << obj.to_s();

      return res.end();
    }
    virtual ~Benchmark() {

    }
};



int main(int argc,char*argv[]) {
  try {
    Benchmark b(argc,argv);
    return b.exec();
  } catch(const runtime_error &e) { 
    cerr << e.what() << endl;
    return 500;
  } catch(...) {
    return 502;
  }
}
