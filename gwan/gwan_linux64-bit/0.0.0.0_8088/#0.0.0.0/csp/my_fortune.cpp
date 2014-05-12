// Test type 4: Fortunes (MySQL version)
// http://localhost:8088/?my_fortune.cpp
#pragma link "mysqlclient"
#pragma link "json"
#include "gwan.h"
#include "gwan-helper.h"
#include <mysql/mysql.h>
#include "db-connect-my.h"
#include "json-obj.h"
#include <cstring>
#include <cstdlib>
#include <vector>
#include <algorithm>

struct Fortune {
    int id;
    string message;
    Fortune(int id,const string& message) : id(id), message(message) {}
    bool operator<(const Fortune& other) const {
      return message < other.message;
    }
};

typedef vector<Fortune> vf;

class Benchmark {
  protected:
    Http res;

    DbClient db;

    int id, randomNumber;

  public:
    Benchmark(int argc, char** argv) : res(argc,argv) {}

    int exec() {

      vf array;

      // query fortunes
      mysql_query(db.connection(), "SELECT id, message FROM Fortune;");
      MYSQL_RES *sqlres = mysql_store_result(db.connection());
      if (!sqlres) throw runtime_error(mysql_error(db.connection()));
      MYSQL_ROW row;
      while( (row=mysql_fetch_row(sqlres)) ){
        array.push_back(Fortune(atoi(row[0]),row[1]));
      }
      mysql_free_result(sqlres);

      // add on the fly
      array.push_back(Fortune(0,"Additional fortune added at request time."));

      // sort
      sort(array.begin(),array.end());

      // output html
      res << "<!DOCTYPE html>"
          "<html>"
          "<head><title>Fortunes</title></head>"
          "<body>"
          "<table>"
          "<tr><th>id</th><th>message</th></tr>";
      for(unsigned z=0;z<array.size();++z) {
        Fortune &az = array[z];
        res << "<tr>"
               "<td>" << az.id << "</td>"
               "<td>" << xml::escape(az.message) << "</td>"
               "</tr>";
      }
      res << "</table>"
             "</body>"
             "</html>";

      return res.end();
    }
    virtual ~Benchmark() {}
};



int main(int argc,char*argv[]) {
  try {
    Benchmark b(argc,argv);
    return b.exec();
  } catch(...) {
    return 500;
  }
}
