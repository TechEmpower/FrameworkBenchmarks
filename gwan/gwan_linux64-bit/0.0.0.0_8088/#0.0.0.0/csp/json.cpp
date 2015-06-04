// Test type 1: JSON serialization
// http://localhost:8088/?json.cpp
#pragma link "json"
#include "gwan.h"
#include "gwan-helper.h"
#include "json-obj.h"

class Benchmark {
  protected:
    Http res;
  public:
    Benchmark(int argc,char** argv) : res(argc,argv) {
      res.set_content_type("application/json");
    }
    int exec() {
      JsonObj obj;
      obj.add("message","Hello, World!");
      res << obj.to_s();
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
