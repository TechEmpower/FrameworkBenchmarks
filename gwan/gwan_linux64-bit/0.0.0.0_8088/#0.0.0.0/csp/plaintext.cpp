// Test type 6: Plaintext
// http://localhost:8088/?plaintext.cpp
#include "gwan.h"
#include "gwan-helper.h"

class Benchmark {
  protected:
    Http res;
  public:
    Benchmark(int argc,char** argv) : res(argc,argv) {
      res.set_content_type("text/plain");
    }
    int exec() {
      res << "Hello, World!";
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
