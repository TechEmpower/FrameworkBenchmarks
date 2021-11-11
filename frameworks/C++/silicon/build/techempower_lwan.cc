#include <silicon/backends/lwan.hh>

#include "techempower.hh"

using namespace s;
using namespace sl;

int main(int argc, char* argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " mysql_host port" << std::endl;
    return 1;
  }

  auto techempower_middlewares = middleware_factories(
    mysql_connection_factory(argv[1], "benchmarkdbuser", "benchmarkdbpass", "hello_world"),
    fortune_orm_factory("Fortune"),
    rn_orm_factory("World")
    );
  
  try
  {

    // Write the pid.
    std::ofstream pidfile(argv[3]);
    pidfile << getpid() << std::endl;
    pidfile.close();

    // Start the server.
    sl::lwan_json_serve(techempower_api, techempower_middlewares, atoi(argv[2]));
  }
  catch (std::exception& e)
  {
    std::cerr << e.what() << std::endl;
  }
  catch (sl::error::error& e)
  {
    std::cerr << e.what() << std::endl;
  }
}
