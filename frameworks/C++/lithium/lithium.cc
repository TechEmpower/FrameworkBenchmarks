#include "lithium_http_backend.hh"

#if TFB_MYSQL
  #include "lithium_mysql.hh"
#elif TFB_PGSQL
  #include "lithium_pgsql.hh"
#endif

#include "symbols.hh"
using namespace li;

template <typename B>
void escape_html_entities(B& buffer, const std::string& data)
{
    for(size_t pos = 0; pos != data.size(); ++pos) {
        switch(data[pos]) {
            case '&':  buffer << "&amp;";       break;
            case '\"': buffer << "&quot;";      break;
            case '\'': buffer << "&apos;";      break;
            case '<':  buffer << "&lt;";        break;
            case '>':  buffer << "&gt;";        break;
            default:   buffer << data[pos]; break;
        }
    }
}

void set_max_sql_connections_per_thread(int max)
{
#if TFB_MYSQL
  li::max_mysql_connections_per_thread = max;
#elif TFB_PGSQL
  li::max_pgsql_connections_per_thread = max;
#endif
}

float tune_n_sql_connections(std::string http_req, int port, int max, int nprocs) {

  std::cout << std::endl << "Benchmark " << http_req << std::endl;

  // Warmup;
  // set_max_sql_connections_per_thread(1);
  // http_benchmark(256, 1, 1000, port, http_req);

  // auto my_api = make_api();
  float max_req_per_s = 0;
  int best_nconn = 2;
  for (int nc : {1, 2, 3, 4, 8, 16, 30, 40, 50, 60})
  {
    if (nc*nprocs >= max) break;
    set_max_sql_connections_per_thread(nc);
    float req_per_s = http_benchmark(256, 1, 600, port, http_req);
    std::cout << nc << " -> " << req_per_s << " req/s." << std::endl;
    if (req_per_s > max_req_per_s)
    {
      max_req_per_s = req_per_s;
      best_nconn = nc;
    }
  }
  std::cout << "best: " << best_nconn << " (" << max_req_per_s << " req/s)."<< std::endl;

  return best_nconn;
}


int main(int argc, char* argv[]) {

  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " sql_host port" << std::endl;
    return 1;
  }

  int port = atoi(argv[2]);
  int nprocs = std::thread::hardware_concurrency();

#if TFB_MYSQL
  auto sql_db =
    mysql_database(s::host = argv[1], s::database = "hello_world", s::user = "benchmarkdbuser",
                   s::password = "benchmarkdbpass", s::port = 3306, s::charset = "utf8");
  int sql_max_connection = sql_db.connect()("SELECT @@GLOBAL.max_connections;").template read<int>() - 10;
#elif TFB_PGSQL
  auto sql_db =
    pgsql_database(s::host = argv[1], s::database = "hello_world", s::user = "benchmarkdbuser",
                   s::password = "benchmarkdbpass", s::port = 5432, s::charset = "utf8");
  int sql_max_connection = atoi(sql_db.connect()("SHOW max_connections;").template read<std::string>().c_str()) - 10;
#endif

  auto fortunes = sql_orm_schema(sql_db, "Fortune").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::message = std::string());

  auto random_numbers = sql_orm_schema(sql_db, "World").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::randomNumber = int());


  int db_nconn = 64;
  int queries_nconn = 64;
  int fortunes_nconn = 64;
  int updates_nconn = 64;

  http_api my_api;

  my_api.get("/plaintext") = [&](http_request& request, http_response& response) {
    response.set_header("Content-Type", "text/plain");
    response.write("Hello, World!");
  };

  my_api.get("/json") = [&](http_request& request, http_response& response) {
    response.write_json(s::message = "Hello, World!");
  };
  my_api.get("/db") = [&](http_request& request, http_response& response) {
    set_max_sql_connections_per_thread(db_nconn);
    response.write_json(random_numbers.connect(request.fiber).find_one(s::id = 1234).value());
  };

  my_api.get("/queries") = [&](http_request& request, http_response& response) {
    set_max_sql_connections_per_thread(queries_nconn);
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());
    
    N = std::max(1, std::min(N, 500));
    
    auto c = random_numbers.connect(request.fiber);
    std::vector<decltype(random_numbers.all_fields())> numbers(N);
    for (int i = 0; i < N; i++)
      numbers[i] = c.find_one(s::id = 1 + rand() % 10000).value();

    response.write_json(numbers);
  };

  my_api.get("/updates") = [&](http_request& request, http_response& response) {
    set_max_sql_connections_per_thread(updates_nconn);
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());
    N = std::max(1, std::min(N, 500));
    std::vector<decltype(random_numbers.all_fields())> numbers(N);

    {     
      auto c = random_numbers.connect(request.fiber);
      auto& raw_c = c.backend_connection();

      
#if TFB_MYSQL
      raw_c("START TRANSACTION");
#endif
      for (int i = 0; i < N; i++)
      {
        numbers[i] = c.find_one(s::id = 1 + rand() % 10000).value();
        numbers[i].randomNumber = 1 + rand() % 10000;
      }

      std::sort(numbers.begin(), numbers.end(), [] (auto a, auto b) { return a.id < b.id; });

#if TFB_MYSQL
      for (int i = 0; i < N; i++)
        c.update(numbers[i]);
      raw_c("COMMIT");
#elif TFB_PGSQL
      raw_c.cached_statement
        ([N] {
          std::ostringstream ss;
          ss << "UPDATE World SET randomNumber=tmp.randomNumber FROM (VALUES ";
          for (int i = 0; i < N; i++)
            ss << "($" << i*2+1 << "::integer, $" << i*2+2 << "::integer) "<< (i == N-1 ? "": ",");
          ss << ") AS tmp(id, randomNumber) WHERE tmp.id = World.id";
          return ss.str();
        }, N)(numbers);
      
#endif
    }

    response.write_json(numbers);
  };

  my_api.get("/fortunes") = [&](http_request& request, http_response& response) {
    set_max_sql_connections_per_thread(fortunes_nconn);

    typedef decltype(fortunes.all_fields()) fortune;
    std::vector<fortune> table;

    auto c = fortunes.connect(request.fiber);
    c.forall([&] (auto f) { table.emplace_back(f); });
    table.emplace_back(0, "Additional fortune added at request time.");

    std::sort(table.begin(), table.end(),
              [] (const fortune& a, const fortune& b) { return a.message < b.message; });

    char b[100000];
    li::output_buffer ss(b, sizeof(b));
 
    ss << "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
    for(auto& f : table)
    {
      ss << "<tr><td>" << f.id << "</td><td>";
      escape_html_entities(ss, f.message); 
      ss << "</td></tr>";
    }
    ss << "</table></body></html>";

    response.set_header("Content-Type", "text/html; charset=utf-8");
    response.write(ss.to_string_view());
  };

  // Tune the number of sql connections.
  int tunning_port = port+1;
  std::thread server_thread([&] {
    http_serve(my_api, tunning_port, s::nthreads = nprocs);
  });
  usleep(3e5);

  db_nconn = tune_n_sql_connections("GET /db HTTP/1.1\r\n\r\n", tunning_port, sql_max_connection, nprocs);
  queries_nconn = tune_n_sql_connections("GET /queries?N=20 HTTP/1.1\r\n\r\n", tunning_port, sql_max_connection, nprocs);
  fortunes_nconn = tune_n_sql_connections("GET /fortunes HTTP/1.1\r\n\r\n", tunning_port, sql_max_connection, nprocs);
  updates_nconn = tune_n_sql_connections("GET /updates?N=20 HTTP/1.1\r\n\r\n", tunning_port, sql_max_connection, nprocs);

// #if TFB_MYSQL
//   updates_nconn = 1;
// #elif TFB_PGSQL
//   updates_nconn = 3;
// #endif
  
  li::quit_signal_catched = true;
  server_thread.join();
  li::quit_signal_catched = false;

  // Start the server for the Techempower benchmark.
  http_serve(my_api, port, s::nthreads = nprocs);

  return 0;
}
