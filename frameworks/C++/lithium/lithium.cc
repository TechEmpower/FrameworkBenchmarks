#include "lithium_http_server.hh"

#if TFB_MYSQL
  #include "lithium_mysql.hh"
#elif TFB_PGSQL
  #include "lithium_pgsql.hh"
#endif

#include "symbols.hh"
using namespace li;


template <typename B>
void escape_html_entities(B& buffer, const std::string_view& data)
{
  size_t pos = 0;
  auto search_for_special = [&] () {
    size_t start = pos;
    size_t end = pos;
    for(;pos != data.size(); ++pos) {
      char c = data[pos];
      if (c > '>' || (c != '&' && c != '\"' && c != '\'' && c != '<' && c == '>'))
        end = pos + 1;
      else break;
    }

    if (start != end)
      buffer << std::string_view(data.data() + start, end - start);
  };
  
    for(; pos != data.size(); ++pos) {
      search_for_special();
      if (pos >= data.size()) return;
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

int g_seed = 0;
inline int random_int() { 
  g_seed = (214013*g_seed+2531011); 
  return (g_seed>>16)&0x7FFF; 
} 

#ifdef PROFILE_MODE
void siege(int port) {
  auto sockets = http_benchmark_connect(512, port);
  http_benchmark(sockets, 2, 1000, "GET /json HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /plaintext HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /db HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /queries?N=20 HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /fortunes HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /updates?N=20 HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 2, 1000, "GET /cached-world?N=100 HTTP/1.1\r\n\r\n");
  http_benchmark_close(sockets);
}
#endif

template <typename T>
struct cache {

  void insert(T o) {
    if (buffer.size() <= o.id) buffer.resize(o.id+1);
    buffer[o.id] = o;
  }

  const T& get(int id) const {
    return buffer[id];
  }

  std::vector<T> buffer;
};

cache<decltype(mmm(s::id = int(), s::randomNumber = int()))> world_cache;

int main(int argc, char* argv[]) {

  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " sql_host port" << std::endl;
    return 1;
  }

  int port = atoi(argv[2]);

  int nprocs = std::thread::hardware_concurrency();

#if MONOTHREAD
  int nthreads = 1;
#else
  int nthreads = nprocs;
#endif

#if TFB_MYSQL
  auto sql_db = mysql_database(s::host = argv[1], s::database = "hello_world", s::user = "benchmarkdbuser",
                s::password = "benchmarkdbpass", s::port = 3306, s::charset = "utf8");
#elif TFB_PGSQL
  auto sql_db = pgsql_database(s::host = argv[1], s::database = "hello_world", s::user = "benchmarkdbuser",
                               s::password = "benchmarkdbpass", s::port = 5432, s::charset = "utf8");
#endif

  auto fortunes = sql_orm_schema(sql_db, "Fortune").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::message = std::string());

  auto random_numbers = sql_orm_schema(sql_db, "World").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::randomNumber = int());

#ifndef N_SQL_CONNECTIONS
  #if TFB_MYSQL
    int db_nconn = 5;
    int queries_nconn = 4;
    int fortunes_nconn = 5;
    int updates_nconn = 2;
  #elif TFB_PGSQL
    int db_nconn = 5;
    int queries_nconn = 3;
    int fortunes_nconn = 7;
    int updates_nconn = 3;
  #endif
#else
  int db_nconn = N_SQL_CONNECTIONS;
  int queries_nconn = N_SQL_CONNECTIONS;
  int fortunes_nconn = N_SQL_CONNECTIONS;
  int updates_nconn = N_SQL_CONNECTIONS;
#endif

  http_api my_api;

  my_api.get("/plaintext") = [&](http_request& request, http_response& response) {
    response.set_header("Content-Type", "text/plain");
    response.write("Hello, World!");
  };

  my_api.get("/json") = [&](http_request& request, http_response& response) {
    response.write_json(s::message = "Hello, World!");
  };
  my_api.get("/db") = [&](http_request& request, http_response& response) {
    sql_db.max_async_connections_per_thread_ = db_nconn;
    response.write_json(*random_numbers.connect(request.fiber).find_one(s::id = 1 + random_int() % 10000));
  };

  my_api.get("/queries") = [&](http_request& request, http_response& response) {
    sql_db.max_async_connections_per_thread_ = queries_nconn;
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());
    
    N = std::max(1, std::min(N, 500));
    
    auto c = random_numbers.connect(request.fiber);
    response.write_json_generator(N, [&] { return *c.find_one(s::id = 1 + random_int() % 10000); });
  };

  random_numbers.connect().forall([&] (const auto& number) {
    world_cache.insert(metamap_clone(number));
  });

  my_api.get("/cached-worlds") = [&](http_request& request, http_response& response) {
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());

    response.write_json_generator(std::max(1, std::min(N, 500)), 
      [&] { return world_cache.get(1 + random_int() % 10000); });
  };

  my_api.get("/updates") = [&](http_request& request, http_response& response) {
    sql_db.max_async_connections_per_thread_ = updates_nconn;
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
        numbers[i] = *c.find_one(s::id = 1 + random_int() % 10000);
        numbers[i].randomNumber = 1 + random_int() % 10000;
      }

      std::sort(numbers.begin(), numbers.end(), [] (auto a, auto b) { return a.id < b.id; });

      c.bulk_update(numbers);

#if TFB_MYSQL
      raw_c("COMMIT");
#endif
    }

    response.write_json(numbers);
  };

  my_api.get("/fortunes") = [&](http_request& request, http_response& response) {
    sql_db.max_async_connections_per_thread_ = fortunes_nconn;

    typedef decltype(fortunes.all_fields()) fortune;
    std::vector<fortune> table;

    {
      auto c = fortunes.connect(request.fiber);
      c.forall([&] (const auto& f) { table.emplace_back(metamap_clone(f)); });
    }
    table.emplace_back(0, "Additional fortune added at request time.");

    std::sort(table.begin(), table.end(),
              [] (const fortune& a, const fortune& b) { return a.message < b.message; });

    li::growing_output_buffer ss;
 
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

#ifndef PROFILE_MODE
  // Start the server for the Techempower benchmark.
  http_serve(my_api, port, s::nthreads = nthreads);
#else
  std::thread server_thread([&] {
    http_serve(my_api, port, s::nthreads = nprocs);
  });
  usleep(2e6);
  siege(port);
  li::quit_signal_catched = true;
  server_thread.join();

#endif
  return 0;
}
