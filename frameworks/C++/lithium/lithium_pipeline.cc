#include "lithium_http_backend.hh"

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

#ifdef PROFILE_MODE
void siege(int port) {
  auto sockets = http_benchmark_connect(256, port);
  http_benchmark(sockets, 1, 100, "GET /json HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 1, 100, "GET /plaintext HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 1, 100, "GET /db HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 1, 100, "GET /queries?N=20 HTTP/1.1\r\n\r\n");
  http_benchmark(sockets, 1, 100, "GET /fortunes HTTP/1.1\r\n\r\n");
  // http_benchmark(sockets, 1, 100, "GET /updates?N=20 HTTP/1.1\r\n\r\n");
  http_benchmark_close(sockets);
}
#endif

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
                               s::password = "benchmarkdbpass", s::port = 5432, s::charset = "utf8", s::max_async_connections_per_thread = N_SQL_CONNECTIONS);
#endif

  std::cout << "Using " << sql_db.max_async_connections_per_thread_ << " sql connections x " << nthreads << " threads." << std::endl;
  auto fortunes = sql_orm_schema(sql_db, "Fortune").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::message = std::string());

  auto random_numbers = sql_orm_schema(sql_db, "World").fields(
    s::id(s::auto_increment, s::primary_key) = int(),
    s::randomNumber = int());



  http_api my_api;

  auto select_N_random_numbers = [] (auto& orm, int N) {
    std::vector<decltype(random_numbers.all_fields())> numbers(N);
    std::vector<decltype(orm.find_one(s::id = 1))> results;
    
    for (int i = 0; i < N; i++)
      results.push_back(orm.find_one(s::id = 1 + (i*10000/N) + rand() % (10000/N)));
    for (int i = 0; i < N; i++){
      // println(" read result " , i);
      numbers[i] = results[i]().value();
    }
    return numbers;
  };

  my_api.get("/plaintext") = [&](http_request& request, http_response& response) {
    response.set_header("Content-Type", "text/plain");
    response.write("Hello, World!");
  };

  my_api.get("/json") = [&](http_request& request, http_response& response) {
    response.write_json(s::message = "Hello, World!");
  };
  my_api.get("/db") = [&](http_request& request, http_response& response) {
    response.write_json(random_numbers.connect(request.fiber).find_one(s::id = 1 + rand() % 10000)());
  };

  my_api.get("/queries") = [&](http_request& request, http_response& response) {
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());
    
    N = std::max(1, std::min(N, 500));
    
    auto c = random_numbers.connect(request.fiber);
    response.write_json(select_N_random_numbers(c, N));
  };

  my_api.get("/updates") = [&](http_request& request, http_response& response) {
    std::string N_str = request.get_parameters(s::N = std::optional<std::string>()).N.value_or("1");
    int N = atoi(N_str.c_str());
    N = std::max(1, std::min(N, 500));

    auto c = random_numbers.connect(request.fiber);
    auto numbers = select_N_random_numbers(c, N);
 
    for (int i = 0; i < N; i++)
      numbers[i].randomNumber = 1 + rand() % 10000;

    auto req = c.bulk_update(numbers);
    if (N_SQL_CONNECTIONS * nthreads > 1) c.backend_connection().end_of_batch();
    req.flush_results();
    response.write_json(numbers);
  };

  my_api.get("/fortunes") = [&](http_request& request, http_response& response) {

    typedef decltype(fortunes.all_fields()) fortune;
    std::vector<fortune> table;

    auto c = fortunes.connect(request.fiber);
    c.forall([&] (const auto& f) { table.emplace_back(metamap_clone(f)); });
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
